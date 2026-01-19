use assert_cmd::cargo::CommandCargoExt;
use assert_cmd::prelude::*;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// Writes a small synthetic CSV with clear main and interaction effects.
/// Columns: participant_id, cong, prev_cong, rt
fn write_small_input(tmp: &TempDir) -> PathBuf {
    let p = tmp.path().join("small_input.csv");
    let csv = "\
participant_id,cong,prev_cong,rt
p1,-1,-1,700
p1,-1,1,710
p1,1,-1,670
p1,1,1,676
p2,-1,-1,720
p2,-1,1,728
p2,1,-1,690
p2,1,1,696
";
    fs::write(&p, csv).expect("write input csv");
    p
}

/// R script that:
/// - Reads original CSV
/// - Fits lmer on rt
/// - Reads Rust parquet for null_interaction qmap_5 and derives lrt_adj as the response column
/// - Recomputes R quantile_map_once lrt_adj on original RTs
/// - Fits lmer on R's lrt_adj
/// - Outputs a JSON with coefficients for: intercept, cong, cong:prev_cong for each (original, rust_qmap5, r_qmap5)
fn r_script_contents() -> String {
    let script = r#"#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(lme4)
  library(dplyr)
  library(tibble)
  library(readr)
  library(arrow)
  library(purrr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript compare.R <input_csv> <rust_parquet_dir>")
}
input_csv <- args[[1]]
out_dir <- args[[2]]

# Read original data
df <- read_csv(input_csv, show_col_types = FALSE) %>%
  transmute(
    participant_id = as.character(participant_id),
    cong = as.integer(cong),
    prev_cong = as.integer(prev_cong),
    rt = as.numeric(rt)
  )

# Fit lmer on original RT
mod_orig <- lmer(rt ~ cong * prev_cong + (1 | participant_id), data = df, REML = TRUE)
co_orig <- summary(mod_orig)$coefficients
coefs_orig <- tibble(term = rownames(co_orig),
                     estimate = co_orig[, 'Estimate'],
                     std_error = co_orig[, 'Std. Error']) %>% 
  filter(term %in% c('(Intercept)', 'cong', 'cong:prev_cong'))

# Find Rust qmap_5 output parquet
files <- list.files(out_dir, pattern = 'processed__.*__null_interaction__qmap_5\\.parquet$', full.names = TRUE)
if (length(files) < 1) {
  stop('No Rust qmap_5 parquet found in output dir: ', out_dir)
}
rust_pq <- files[[1]]
dfr <- read_parquet(rust_pq)
# Rust parquet should contain lrt_adj if quantile_map_once returns adjusted response; if not, construct it:
has_lrt_adj <- 'lrt_adj' %in% names(dfr)
if (!has_lrt_adj) {
  # If the parquet contains only rt adjusted (i.e., rt := lrt_adj), we can rename 'rt' to 'lrt_adj' for comparison purpose.
  # Otherwise we would need Rust to emit lrt_adj explicitly.
  dfr <- dfr %>% mutate(lrt_adj = as.numeric(rt))
}

dfr <- dfr %>%
  mutate(
    participant_id = as.character(participant_id),
    cong = as.integer(cong),
    prev_cong = as.integer(prev_cong)
  )

mod_rust <- lmer(lrt_adj ~ cong * prev_cong + (1 | participant_id), data = dfr, REML = TRUE)
co_rust <- summary(mod_rust)$coefficients
coefs_rust <- tibble(term = rownames(co_rust),
                     estimate = co_rust[, 'Estimate'],
                     std_error = co_rust[, 'Std. Error']) %>% 
  filter(term %in% c('(Intercept)', 'cong', 'cong:prev_cong'))

# R implementation of quantile_map_once on raw rt (as provided)
quantile_map_once <- function(data, scale_col = "rt", kappa = 5, ngrid = 200) {
  pats <- unique(data$participant_id)
  taus <- seq(0, 1, length.out = ngrid)

  global_cell_q <- data %>%
    group_by(cong, prev_cong) %>%
    summarise(
      global_qs = list(quantile(.data[[scale_col]], probs = taus, na.rm = TRUE, type = 8)),
      .groups = "drop"
    )

  out_list <- purrr::map(pats, function(pid) {
    sub <- dplyr::filter(data, participant_id == pid)
    if (nrow(sub) < 2) {
      return(sub %>% mutate(lrt_adj = .data[[scale_col]]))
    }

    cell_ecdf <- sub %>%
      group_by(cong, prev_cong) %>%
      summarise(
        ecdf_fun = list(ecdf(.data[[scale_col]])),
        n_cell = n(), .groups = "drop"
      ) %>%
      left_join(global_cell_q, by = c("cong", "prev_cong"))

    sub <- sub %>%
      left_join(cell_ecdf, by = c("cong", "prev_cong")) %>%
      rowwise() %>%
      mutate(tau = {
        if (n_cell < 2) {
          approx(taus, global_qs, xout = .data[[scale_col]], rule = 2)$y
        } else {
          ecdf_fun(.data[[scale_col]])
        }
      }) %>%
      ungroup()

    row_q <- sub %>%
      group_by(cong) %>%
      summarise(
        qs = list(quantile(.data[[scale_col]], probs = taus, na.rm = TRUE, type = 8)),
        .groups = "drop"
      )
    col_q <- sub %>%
      group_by(prev_cong) %>%
      summarise(
        qs = list(quantile(.data[[scale_col]], probs = taus, na.rm = TRUE, type = 8)),
        .groups = "drop"
      )
    grand_q <- quantile(sub[[scale_col]], probs = taus, na.rm = TRUE, type = 8)

    sub <- sub %>%
      left_join(row_q, by = "cong") %>%
      rename(row_qs = qs) %>%
      left_join(col_q, by = "prev_cong") %>%
      rename(col_qs = qs) %>%
      rowwise() %>%
      mutate(
        q_row_tau = approx(x = taus, y = row_qs, xout = tau, rule = 2)$y,
        q_col_tau = approx(x = taus, y = col_qs, xout = tau, rule = 2)$y,
        q_grand_tau = approx(x = taus, y = grand_q, xout = tau, rule = 2)$y,
        lrt_adj = q_row_tau + q_col_tau - q_grand_tau
      ) %>%
      ungroup() %>%
      select(-row_qs, -col_qs, -ecdf_fun, -n_cell, -global_qs)

    sub
  })

  bind_rows(out_list)
}

dfq <- quantile_map_once(df, scale_col = "rt", kappa = 5, ngrid = 200)

mod_r <- lmer(lrt_adj ~ cong * prev_cong + (1 | participant_id), data = dfq, REML = TRUE)
co_r <- summary(mod_r)$coefficients
coefs_r <- tibble(term = rownames(co_r),
                  estimate = co_r[, 'Estimate'],
                  std_error = co_r[, 'Std. Error']) %>% 
  filter(term %in% c('(Intercept)', 'cong', 'cong:prev_cong'))

# Output JSON comparing coefficients
res <- list(
  original = coefs_orig,
  rust_qmap5 = coefs_rust,
  r_qmap5 = coefs_r
)
# Convert tibbles to plain lists
res <- lapply(res, function(df) {
  list(
    terms = df$term,
    estimates = df$estimate,
    std_errors = df$std_error
  )
})
cat(jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE))
"#;
    script.to_string()
}

#[test]
fn compare_rust_qmap_with_r_lmer_coeffs() {
    // Skip if Rscript not available
    let rscript_path = which::which("Rscript");
    if rscript_path.is_err() {
        eprintln!("Skipping test: Rscript not found in PATH");
        return;
    }

    //let input = write_small_input(&tmp);
    let input = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..") // up from rust/
        .join("data")
        .join("raw")
        .join("merged_data.csv");
    if !input.exists() {
        eprintln!("Skipping test: input CSV not found at {}", input.display());
        return;
    }
    let tmp = TempDir::new().expect("temp dir");
    let out_dir = tmp.path().join("out");
    fs::create_dir_all(&out_dir).expect("create out dir");
    // Run Rust process to produce qmap_5 null_interaction parquet
    let mut cmd = std::process::Command::cargo_bin("process").expect("find process binary");
    cmd.arg("--input")
        .arg(&input)
        .arg("--output-dir")
        .arg(&out_dir)
        .arg("--sample-sizes")
        .arg("1.0")
        .arg("--transformations")
        .arg("no_log_rt")
        .arg("--outliers")
        .arg("none")
        .arg("--effect-conditions")
        .arg("null_interaction")
        .arg("--strip-methods")
        .arg("qmap_5")
        .arg("--threads")
        .arg("2");
    let out = cmd.output().expect("run process");

    eprintln!(
        "=== Rust process stdout ===\n{}",
        String::from_utf8_lossy(&out.stdout)
    );
    eprintln!(
        "=== Rust process stderr ===\n{}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        out.status.success(),
        "process failed: stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );

    // Write R script to temp
    let rscript = r_script_contents();
    let rfile = tmp.path().join("compare.R");
    let mut f = fs::File::create(&rfile).expect("create R file");
    f.write_all(rscript.as_bytes()).expect("write R file");

    // Run Rscript
    let output = Command::new(rscript_path.unwrap())
        .arg(&rfile)
        .arg(&input)
        .arg(&out_dir)
        .output()
        .expect("run Rscript");

    eprintln!(
        "=== Rscript stdout (summary + JSON) ===\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
    eprintln!(
        "=== Rscript stderr ===\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    assert!(
        output.status.success(),
        "Rscript failed: stdout={} stderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    // Parse JSON
    let json = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&json).expect("parse json");

    // Extract estimates
    let orig_terms = v["original"]["terms"].as_array().unwrap();
    let orig_est = v["original"]["estimates"].as_array().unwrap();
    let rust_est = v["rust_qmap5"]["estimates"].as_array().unwrap();
    let r_est = v["r_qmap5"]["estimates"].as_array().unwrap();

    // Helper to find indices
    let find_term_index = |terms: &Vec<serde_json::Value>, name: &str| -> usize {
        terms
            .iter()
            .position(|t| t.as_str().unwrap() == name)
            .unwrap()
    };
    let i_intercept = find_term_index(&orig_terms.clone(), "(Intercept)");
    let i_cong = find_term_index(&orig_terms.clone(), "cong");
    let i_inter = find_term_index(&orig_terms.clone(), "cong:prev_cong");

    let orig_cong = orig_est[i_cong].as_f64().unwrap();
    let rust_cong = rust_est[i_cong].as_f64().unwrap();
    let r_cong = r_est[i_cong].as_f64().unwrap();

    let orig_inter = orig_est[i_inter].as_f64().unwrap();
    let rust_inter = rust_est[i_inter].as_f64().unwrap();
    let r_inter = r_est[i_inter].as_f64().unwrap();

    let orig_int_term = orig_est[i_inter].as_f64().unwrap();
    let rust_int_term = rust_est[i_inter].as_f64().unwrap();
    let r_int_term = r_est[i_inter].as_f64().unwrap();

    println!("=== Comparison Summary ===");
    println!(
        "Original:     Intercept={:.4}, cong={:.4}, cong:prev_cong={:.4}",
        orig_inter, orig_cong, orig_int_term
    );
    println!(
        "Rust qmap_5:  Intercept={:.4}, cong={:.4}, cong:prev_cong={:.4}",
        rust_inter, rust_cong, rust_int_term
    );
    println!(
        "R qmap_5:     Intercept={:.4}, cong={:.4}, cong:prev_cong={:.4}",
        r_inter, r_cong, r_int_term
    );

    // Checks:
    // 1) Rust congruency effect should be closer to R's congruency effect than to zero (preserve main effect)
    assert!(
        (rust_cong - r_cong).abs() <= r_cong.abs() * 0.25,
        "Rust cong deviates too much from R cong: rust={} r={}",
        rust_cong,
        r_cong
    );

    // 2) Interaction (cong:prev_cong) should be near zero for both Rust and R; compare closeness
    let orig_int = orig_est[i_inter].as_f64().unwrap();
    let rust_int = rust_est[i_inter].as_f64().unwrap();
    let r_int = r_est[i_inter].as_f64().unwrap();
    assert!(
        rust_int.abs() <= orig_int.abs() * 0.25,
        "Rust interaction not sufficiently reduced: rust={} orig={}",
        rust_int,
        orig_int
    );
    assert!(
        r_int.abs() <= orig_int.abs() * 0.25,
        "R interaction not sufficiently reduced: r={} orig={}",
        r_int,
        orig_int
    );

    // 3) Intercept should be roughly preserved (allow some drift)
    let orig_b0 = orig_est[i_intercept].as_f64().unwrap();
    let rust_b0 = rust_est[i_intercept].as_f64().unwrap();
    let r_b0 = r_est[i_intercept].as_f64().unwrap();
    assert!(
        (rust_b0 - r_b0).abs() <= (orig_b0.abs() * 0.10 + 1.0),
        "Rust intercept deviates too much from R intercept: rust={} r={} orig={}",
        rust_b0,
        r_b0,
        orig_b0
    );
}
