# Quick Start & Operational Guide

## Quick Start (5 minutes)

### Prerequisites

```bash
# System requirements
# - R 4.2+
# - Rust 1.70+
# - SLURM (for HPC) or local machine
# - ~10 GB free disk space for test data

# R packages
Rscript -e "install.packages(c('targets', 'crew', 'tidyverse', 'lme4', 'afex', 'logger', 'polars'))"

# Rust toolchain
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Run locally (test data)

```bash
# 1. Clone/setup project
cd multiverse-analysis

# 2. Generate small test dataset
Rscript -e "
  source('R/_targets.R')
  data <- generate_test_data(n_participants = 20, n_trials_per_participant = 100)
  write_csv(data, 'data/raw/experiment.csv')
"

# 3. Build Rust components
cd rust
cargo build --release --bin process
cd ..

# 4. Run pipeline
USE_HPC=FALSE N_WORKERS=2 Rscript R/run.R

# 5. View results
cat outputs/analysis_report.html | head -50
```

**Expected time**: ~30-60 seconds for small dataset.

---

## Operational Procedures

### Local Development Mode

**Scenario**: Testing new model, debugging branch logic

```bash
# 1. Start R session
R

# 2. Interactive target inspection
tar_visnetwork()  # View DAG
tar_load(branch_specs)  # Load branch definitions
tar_load(raw_data)  # Load raw data

# 3. Run subset of branches
tar_make(names = c("branch_specs", "raw_data", "processed_data_all"))

# 4. Debug specific branch
branch <- branch_specs[[15]]
data <- read_parquet(glue::glue("data/processed/processed__{branch$sample_size}__..."))

# 5. Test model fitting
source("R/models.R")
result <- fit_model(data, MODELS[[branch$model]], branch$model, branch$branch_id)
```

### HPC Batch Submission

**Scenario**: Running full analysis on production data

```bash
# 1. Prepare data
# Place experiment.csv in data/raw/

# 2. Configure job
# Edit slurm_submit.sh:
#   - CPUs: match your cluster capacity
#   - Memory: 4-6 GB per CPU typical
#   - Time: 4-8 hours for 1M observations
#   - Partition: CPU or GPU as available

# 3. Submit
sbatch slurm_submit.sh

# 4. Monitor
squeue -u $USER
tail -f logs/slurm_*.out

# 5. Retrieve results
# After job completes, results in outputs/ and logs/ directories
```

### Debugging Failed Branches

**Scenario**: Some models didn't converge

```bash
# 1. Check logs
grep "convergence code" logs/r_execution.log

# 2. Identify problematic branches
tar_read(results_summary) %>% 
  filter(!full_converged | !null_converged)

# 3. Refit with diagnostics
problem_branch <- # ...
tryCatch({
  result <- tar_read(model_results)[[problem_branch]]
  # Inspect result$full_model@optinfo$conv
}, error = function(e) print(e))

# 4. Try alternative optimizer
MODELS$lmm_intercept$control <- list(
  optimizer = "Nelder_Mead",  # Different optimizer
  optControl = list(maxfun = 100000)
)

# 5. Rerun specific branches only
tar_make(
  names = glue::glue("model_results_{c(5, 10, 15)}")
)
```

### Performance Profiling

**Scenario**: Pipeline slower than expected

```bash
# 1. Profile Rust processing
cd rust
cargo bench --bin process

# 2. Profile R model fitting
Rprof("prof.log")
tar_make()
Rprof(NULL)
summaryRprof("prof.log")  # View results

# 3. Check resource usage
# During execution:
htop  # Monitor CPU/memory
nvidia-smi  # If using GPU

# 4. Optimize bottleneck
# If Rust slow: increase POLARS_MAX_THREADS
# If R slow: check convergence warnings, try different optimizer
```

### Data Quality Checks

**Scenario**: Ensuring transformations are correct

```bash
# 1. Compare raw vs. processed data
raw <- read_csv("data/raw/experiment.csv")
proc <- read_parquet("data/processed/processed__1.0__log_rt__sd_3.parquet")

# 2. Spot check specific cases
raw %>% 
  filter(participant_id == 1, trial_num <= 5) %>%
  left_join(proc %>% filter(participant_id == 1) %>% head(5))

# 3. Verify outlier removal
old_n <- nrow(raw %>% filter(participant_id == 1))
new_n <- nrow(proc %>% filter(participant_id == 1))
sprintf("Removed %d/%d rows (%.1f%%)", old_n - new_n, old_n, 100*(old_n-new_n)/old_n)

# 4. Check transformation correctness
proc %>%
  head(10) %>%
  select(rt, participant_id)

# rt should be log scale:
# If untransformed data ~300-500ms
# Log RT should be ~5.7-6.2
```

---

## Common Issues & Solutions

### Issue: Rust compilation fails

**Symptom**: `cargo build --release` fails with cryptic error

**Solutions**:
```bash
# 1. Update Rust
rustup update

# 2. Check dependencies are compatible
cargo tree  # Check version conflicts

# 3. Try incremental rebuild
cargo clean
cargo build --release

# 4. Check system dependencies
# On Ubuntu:
sudo apt-get install pkg-config libssl-dev

# Verify with sample build:
cargo new test_project
cd test_project
cargo build
```

### Issue: R package installation fails

**Symptom**: `install.packages()` error, usually lme4

**Solutions**:
```r
# 1. Use precompiled binary
options(pkgType = "binary")
install.packages("lme4")

# 2. Install build dependencies (Ubuntu/Debian)
system("sudo apt-get install build-essential gfortran")

# 3. Use conda instead
# conda create -n multiverse -c conda-forge r-lme4 r-afex
# conda activate multiverse

# 4. Try alternative installation
remotes::install_github("lme4/lme4")
```

### Issue: Out of memory

**Symptom**: "Cannot allocate X GB" or OS kills process

**Solutions**:
```bash
# 1. Reduce parallel workers
export N_WORKERS=2  # Down from 8

# 2. Process subset of data locally first
head -100000 data/raw/experiment.csv > data/test.csv
./target/release/process --input data/test.csv

# 3. Increase available swap (temporary)
sudo fallocate -l 16G /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

# 4. Run on larger machine/node
sbatch --mem=256G slurm_submit.sh  # Increase request
```

### Issue: Models not converging

**Symptom**: `WARNING: Model failed to converge` in output

**Solutions**:
```r
# 1. Try different optimizer
control = list(optimizer = "bobyqa")  # vs. default

# 2. Rescale variables (common issue)
data <- data %>%
  mutate(
    rt_scaled = scale(rt)[,1],
    rt_centered = rt - mean(rt)
  )

# 3. Simplify model
# Use intercept-only random effects if full fails

# 4. Check data quality
data %>%
  group_by(participant_id) %>%
  summarise(n = n(), .groups = 'drop') %>%
  arrange(n)  # Any participants with very few trials?

# 5. Use control parameters
lmerControl(
  optimizer = "bobyqa",
  optControl = list(maxfun = 100000),
  check.conv.singular = .makeCC(action = "warn", tol = 1e-4)
)
```

---

## Key Files Reference

### Configuration

- `_targets.R` - Main pipeline definition (axes, models, sample sizes)
- `slurm_submit.sh` - HPC job parameters

### Data I/O

- Input: `data/raw/experiment.csv` (rt, cong, prev_cong, participant_id, correct_response)
- Intermediate: `data/processed/*.parquet` (one per branch)
- Output: `outputs/analysis_report.html`, `results_summary.csv`

### Execution Scripts

- `R/run.R` - Execution entry point (local/HPC)
- `slurm_submit.sh` - HPC submission wrapper

### Core Logic

- `R/models.R` - Model fitting (fit_model, fit_lmm, fit_rmanova)
- `rust/src/lib.rs` - Rust core (BranchPipeline, OutlierMethod)
- `rust/src/bin/process.rs` - Main CLI tool

### Testing

- `data/test_sim/` - Simulated test data
- `tests/` - Unit tests

---

## Monitoring & Logging

### Real-time monitoring (HPC)

```bash
# Terminal 1: Monitor job
watch -n 5 "squeue -u $USER"

# Terminal 2: Stream output
tail -f logs/slurm_*.out

# Terminal 3: Monitor resources
watch -n 2 "sinfo -p cpu"  # Check partition
watch -n 2 nvidia-smi  # If using GPU
```

### Log interpretation

**Logs location**: `logs/`

| File | Content |
|------|---------|
| `pipeline.log` | R targets execution |
| `cargo_build.log` | Rust compilation |
| `r_execution.log` | Model fitting details |
| `crew.log` | Worker communication |
| `slurm_*.out` | SLURM job output |
| `slurm_*.err` | SLURM errors |

### Extracting diagnostics

```bash
# Count successful branches
grep "Branch processing complete" logs/r_execution.log | wc -l

# Find convergence issues
grep -i "convergence" logs/r_execution.log

# Check timing
grep "time_ms" logs/r_execution.log | awk '{sum += $NF} END {print "Total: " sum "ms"}'

# Count errors
grep -i "error" logs/* | wc -l
```

---

## Performance Tuning

### Optimize for speed

```bash
# 1. Increase Rust threads
export POLARS_MAX_THREADS=32  # Use all CPUs

# 2. Disable detailed logging
USE_HPC=TRUE N_WORKERS=4 Rscript R/run.R  # Fewer workers = faster model fitting

# 3. Use release build
cd rust && cargo build --release
cd ..

# 4. Pin CPU affinity (Linux)
numactl --physcpubind=0-31 ./target/release/process ...
```

### Optimize for memory

```bash
# 1. Use fewer workers
export N_WORKERS=2

# 2. Process in batches (modify _targets.R)
# Split 80 branches into 4 × 20, submit separate jobs

# 3. Use streaming Rust API
# For very large files, process in chunks
```

### Optimize for reproducibility

```r
# 1. Pin random seeds
set.seed(20251122)

# 2. Record all parameters
# _targets.R already does this

# 3. Version control code
git commit -am "Analysis config v1"
git tag analysis-v1

# 4. Archive results
tar czf results_20251122.tar.gz outputs/ logs/
```

---

## Production Checklist

Before running on cluster:

- [ ] Code reviewed and tested locally
- [ ] Data validated (no missing values in required columns)
- [ ] Random seeds set
- [ ] Resource requests are reasonable
- [ ] Email notification configured
- [ ] Results backup location identified
- [ ] Cluster queue/partition confirmed
- [ ] Module loading verified (R, modules in slurm_submit.sh)
- [ ] Logs directory writable
- [ ] Output directory has space

---

**Questions?** See `docs/design_doc.md` for architecture details.
