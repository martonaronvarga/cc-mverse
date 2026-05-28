# Design & Architecture Document

## Executive Summary

**Multiverse Analysis** is a high-performance polyglot system for exploring reaction time data across 4 analysis dimensions (sample size, transformation, outlier filtering, model specification). It combines:

- **R (targets)** - Workflow orchestration & model fitting
- **Rust (polars + rayon)** - Fast parallel data processing
- **C++ (optional)** - Custom statistical models for performance
- **SLURM** - HPC cluster coordination

**Goal**: Process 2 × 10 × 4 = 80 branches efficiently with reproducibility and debugging support.

---

## Architecture Overview

### Layer 1: Orchestration (R - targets)

**Framework**: `targets` + `crew` + `polars`

**Responsibilities**:
- Define complete analysis DAG (directed acyclic graph)
- Generate branch specifications automatically
- Schedule parallel execution (local/HPC)
- Aggregate results and generate reports
- Handle error recovery

**Key Files**:
- `_targets.R` - Pipeline definition
- `R/run.R` - Execution controller
- `R/models.R` - Model fitting
- `slurm_submit.sh` - HPC submission

**Design Rationale**:
- targets enables reproducible, incremental workflows
- crew abstracts away cluster scheduling details
- Immutable targets make debugging trivial
- Separation of R logic from compute-intensive operations

### Layer 2: Data Processing (Rust)

**Framework**: `polars` + `rayon` + `clap`

**Responsibilities**:
- Load CSV data efficiently (streaming for large files)
- Generate all branch combinations in parallel
- Apply transformations (log RT)
- Apply outlier filters (SD, MAD, range-based)
- Export as parquet for downstream R processing
- Comprehensive logging & diagnostics

**Key Files**:
- `src/lib.rs` - Core library with BranchPipeline
- `src/bin/process.rs` - Main CLI tool
- `src/bin/simulate.rs` - Test data generation

**Performance Characteristics**:
- Polars lazy evaluation for query optimization
- Rayon work-stealing scheduler (adaptive parallelism)
- Memory-efficient column-based storage
- ~4-10x speedup vs. pure R for large datasets

**Example: Processing Pipeline**

```rust
// For each branch configuration:
for config in &configs {
    let pipeline = BranchPipeline::new(*config);
    
    // 1. Sample (reproducible)
    let sampled = pipeline.sample_data(data)?;  
    
    // 2. Transform (log RT, etc.)
    let transformed = pipeline.apply_transformation(&sampled)?;
    
    // 3. Filter (SD/MAD/range)
    let filtered = pipeline.filter_outliers(&transformed)?;
    
    // 4. Export (parquet)
    filtered.write_parquet(path)?;
}
```

All branches processed in parallel via `rayon::par_iter()`.

### Layer 3: Statistical Modeling (R)

**Packages**: `lme4`, `afex`, `lmerTest`

**Responsibilities**:
- Fit RMANOVA (via ez/afex)
- Fit LMM variants (1 + cong slope, 1 + both slopes)
- Null model comparisons
- Convergence checking
- Effect size & hypothesis testing

**Key Functions**:
- `fit_model()` - Dispatcher
- `fit_rmanova()` - Repeated measures ANOVA
- `fit_lmm()` - Linear mixed models
- `extract_results()` - Standardized output

**Example: Model Fit**

```r
# Full model
rt ~ cong * prev_cong + (1 + cong * prev_cong | participant_id)

# Null model (context effect only)
rt ~ 1 + (1 + cong * prev_cong | participant_id)

# Comparison
anova(full_model, null_model, test = "Chisq")
```

### Optional Layer 4: C++ Statistical Models

**When to use**: If R model fitting becomes bottleneck (unlikely for typical sample sizes).

**Options**:
1. Direct C++ LMM implementation via `Rcpp`
2. Custom Newton-Raphson optimizer
3. GPU-accelerated matrix operations (if available)

**Example interface**:
```r
fit_cpp_lmm(data, formula, random_effects)
```

---

## Data Flow

```
experiment.csv (raw data)
    ↓
[Rust] generate 80 branch configs
    ↓
[Rust] parallel processing (rayon)
    ├─ branch 1 (sample_50%, log, sd_2)   → processed__0.5__log_rt__sd_2.parquet
    ├─ branch 2 (sample_50%, log, sd_2.5) → processed__0.5__log_rt__sd_2.5.parquet
    ├─ ...
    └─ branch 80 (sample_100%, no_log, no_filter)
    ↓
[R] read parquet files (efficient polars I/O)
    ↓
[R] fit models in parallel (crew workers)
    ├─ RMANOVA
    ├─ LMM (intercept only)
    ├─ LMM (intercept + cong slope)
    └─ LMM (full random effects)
    ↓
[R] aggregate results → results_summary.csv
    ↓
[R] generate report → analysis_report.html
```

---

## Key Design Decisions

### 1. Why Rust for data processing?

| Aspect | R | Rust |
|--------|---|------|
| Speed | Moderate | 4-10x faster |
| Parallelism | GIL/forking overhead | Native work-stealing |
| Memory safety | GC, can be inefficient | Zero-cost abstractions |
| Learning curve | Low | Higher, but worth it |

**Decision**: Use Rust for data processing bottleneck, keep R for statistical modeling.

### 2. Why targets for orchestration?

- **Reproducibility**: Every target is a pure function
- **Incrementality**: Only rerun changed targets
- **Scalability**: Works locally → cluster with one config change
- **Transparency**: All dependencies visible in DAG

### 3. Why separate processing from modeling?

- **Testability**: Validate transformations independently
- **Flexibility**: Easy to add new branches
- **Debugging**: Compare processed data across filters
- **Caching**: Reuse processed data across model variants

### 4. Why no dynamic branching in Rust?

- **Reproducibility**: All configs known upfront
- **Logging**: Branch IDs deterministic
- **Debugging**: Enumerate all possibilities
- **Monitoring**: Track progress per branch

---

## Computational Complexity

### Time Complexity

| Stage | Branches | Per-branch | Total |
|-------|----------|-----------|-------|
| Data processing (Rust) | 80 | O(n log n) sampling, O(n) filter | ~O(n) parallel |
| Model fitting | 80 | O(n p²) LMM + O(iterations) optimizer | O(n p²) × 80 |

**n** = observations, **p** = parameters (~20 for full random effects)

For **n=10,000** participants × 100 trials = 1M observations:
- Rust processing: ~2-5 seconds (Polars optimizations)
- Model fitting: ~30-60 seconds total (8-16 workers)

### Memory Usage

- Raw data: ~50 MB (1M observations × 5 columns)
- Processed branches: ~50 MB × 80 = 4 GB (Parquet compression ~50%)
- Working memory (R): ~100 MB per worker

**Total**: ~8-10 GB for full pipeline on single node.

---

## Local Development Workflow

### 1. Setup (once)

```bash
# Install dependencies
cd rust && cargo build --release

# Create test data
Rscript -e "source('R/_targets.R'); generate_test_data()" > data/test_sim/data.csv
```

### 2. Develop & test

```bash
# Test Rust component
cd rust
cargo test
cargo run --release --bin simulate -- --n-participants 50 --output data/test_sim/sim.csv

# Run small pipeline locally
cd ..
USE_HPC=FALSE N_WORKERS=2 Rscript R/run.R

# Or use targets interactively
tar_visnetwork()  # View DAG
tar_load(raw_data)  # Load specific target
tar_make()  # Run all
```

### 3. Debug specific branch

```r
# Load pipeline definition
source("_targets.R")

# Manually run one branch
branch_spec <- branch_specs[[42]]  # 42nd combination
data <- tar_read(processed_data_all)[[42]]
result <- fit_model(data, MODELS[[branch_spec$model]], ...)
```

### 4. Profile performance

```bash
# Rust profiling
cd rust
cargo bench
perf record --call-graph=dwarf ./target/release/process ...

# R profiling
Rprof("profile.log"); tar_make(); Rprof(NULL)
summaryRprof("profile.log")
```

---

## HPC Deployment

### Submission

```bash
# Adjust parameters in slurm_submit.sh
sbatch slurm_submit.sh

# Monitor
squeue -u $USER
tail -f logs/slurm_*.out
```

### Resource Allocation

**Recommended** (for 1M observations, 80 branches):
- Nodes: 1
- CPUs: 32 (Rust uses all, R workers get 4 each)
- Memory: 128 GB
- Time: 4 hours
- Partition: CPU (not GPU needed)

**Calculation**:
- Rust processing: ~5s × 80 branches parallel = 5s (not sequential!)
- R modeling: 60s / 8 workers = ~8s effective per worker
- Total: ~30-40 seconds of actual computation + overhead

### Troubleshooting

**Crew workers won't start**:
```bash
# Check SLURM logs
cat logs/crew.log
# Verify R version, module loading
```

**Memory exceeded**:
```bash
# Reduce sample sizes or parallelize differently
# Option 1: Process fewer branches per job
# Option 2: Use streaming API in Rust for larger data
```

**Slow model fitting**:
```bash
# Check convergence warnings
grep "convergence code" logs/r_execution.log

# Use bobyqa optimizer (vs. default)
# Consider C++ implementation
```

---

## Reproducibility & Versioning

### Track everything:

```r
# sessionInfo() captured in report
# Git commit of code
# Data version (hash)
# Seed for all RNG operations
```

**Outputs include metadata**:
- Processing parameters per branch
- Model convergence status
- Execution timestamp
- System information
- Software versions

### Rerun analysis:

```bash
git checkout <commit-hash>
sbatch slurm_submit.sh
```

Results will be identical (deterministic sampling, fixed seeds).

---

## Extending the System

### Adding a new transformation

1. **Rust**:
```rust
pub enum Transformation {
    LogRt,
    NoLogRt,
    SqrtRt,  // NEW
}

// In BranchPipeline::apply_transformation()
Transformation::SqrtRt => {
    let rt = data.column("rt")?;
    let sqrt_rt = rt.f64()?.apply(|v| v.map(f64::sqrt));
    // ...
}
```

2. **R** (`_targets.R`):
```r
TRANSFORMATIONS <- c("log_rt", "no_log_rt", "sqrt_rt")
```

3. **Test**:
```bash
cargo run --release --bin process -- \
  --input data/test.csv \
  --transformations sqrt_rt
```

### Adding a new model

1. **R** (`R/models.R`):
```r
fit_gamm <- function(data, spec) {
  # Use mgcv package for generalized additive models
  # Return consistent structure
}
```

2. **R** (`_targets.R`):
```r
MODELS <- list(
  # ... existing
  gamm = list(
    type = "gamm",
    formula_full = "rt ~ s(cong) + s(prev_cong) + ...",
    # ...
  )
)
```

3. **Dispatcher**:
```r
fit_model <- function(...) {
  switch(model_spec$type,
    "rmanova" = ...,
    "lmm" = ...,
    "gamm" = fit_gamm(...),  # NEW
    # ...
  )
}
```

---

## Testing Strategy

### Unit tests

```bash
# Rust
cd rust && cargo test --lib

# R
testthat::test_dir("tests/R")
```

### Integration tests

```r
# Local mini-pipeline
tar_make(branches = 1:5)  # Run only first 5 branches
```

### Validation

- Cross-check Rust vs. R transformations on test data
- Compare LMM results with other software (nlme, glmmTMB)
- Verify metadata tracking is complete

---

## Performance Optimization Roadmap

### Phase 1 (current)
- ✅ Parallel Rust processing (Rayon)
- ✅ Efficient data formats (Parquet, Polars)
- ✅ Vectorized R operations

### Phase 2 (medium-term)
- [ ] GPU acceleration for matrix operations (CUDA)
- [ ] Timely dataflow for complex dependencies
- [ ] C++ LMM implementation (10x speedup possible)

### Phase 3 (advanced)
- [ ] Streaming processing for TB-scale data
- [ ] Incremental computation (update only changed branches)
- [ ] Distributed across multiple nodes

---

## References & Resources

- **targets**: https://books.ropensci.org/targets/
- **crew**: https://wlandau.github.io/crew/
- **Polars**: https://pola-rs.github.io/polars/
- **Rayon**: https://docs.rs/rayon/
- **lme4**: https://cran.r-project.org/web/packages/lme4/

---

**Last updated**: 2025-11-22
**Version**: 1.0
