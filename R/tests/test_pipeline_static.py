#!/usr/bin/env python3
"""Static smoke checks for pipeline orchestration regressions."""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def read(rel: str) -> str:
    return (ROOT / rel).read_text(encoding="utf-8")


def test_targets_requires_rust_outputs_before_modeling() -> None:
    targets = read("_targets.R")
    assert "processed files missing" in targets
    assert "expected[!file.exists(expected)]" in targets


def test_targets_tracks_processed_files_and_dependencies() -> None:
    targets = read("_targets.R")
    assert "dependency_preflight" in targets
    assert "format = \"file\"" in targets
    assert "expected" in targets


def test_raw_data_target_uses_config_raw_csv() -> None:
    targets = read("_targets.R")
    assert "config$raw_csv" in targets
    assert 'file.path(paths$data_raw, "merged_data.csv")' not in targets


def test_current_strip_methods_and_no_null_both() -> None:
    pipeline = read("pipeline.yaml")
    assert "all_indexed.csv" in pipeline
    assert "local_median_residual" in pipeline
    assert "additive_qmap" in pipeline
    assert "qmap_5" not in pipeline
    assert "null_both" not in pipeline


def main() -> None:
    tests = [
        test_targets_requires_rust_outputs_before_modeling,
        test_targets_tracks_processed_files_and_dependencies,
        test_raw_data_target_uses_config_raw_csv,
        test_current_strip_methods_and_no_null_both,
    ]
    failures = []
    for test in tests:
        try:
            test()
        except AssertionError as exc:
            failures.append((test.__name__, exc))

    if failures:
        for name, exc in failures:
            print(f"FAIL {name}: {exc}")
        raise SystemExit(1)

    for test in tests:
        print(f"PASS {test.__name__}")


if __name__ == "__main__":
    main()
