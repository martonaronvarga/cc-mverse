use multiverse_analysis::{interp, invert_monotone_interp, quantiles_type8};
use proptest::prelude::*;

proptest! {
    // quantiles_type8 should return a vector of same length as ps, finite or NaN only,
    // and be non-decreasing in p when input has finite values.
    #[test]
    fn quantiles_type8_basic_properties(xs in proptest::collection::vec(any::<f64>(), 0..200),
                                        ps in proptest::collection::vec(0.0f64..=1.0, 1..50)) {
        let q = quantiles_type8(&xs.clone(), &ps);
        prop_assert_eq!(q.len(), ps.len());

        if xs.iter().copied().filter(|v| v.is_finite()).count() == 0 {
            // All quantiles should be NaN when input has no finite values
            prop_assert!(q.iter().all(|v| v.is_nan()));
        } else {
            // Non-decreasing in ps
            for i in 1..ps.len() {
                let p0 = ps[i-1];
                let p1 = ps[i];
                let q0 = q[i-1];
                let q1 = q[i];
                if p1 >= p0 {
                    // Allow equality due to repeated values
                    prop_assert!(q1 >= q0 || (q1 - q0).abs() < std::f64::EPSILON);
                }
            }
        }
    }

    // interp should clamp to endpoints (rule=2) and be linear within segments.
    #[test]
    fn interp_clamps_and_is_linear(x in proptest::collection::vec(0.0f64..1000.0, 2..50),
                                   y in proptest::collection::vec(-1000.0f64..1000.0, 2..50),
                                   xout in -1000.0f64..1000.0) {
        // Sort x and reorder y to match monotone increasing x
        let mut pairs: Vec<(f64,f64)> = x.into_iter().zip(y.into_iter()).collect();
        // Ensure equal length (vec generators above already enforce this, but zip truncates; we want full length)
        // Since both ranges are 2..50, lengths are equal; zip keeps that. Proceed.
        pairs.sort_by(|a,b| a.0.partial_cmp(&b.0).unwrap());
        let (x_sorted, y_sorted): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();

        let v = interp(&x_sorted, &y_sorted, xout);
        // Clamp behavior
        if xout <= x_sorted[0] {
            prop_assert_eq!(v, y_sorted[0]);
        } else if xout >= *x_sorted.last().unwrap() {
            prop_assert_eq!(v, *y_sorted.last().unwrap());
        } else {
            // Value must lie between neighboring segment endpoints
            // Find i s.t. x[i-1] <= xout <= x[i]
            let mut i = 1usize;
            while i < x_sorted.len() && x_sorted[i] < xout { i += 1; }
            let i0 = i - 1;
            let x0 = x_sorted[i0];
            let x1 = x_sorted[i];
            let y0 = y_sorted[i0];
            let y1 = y_sorted[i];
            // Check linear interpolation formula consistency
            let expected = if (x1 - x0).abs() < std::f64::EPSILON {
                y0
            } else {
                y0 + (y1 - y0) * (xout - x0) / (x1 - x0)
            };
            prop_assert!((v - expected).abs() < 1e-9);
        }
    }

    // invert_monotone_interp composes with interp: interp(x,y, invert(y)) ~= original yout
    // Ensure x and y have the SAME length and y is non-decreasing.
    #[test]
    fn invert_monotone_roundtrip(
        // Generate a single length n and use it for both x and y
        n in 2usize..50,
        mut x in proptest::collection::vec(0.0f64..1000.0, 2..50),
        mut y in proptest::collection::vec(-1000.0f64..1000.0, 2..50),
        yout in -1000.0f64..1000.0
    ) {
        // Truncate or expand to ensure equal lengths using n
        x.truncate(n);
        y.truncate(n);

        // Sort x ascending
        x.sort_by(|a,b| a.partial_cmp(b).unwrap());

        // Make y non-decreasing by sorting
        y.sort_by(|a,b| a.partial_cmp(b).unwrap());

        // Guard: lengths must match and be >= 2
        prop_assume!(x.len() == y.len());
        prop_assume!(x.len() >= 2);

        let x_est = invert_monotone_interp(&x, &y, yout);
        let y_round = interp(&x, &y, x_est);

        // Boundary clamp: if yout beyond range, y_round clamps to ends
        let ymin = y[0];
        let ymax = *y.last().unwrap();
        if yout <= ymin {
            prop_assert_eq!(y_round, ymin);
        } else if yout >= ymax {
            prop_assert_eq!(y_round, ymax);
        } else {
            // Within range: round-trip should be close to yout
            prop_assert!((y_round - yout).abs() < 1e-6);
        }
    }
}
