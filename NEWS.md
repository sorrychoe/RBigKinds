# RBigKinds 0.5.2

## Bug fixes

* `network_graph()` now validates its `dcm` argument instead of an undefined
  `df`, so it no longer fails with `object 'df' not found`.
* `association()` gained a `min_confidence` argument. Confidence is now filtered
  against the confidence threshold rather than the support threshold.
* `tfidf_vector()` now builds one row per article (document) instead of one row
  per keyword token, so the resulting matrix is a proper document-by-term
  TF-IDF matrix. Short (2-character) Korean keywords are no longer dropped.
* `normalize_vector()` now performs per-row min-max scaling as documented
  (previously it applied a single global min-max over the whole matrix).
* `header_remover()` uses a correct character class (`[^]]`) when stripping
  bracketed headers.

## Data

* The bundled sample dataset is now `president_lee_2026_08` (6,537 BigKinds
  articles, August 2026), replacing `president_yoon_2023_05`. It is stored as
  a properly named, `xz`-compressed `.rda`; the source spreadsheet stays in
  the repository but is excluded from the build.

## Documentation and packaging

* `DESCRIPTION`: fixed the malformed `Authors@R` entry (email was being parsed
  as a middle name), dropped the stale hand-written `Author`/`Maintainer`
  fields, added `URL`, `BugReports`, and `Depends: R (>= 4.2)`, and rewrote the
  `Description` field so it no longer starts with the package name.
* Package-level documentation now uses the `"_PACKAGE"` sentinel.
* All files under `R/` are now ASCII-only: the Korean BigKinds column names are
  defined once as `\u`-escaped constants in `R/globals.R` and referenced
  through tidy-evaluation helpers, fixing the "non-ASCII characters" warning.
* Declared the previously undefined imports (`stats::kmeans`, `stats::reorder`,
  `utils::head`) and registered data-masked variables with
  `utils::globalVariables()`.
* `LICENSE` is now a valid DCF stub; the full MIT text moved to `LICENSE.md`.
* `NEWS.md` uses a parseable version heading.
* `DBSCAN()` documents its `min_samples` argument (was `min_sample`).
* `topic_modeling()` documents its `method` argument.
* `Kmeans()` example uses `k = 2` so it runs on the 3-row example matrix.
* Renamed `R/nework_analysis.R` to `R/network_analysis.R`.
* CI: `R-CMD-check` now also runs for pushes and pull requests targeting
  `release`; bumped `actions/checkout` to v4.
* `R CMD check` now passes cleanly (0 errors, 0 warnings, 0 notes).
