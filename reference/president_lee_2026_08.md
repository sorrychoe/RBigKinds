# BigKinds sample export: news mentioning President Lee (August 2026)

A raw BigKinds article export, shipped as sample data so that the
package functions can be demonstrated on a realistic BigKinds dataset.
The raw spreadsheet it was built from lives in
`data/president_lee_2026_08.xlsx` (kept in the repository, not in the
installed package).

## Usage

``` r
data(president_lee_2026_08)
```

## Format

A tibble with 6,537 rows and 19 columns, i.e. the standard BigKinds
export layout. The columns consumed by this package are:

- `일자`:

  Publication date (`YYYYMMDD`).

- `언론사`:

  News outlet.

- `제목`:

  Article headline.

- `키워드`:

  Comma-separated keywords extracted by BigKinds.

## Source

BigKinds, Korea Press Foundation. <https://www.bigkinds.or.kr/>
