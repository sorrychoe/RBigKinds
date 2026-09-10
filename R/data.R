#' BigKinds sample export: news mentioning President Lee (August 2026)
#'
#' A raw BigKinds article export, shipped as sample data so that the package
#' functions can be demonstrated on a realistic BigKinds dataset. The raw
#' spreadsheet it was built from lives in
#' \code{data/president_lee_2026_08.xlsx} (kept in the repository, not in the
#' installed package).
#'
#' @format A tibble with 6,537 rows and 19 columns, i.e. the standard BigKinds
#'   export layout. The columns consumed by this package are:
#' \describe{
#'   \item{\code{일자}}{Publication date (\code{YYYYMMDD}).}
#'   \item{\code{언론사}}{News outlet.}
#'   \item{\code{제목}}{Article headline.}
#'   \item{\code{키워드}}{Comma-separated keywords extracted by BigKinds.}
#' }
#' @source BigKinds, Korea Press Foundation. \url{https://www.bigkinds.or.kr/}
#' @usage data(president_lee_2026_08)
#' @keywords datasets
"president_lee_2026_08"
