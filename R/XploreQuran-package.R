#' XploreQuran: Quran Translation Text Mining & Analytics
#'
#' @description
#' XploreQuran provides tools to download, process, and perform text mining
#' and analytics on Quran translations from \url{https://tanzil.net/trans/}.
#'
#' The package supports multiple languages and translators, with a modular
#' design allowing consistent analysis across different functions via the
#' \code{\link{QuranConfig}} S3 object.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{tanzil_translation}}}{Download a Quran translation from tanzil.net}
#'   \item{\code{\link{quran_config}}}{Create a reusable analysis configuration object}
#'   \item{\code{\link{tf_trans}}}{Compute term frequency and TF-IDF}
#'   \item{\code{\link{wordcloud_trans}}}{Generate a word cloud}
#'   \item{\code{\link{run_app}}}{Launch the interactive Shiny web application}
#' }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud wordcloud
#' @importFrom SnowballC wordStem
#' @importFrom stopwords stopwords stopwords_getlanguages
## usethis namespace: end
NULL
