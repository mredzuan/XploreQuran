#' Generate a Word Cloud for Quran Translation
#'
#' @description
#' Generates a word cloud from a Quran translation. Grouping, selection, and
#' preprocessing are fully controlled via a \code{TransAnalyticConfig} object,
#' which acts as the complete analysis context.
#'
#' @param tanzil_trans_object A \code{translationList} object created with
#'   \code{tanzil_translation()}.
#' @param config A \code{TransAnalyticConfig} object created with
#'   \code{trans_analytic_config()}. Defaults to \code{trans_analytic_config()}
#'   (all surahs, English, unigram).
#' @param max_word Integer. Maximum number of words to display. Default \code{200}.
#' @param min_freq Integer. Minimum frequency for a word to appear. Default \code{1}.
#'
#' @return Invisibly returns the word frequency data frame used to render the cloud.
#'
#' @import dplyr
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'
#' @examples
#' # data(trans_en_sahih, package = "XploreQuran")
#'
#' # Word cloud for entire translation (default config)
#' # wordcloud_trans(trans_en_sahih)
#'
#' # Surah 114 only, Malay translation
#' # cfg <- trans_analytic_config(by = "surah", sub_by = 114, stopword_lang = "ms")
#' # wordcloud_trans(trans_ms_basmeih, config = cfg, max_word = 50)
wordcloud_trans <- function(
    tanzil_trans_object,
    config   = trans_analytic_config(),
    max_word = 200,
    min_freq = 1
) {
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"TransAnalyticConfig" %in% class(config)) {
    stop("config must be a TransAnalyticConfig object. Create one with trans_analytic_config().")
  }

  # Run preprocessing pipeline (sub_by filtering is handled inside)
  tokens <- preprocess_tokens(tanzil_trans_object, config)

  # Count word frequency across all selected groups
  word_freq <- tokens %>% count(word, sort = TRUE)

  # Render word cloud
  wordcloud::wordcloud(
    words        = word_freq$word,
    freq         = word_freq$n,
    max.words    = max_word,
    min.freq     = min_freq,
    random.order = FALSE,
    rot.per      = 0.1,
    colors       = brewer.pal(8, "Dark2")
  )

  invisible(word_freq)
}
