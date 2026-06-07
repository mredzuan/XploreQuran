#' Generate a Word Cloud for Quran Translation
#'
#' @description
#' Generates a word cloud from a Quran translation. Tokenization and
#' preprocessing are controlled via a \code{QuranConfig} object. The resulting
#' plot is rendered to the active R graphics device.
#'
#' @param tanzil_trans_object A \code{translationList} object created with
#'   \code{tanzil_translation()}.
#' @param config A \code{QuranConfig} object created with \code{quran_config()}.
#'   Defaults to \code{quran_config()} (English, unigram, by Surah, top 20).
#' @param selection Integer vector of Surah/Juz/Ayah IDs to include.
#'   \code{NULL} (default) means all.
#' @param max_word Integer. Maximum number of words in the word cloud. Default \code{200}.
#' @param min_freq Integer. Minimum frequency for a word to be included. Default \code{1}.
#'
#' @return Invisibly returns the word frequency data frame used to draw the cloud.
#'
#' @import dplyr
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
#'
#' @examples
#' # Default wordcloud for entire translation
#' # data(trans_en_sahih, package = "XploreQuran")
#' # wordcloud_trans(trans_en_sahih)
#'
#' # Wordcloud for Surah 114 only, Malay translation
#' # cfg <- quran_config(stopword_lang = "ms", normalize = FALSE)
#' # wordcloud_trans(trans_ms_basmeih, config = cfg, selection = 114, max_word = 50)
wordcloud_trans <- function(
    tanzil_trans_object,
    config    = quran_config(),
    selection = NULL,
    max_word  = 200,
    min_freq  = 1
) {
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"QuranConfig" %in% class(config)) {
    stop("config must be a QuranConfig object. Create one with quran_config().")
  }
  
  # Run preprocessing pipeline
  tokens <- preprocess_tokens(tanzil_trans_object, config, selection)
  
  # Count word frequency (across all selected groups)
  word_freq <- tokens %>%
    count(word, sort = TRUE)
  
  # Render the wordcloud
  wordcloud::wordcloud(
    words         = word_freq$word,
    freq          = word_freq$n,
    max.words     = max_word,
    min.freq      = min_freq,
    random.order  = FALSE,
    rot.per       = 0.1,
    colors        = brewer.pal(8, "Dark2")
  )
  
  invisible(word_freq)
}
