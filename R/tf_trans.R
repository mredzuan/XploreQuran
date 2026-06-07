#' Compute Term Frequency and TF-IDF for Quran Translation
#'
#' @description
#' Computes word or n-gram frequency, raw term frequency (TF), and 
#' TF-IDF scores for a Quran translation. Grouping and preprocessing
#' are controlled via a \code{QuranConfig} object.
#'
#' @param tanzil_trans_object A \code{translationList} object created with
#'   \code{tanzil_translation()}.
#' @param config A \code{QuranConfig} object created with \code{quran_config()}.
#'   Defaults to \code{quran_config()} (English, unigram, by Surah, top 20).
#' @param selection Integer vector of Surah/Juz/Ayah IDs to include.
#'   \code{NULL} (default) means all.
#'
#' @return A data frame with columns: group identifier, \code{word}, \code{n},
#'   \code{total}, \code{tf}, \code{tf_idf}, sorted by descending \code{tf}.
#'
#' @import dplyr
#' @importFrom tidytext bind_tf_idf
#'
#' @export
#'
#' @examples
#' # Default config - top 20 words by surah
#' # data(trans_en_sahih, package = "XploreQuran")
#' # tf_trans(trans_en_sahih)
#'
#' # Analyse Juz 30 using bigrams, remove stopwords
#' # cfg <- quran_config(by = "juz", ngram = "bigram", top_n = 15)
#' # tf_trans(trans_en_sahih, config = cfg, selection = 30)
tf_trans <- function(
    tanzil_trans_object,
    config    = quran_config(),
    selection = NULL
) {
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"QuranConfig" %in% class(config)) {
    stop("config must be a QuranConfig object. Create one with quran_config().")
  }
  
  # Resolve grouping column
  group_col <- switch(config$by,
    "surah" = "surah_id",
    "juz"   = "juz",
    "ayah"  = "ayah_id"
  )
  
  # Run preprocessing pipeline
  tokens <- preprocess_tokens(tanzil_trans_object, config, selection)
  
  # Count words per group
  count_words <- tokens %>%
    count(.data[[group_col]], word, sort = TRUE)
  
  # Compute group totals
  total_words <- count_words %>%
    group_by(.data[[group_col]]) %>%
    summarize(total = sum(n), .groups = "drop")
  
  # Join, compute TF, and compute TF-IDF
  result <- count_words %>%
    left_join(total_words, by = group_col) %>%
    mutate(tf = n / total) %>%
    tidytext::bind_tf_idf(word, !!sym(group_col), n) %>%
    arrange(desc(tf)) %>%
    group_by(.data[[group_col]]) %>%
    slice_head(n = config$top_n) %>%
    ungroup()
  
  return(result)
}
