#' Compute Term Frequency and TF-IDF for Quran Translation
#'
#' @description
#' Computes word or n-gram frequency, raw term frequency (TF), and
#' TF-IDF scores for a Quran translation. Grouping, selection, and
#' preprocessing are fully controlled via a \code{TransAnalyticConfig} object,
#' which acts as the complete analysis context.
#'
#' @param tanzil_trans_object A \code{translationList} object created with
#'   \code{tanzil_translation()}.
#' @param config A \code{TransAnalyticConfig} object created with
#'   \code{trans_analytic_config()}. Defaults to \code{trans_analytic_config()}
#'   (all surahs, English, unigram, top 20).
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
#' # data(trans_en_sahih, package = "XploreQuran")
#'
#' # Default: all surahs, top 20 English unigrams
#' # tf_trans(trans_en_sahih)
#'
#' # Juz 30 only, bigrams
#' # cfg <- trans_analytic_config(by = "juz", sub_by = 30, ngram = "bigram", top_n = 15)
#' # tf_trans(trans_en_sahih, config = cfg)
#'
#' # Surahs 1 and 114 only
#' # cfg2 <- trans_analytic_config(by = "surah", sub_by = c(1, 114))
#' # tf_trans(trans_en_sahih, config = cfg2)
tf_trans <- function(
    tanzil_trans_object,
    config = trans_analytic_config()
) {
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"TransAnalyticConfig" %in% class(config)) {
    stop("config must be a TransAnalyticConfig object. Create one with trans_analytic_config().")
  }

  # Resolve grouping column
  group_col <- switch(config$by,
    "surah" = "surah_id",
    "juz"   = "juz",
    "ayah"  = "ayah_id"
  )

  # Run preprocessing pipeline (sub_by filtering is handled inside)
  tokens <- preprocess_tokens(tanzil_trans_object, config)

  # Count words per group
  count_words <- tokens %>%
    count(.data[[group_col]], word, sort = TRUE)

  # Compute group totals
  total_words <- count_words %>%
    group_by(.data[[group_col]]) %>%
    summarize(total = sum(n), .groups = "drop")

  # Join, compute TF and TF-IDF, apply top_n per group
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
