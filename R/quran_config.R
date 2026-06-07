#' Create a Quran Analysis Configuration Object
#'
#' @description
#' Creates a \code{QuranConfig} S3 object that bundles all text preprocessing
#' and analysis parameters. This config object is passed to analytic functions
#' such as \code{tf_trans()} and \code{wordcloud_trans()}, ensuring a consistent
#' and reusable input pattern across all functions in the XploreQuran package.
#'
#' @param by Character. The dimension to group analysis by. One of:
#'   \code{"surah"} (default), \code{"juz"}, or \code{"ayah"}.
#' @param ngram Character. The type of token to use. One of:
#'   \code{"unigram"} (default), \code{"bigram"}, or \code{"trigram"}.
#' @param top_n Integer. Number of top terms to return by frequency. Default \code{20}.
#' @param remove_stopwords Logical. Whether to remove stop words. Default \code{TRUE}.
#' @param stopword_lang Character. Language code for stop word removal (ISO 639-1
#'   codes preferred, e.g. \code{"en"} for English, \code{"ms"} for Malay,
#'   \code{"id"} for Indonesian). Full language names (e.g. \code{"english"})
#'   are also accepted if supported by the \code{snowball} source.
#'   Default \code{"en"}.
#' @param normalize Logical. Whether to apply word stemming via SnowballC. 
#'   Default \code{TRUE}. Automatically skipped for languages not supported by
#'   SnowballC (e.g. Malay \code{"ms"}), with a message to the user.
#' @param remove_special Logical. Whether to strip punctuation and numbers.
#'   Default \code{TRUE}.
#' @param remove_words Character vector. Additional custom words to remove.
#'   Default \code{NULL}.
#'
#' @return An S3 object of class \code{QuranConfig}.
#'
#' @export
#'
#' @examples
#' # Default English config
#' cfg <- quran_config()
#'
#' # Malay config with bigrams and no normalization
#' cfg_ms <- quran_config(
#'   by               = "juz",
#'   ngram            = "bigram",
#'   top_n            = 10,
#'   stopword_lang    = "ms",
#'   remove_stopwords = TRUE,
#'   normalize        = FALSE,
#'   remove_words     = c("allah", "said")
#' )
quran_config <- function(
    by               = "surah",
    ngram            = "unigram",
    top_n            = 20L,
    remove_stopwords = TRUE,
    stopword_lang    = "en",
    normalize        = TRUE,
    remove_special   = TRUE,
    remove_words     = NULL
) {
  # --- Input Validation ---
  by    <- match.arg(by,    choices = c("surah", "juz", "ayah"))
  ngram <- match.arg(ngram, choices = c("unigram", "bigram", "trigram"))
  
  stopifnot(
    "top_n must be a positive integer" = is.numeric(top_n) && top_n > 0,
    "remove_stopwords must be logical"  = is.logical(remove_stopwords),
    "normalize must be logical"         = is.logical(normalize),
    "remove_special must be logical"    = is.logical(remove_special),
    "remove_words must be NULL or character vector" =
      is.null(remove_words) || is.character(remove_words)
  )
  
  # Convenience: map common full language names to ISO 639-1 codes
  lang_name_to_iso <- c(
    "english"    = "en",
    "malay"      = "ms",
    "indonesian" = "id",
    "arabic"     = "ar",
    "french"     = "fr",
    "german"     = "de",
    "spanish"    = "es",
    "portuguese" = "pt",
    "russian"    = "ru",
    "chinese"    = "zh",
    "japanese"   = "ja",
    "korean"     = "ko",
    "turkish"    = "tr",
    "dutch"      = "nl"
  )
  if (stopword_lang %in% names(lang_name_to_iso)) {
    stopword_lang <- unname(lang_name_to_iso[[stopword_lang]])
  }
  
  # Validate that the stopword language is supported
  supported_langs <- c(
    stopwords::stopwords_getlanguages("stopwords-iso"),
    stopwords::stopwords_getlanguages("snowball")
  )
  if (remove_stopwords && !stopword_lang %in% supported_langs) {
    warning(
      sprintf(
        "Language '%s' is not found in the stopwords package. Stop word removal will be skipped. Use stopwords::stopwords_getlanguages() to see supported codes.",
        stopword_lang
      )
    )
  }
  
  # Check normalization support
  snowball_langs <- SnowballC::getStemLanguages()
  # Map ISO codes to Snowball language names where needed
  lang_map <- c("en" = "english", "id" = "indonesian", "ms" = "malay",
                "fr" = "french", "de" = "german", "ar" = "arabic")
  resolved_stem_lang <- if (stopword_lang %in% names(lang_map)) lang_map[[stopword_lang]] else stopword_lang
  
  can_stem <- resolved_stem_lang %in% snowball_langs
  
  if (normalize && !can_stem) {
    message(
      sprintf(
        "[XploreQuran] Note: Word stemming (normalization) is not supported for language '%s'. ",
        stopword_lang
      ),
      "Normalization will be skipped. You may set normalize = FALSE to suppress this message."
    )
    normalize <- FALSE
  }
  
  # Build the config object
  config <- structure(
    list(
      by               = by,
      ngram            = ngram,
      top_n            = as.integer(top_n),
      remove_stopwords = remove_stopwords,
      stopword_lang    = stopword_lang,
      normalize        = normalize,
      remove_special   = remove_special,
      remove_words     = remove_words,
      # Internal: resolved Snowball language name for stemming
      .stem_lang       = if (can_stem) resolved_stem_lang else NULL
    ),
    class = "QuranConfig"
  )
  
  return(config)
}


#' Print a QuranConfig object
#'
#' @param x A \code{QuranConfig} object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.QuranConfig <- function(x, ...) {
  cat("== XploreQuran Analysis Configuration ==\n")
  cat(sprintf("  Group by          : %s\n", x$by))
  cat(sprintf("  N-gram            : %s\n", x$ngram))
  cat(sprintf("  Top N terms       : %d\n", x$top_n))
  cat(sprintf("  Remove stopwords  : %s (language: %s)\n",
              x$remove_stopwords, x$stopword_lang))
  cat(sprintf("  Normalize (stem)  : %s\n", x$normalize))
  cat(sprintf("  Remove special    : %s\n", x$remove_special))
  if (!is.null(x$remove_words)) {
    cat(sprintf("  Remove words      : %s\n", paste(x$remove_words, collapse = ", ")))
  } else {
    cat("  Remove words      : (none)\n")
  }
  invisible(x)
}
