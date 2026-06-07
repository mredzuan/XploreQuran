#' Create a Translation Analytic Configuration Object
#'
#' @description
#' Creates a \code{TransAnalyticConfig} S3 object that bundles all text
#' preprocessing, grouping, selection, and analysis parameters. This config
#' object is the complete context passed to all analytic functions such as
#' \code{tf_trans()} and \code{wordcloud_trans()}, ensuring consistency and
#' reusability across all functions in the XploreQuran package.
#'
#' @param by Character. The dimension to group analysis by. One of:
#'   \code{"surah"} (default), \code{"juz"}, or \code{"ayah"}.
#' @param sub_by Integer vector. Subset selection within the chosen \code{by}
#'   dimension. Validated ranges are:
#'   \itemize{
#'     \item \code{"surah"}: values must be within \code{1–114}
#'     \item \code{"juz"}: values must be within \code{1–30}
#'     \item \code{"ayah"}: values must be within \code{1–6236}
#'   }
#'   Use \code{NULL} (default) to include all available data.
#' @param ngram Character. The type of token to use. One of:
#'   \code{"unigram"} (default), \code{"bigram"}, or \code{"trigram"}.
#' @param top_n Integer. Number of top terms to return by frequency. Default \code{20}.
#' @param remove_stopwords Logical. Whether to remove stop words. Default \code{TRUE}.
#' @param stopword_lang Character. Language code for stop word removal. ISO 639-1
#'   codes preferred (e.g. \code{"en"} for English, \code{"ms"} for Malay,
#'   \code{"id"} for Indonesian). Common full language names (e.g.
#'   \code{"english"}, \code{"malay"}, \code{"indonesian"}) are also accepted
#'   and will be auto-mapped to ISO codes. Default \code{"en"}.
#' @param normalize Logical. Whether to apply word stemming via SnowballC.
#'   Default \code{TRUE}. Automatically skipped for languages not supported by
#'   SnowballC (e.g. Malay \code{"ms"}), with a message to the user.
#' @param remove_special Logical. Whether to strip punctuation and numbers.
#'   Default \code{TRUE}.
#' @param remove_words Character vector. Additional custom words to remove.
#'   Default \code{NULL}.
#'
#' @return An S3 object of class \code{TransAnalyticConfig}.
#' @aliases TransAnalyticConfig
#'
#' @export
#'
#' @examples
#' # Default config - all surahs, English, unigram
#' cfg <- trans_analytic_config()
#'
#' # Analyse Juz 30 only
#' cfg_juz30 <- trans_analytic_config(by = "juz", sub_by = 30)
#'
#' # Analyse multiple surahs with bigrams, Malay translation
#' cfg_ms <- trans_analytic_config(
#'   by               = "surah",
#'   sub_by           = c(1, 2, 36),
#'   ngram            = "bigram",
#'   top_n            = 10,
#'   stopword_lang    = "ms",
#'   normalize        = FALSE,
#'   remove_words     = c("dan", "yang")
#' )
trans_analytic_config <- function(
    by               = "surah",
    sub_by           = NULL,
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
    "top_n must be a positive integer"             = is.numeric(top_n) && top_n > 0,
    "remove_stopwords must be logical"              = is.logical(remove_stopwords),
    "normalize must be logical"                     = is.logical(normalize),
    "remove_special must be logical"                = is.logical(remove_special),
    "remove_words must be NULL or character vector" = is.null(remove_words) || is.character(remove_words)
  )

  # --- Validate sub_by ---
  if (!is.null(sub_by)) {
    sub_by <- as.integer(sub_by)

    # Enforce that sub_by values are within valid range for chosen 'by' dimension
    limits <- list(
      surah = c(1L,   114L),
      juz   = c(1L,   30L),
      ayah  = c(1L,   6236L)
    )
    lim <- limits[[by]]
    invalid <- sub_by[sub_by < lim[1] | sub_by > lim[2]]

    if (length(invalid) > 0) {
      stop(sprintf(
        "sub_by value(s) out of range for by = '%s' (valid: %d to %d): %s",
        by, lim[1], lim[2], paste(invalid, collapse = ", ")
      ))
    }
  }

  # --- Language name → ISO 639-1 code mapping ---
  lang_name_to_iso <- c(
    "english"    = "en", "malay"      = "ms", "indonesian" = "id",
    "arabic"     = "ar", "french"     = "fr", "german"     = "de",
    "spanish"    = "es", "portuguese" = "pt", "russian"    = "ru",
    "chinese"    = "zh", "japanese"   = "ja", "korean"     = "ko",
    "turkish"    = "tr", "dutch"      = "nl"
  )
  if (stopword_lang %in% names(lang_name_to_iso)) {
    stopword_lang <- unname(lang_name_to_iso[[stopword_lang]])
  }

  # --- Validate stopword language ---
  supported_langs <- c(
    stopwords::stopwords_getlanguages("stopwords-iso"),
    stopwords::stopwords_getlanguages("snowball")
  )
  if (remove_stopwords && !stopword_lang %in% supported_langs) {
    warning(sprintf(
      "Language '%s' is not found in the stopwords package. Stop word removal will be skipped. Use stopwords::stopwords_getlanguages() to see supported codes.",
      stopword_lang
    ))
  }

  # --- Check normalization (stemming) support ---
  snowball_langs <- SnowballC::getStemLanguages()
  lang_map <- c(
    "en" = "english", "id" = "indonesian", "ms" = "malay",
    "fr" = "french",  "de" = "german",     "ar" = "arabic"
  )
  resolved_stem_lang <- if (stopword_lang %in% names(lang_map)) lang_map[[stopword_lang]] else stopword_lang
  can_stem <- resolved_stem_lang %in% snowball_langs

  if (normalize && !can_stem) {
    message(sprintf(
      "[XploreQuran] Note: Word stemming (normalization) is not supported for language '%s'. Normalization will be skipped. Set normalize = FALSE to suppress this message.",
      stopword_lang
    ))
    normalize <- FALSE
  }

  # --- Build config object ---
  config <- structure(
    list(
      by               = by,
      sub_by           = sub_by,
      ngram            = ngram,
      top_n            = as.integer(top_n),
      remove_stopwords = remove_stopwords,
      stopword_lang    = stopword_lang,
      normalize        = normalize,
      remove_special   = remove_special,
      remove_words     = remove_words,
      .stem_lang       = if (can_stem) resolved_stem_lang else NULL
    ),
    class = "TransAnalyticConfig"
  )

  return(config)
}


#' Print a TransAnalyticConfig object
#'
#' @param x A \code{TransAnalyticConfig} object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.TransAnalyticConfig <- function(x, ...) {
  sub_by_str <- if (is.null(x$sub_by)) "(all)" else paste(x$sub_by, collapse = ", ")
  cat("== XploreQuran Translation Analytic Config ==\n")
  cat(sprintf("  Group by          : %s\n", x$by))
  cat(sprintf("  Selection (sub_by): %s\n", sub_by_str))
  cat(sprintf("  N-gram            : %s\n", x$ngram))
  cat(sprintf("  Top N terms       : %d\n", x$top_n))
  cat(sprintf("  Remove stopwords  : %s (language: %s)\n", x$remove_stopwords, x$stopword_lang))
  cat(sprintf("  Normalize (stem)  : %s\n", x$normalize))
  cat(sprintf("  Remove special    : %s\n", x$remove_special))
  if (!is.null(x$remove_words)) {
    cat(sprintf("  Remove words      : %s\n", paste(x$remove_words, collapse = ", ")))
  } else {
    cat("  Remove words      : (none)\n")
  }
  invisible(x)
}
