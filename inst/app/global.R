# ==============================================================================
# global.R
# XploreQuran Shiny App - Global Setup
# Loaded once at startup. Shared across all sessions.
# ==============================================================================

# --- Libraries ----------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stopwords)
library(quRan)
library(XploreQuran)

# --- Available Translations ---------------------------------------------------
# Named list: display label -> internal dataset name
TRANSLATIONS <- list(
  "English - Sahih International"  = "trans_en_sahih",
  "English - Yusuf Ali"            = "trans_en_yusufali",
  "English - Pickthall"            = "trans_en_pickthall",
  "Malay - Basmeih"                = "trans_ms_basmeih",
  "Indonesian - Kemenag"           = "trans_id_indonesian"
)

# --- Quran Index (Surah / Juz metadata) ---------------------------------------
data("quran_index", package = "XploreQuran", envir = .GlobalEnv)

SURAH_CHOICES <- setNames(
  quran_index$surah_id,
  paste0(quran_index$surah_id, ". ", quran_index$surah_title_en)
)

JUZ_CHOICES <- 1:30

# --- Helper: Load Translation Dataset ----------------------------------------
#' Load a translation tibble by its dataset name key.
#' Returns the `translation_text` tibble from the translationList object.
load_translation <- function(dataset_name) {
  env <- new.env(parent = emptyenv())
  data(list = dataset_name, package = "XploreQuran", envir = env)
  obj <- get(dataset_name, envir = env)

  # Support both translationList (list$translation_text) and bare tibbles
  if (inherits(obj, "translationList")) {
    return(obj$translation_text)
  }
  return(obj)
}

# --- Helper: Filter by Surah or Juz ------------------------------------------
#' Subset a translation tibble by the grouping dimension.
#' @param df       A translation tibble with columns surah_id, juz_id.
#' @param by       One of "surah" or "juz".
#' @param sub_by   Integer vector of IDs to keep; NULL = all.
filter_quran <- function(df, by = "surah", sub_by = NULL) {
  if (is.null(sub_by) || length(sub_by) == 0) return(df)

  col <- switch(by, surah = "surah_id", juz = "juz_id")

  if (!col %in% names(df)) {
    warning(sprintf("[XploreQuran] Column '%s' not found in dataset. Returning unfiltered data.", col))
    return(df)
  }

  dplyr::filter(df, .data[[col]] %in% as.integer(sub_by))
}

# --- Helper: Tokenise with stopword removal -----------------------------------
#' Tokenise a translation tibble to one-word-per-row.
#' @param df             A translation tibble (must have a `translation` column).
#' @param remove_sw      Logical. Remove stop words?
#' @param sw_lang        ISO 639-1 language code (e.g. "en", "ms", "id").
#' @param remove_words   Additional words to remove.
tokenise_translation <- function(df,
                                 remove_sw    = TRUE,
                                 sw_lang      = "en",
                                 remove_words = NULL) {

  text_col <- if ("translation" %in% names(df)) "translation" else "text"

  tokens <- df |>
    tidytext::unnest_tokens(word, !!rlang::sym(text_col)) |>
    dplyr::filter(!stringr::str_detect(word, "[0-9]"))

  if (remove_sw) {
    sw <- tryCatch(
      stopwords::stopwords(sw_lang, source = "stopwords-iso"),
      error = function(e) character(0)
    )
    tokens <- dplyr::filter(tokens, !word %in% sw)
  }

  if (!is.null(remove_words) && length(remove_words) > 0) {
    tokens <- dplyr::filter(tokens, !word %in% tolower(remove_words))
  }

  tokens
}
