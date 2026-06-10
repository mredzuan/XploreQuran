# ==============================================================================
# global.R
# Quran Translation Explorer - Global Setup
# Loaded once at startup. Shared across all sessions.
# ==============================================================================

# --- Libraries ----------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stopwords)
library(purrr)
library(rlang)
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

# --- Helper: Load Built-in Translation Dataset --------------------------------
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

# --- Helper: Load Custom Translation from Tanzil URL -------------------------
#' Download and validate a custom translation from tanzil.net.
#'
#' Language is auto-detected from the URL prefix (e.g. "ms.", "id.", "ur.").
#' If the language prefix is not recognised, the app defaults to English ("en")
#' for stop word removal and sets lang_defaulted = TRUE so the server can
#' alert the user with a Shiny notification.
#'
#' @param url   Full tanzil.net URL (e.g. "https://tanzil.net/trans/ur.jawadi").
#' @param name  Display name the user assigned (e.g. "Urdu - Jawadi").
#'
#' @return A named list:
#'   \item{data}{Translation tibble (same structure as built-in datasets).}
#'   \item{lang}{Detected ISO 639-1 code used for stop word removal.}
#'   \item{name}{Display name.}
#'   \item{key}{Safe R identifier (used as reactiveVal list key).}
#'   \item{lang_defaulted}{TRUE if language fell back to English.}
#'
load_custom_translation <- function(url, name) {

  url  <- trimws(url)
  name <- trimws(name)

  if (nchar(url) == 0)  stop("URL is empty.")
  if (nchar(name) == 0) stop("Display name is empty.")

  if (!grepl("^https?://tanzil\\.net/trans/", url, ignore.case = TRUE)) {
    stop(paste0(
      "URL must be a tanzil.net translation link ",
      "(e.g. https://tanzil.net/trans/ms.basmeih)."
    ))
  }

  # --- Auto-detect language from URL slug -------------------------------------
  # URL format: https://tanzil.net/trans/{lang_code}.{translator_name}
  slug        <- sub(".*tanzil\\.net/trans/", "", url, ignore.case = TRUE)
  lang_prefix <- sub("\\..*$", "", slug)   # e.g. "ms", "ur", "fa"

  known_langs <- c(
    "en", "ms", "id", "ar", "fr", "de", "es", "pt", "tr",
    "ur", "fa", "ru", "zh", "nl", "ko", "ja", "bn", "ha",
    "so", "sq", "az", "bs", "cs", "fi", "gl", "it", "nl",
    "pl", "ro", "sk", "sv", "sw", "tg", "tt", "uz"
  )

  lang_defaulted <- FALSE
  if (!lang_prefix %in% known_langs) {
    lang_prefix    <- "en"
    lang_defaulted <- TRUE
  }

  # --- Download from tanzil.net -----------------------------------------------
  trans_list <- tryCatch(
    XploreQuran::tanzil_translation(url),
    error = function(e) {
      stop(sprintf("Download failed for '%s': %s", url, conditionMessage(e)))
    }
  )

  trans_df <- if (inherits(trans_list, "translationList")) {
    trans_list$translation_text
  } else {
    trans_list
  }

  # --- Build a safe R identifier key ------------------------------------------
  safe_key <- paste0("custom_", gsub("[^a-zA-Z0-9]", "_", tolower(name)))

  list(
    data           = trans_df,
    lang           = lang_prefix,
    name           = name,
    key            = safe_key,
    lang_defaulted = lang_defaulted
  )
}

# --- In-memory store for custom translations (placeholder) -------------------
# The actual live store is a reactiveVal() defined inside server() in app.R.
CUSTOM_TRANS_STORE <- list()
