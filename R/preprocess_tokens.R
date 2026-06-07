#' Preprocess and Tokenize Translation Text (Internal)
#'
#' @param tanzil_trans_object A translationList object from tanzil_translation().
#' @param config A QuranConfig object from quran_config().
#' @param selection Integer vector of IDs to filter by. NULL means all.
#' @return A tidy data frame of tokens.
#'
#' @importFrom dplyr filter mutate select anti_join
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_replace_all str_squish
#' @importFrom SnowballC wordStem
#' @importFrom stopwords stopwords stopwords_getlanguages
#' @noRd
preprocess_tokens <- function(tanzil_trans_object, config, selection = NULL) {
  
  # --- Validate inputs ---
  if (!"translationList" %in% class(tanzil_trans_object)) {
    stop("Input must be a translationList object created with tanzil_translation().")
  }
  if (!"QuranConfig" %in% class(config)) {
    stop("Config must be a QuranConfig object created with quran_config().")
  }
  
  text_df <- tanzil_trans_object[["translation_text"]]
  
  # --- Determine grouping column ---
  group_col <- switch(config$by,
    "surah" = "surah_id",
    "juz"   = "juz",
    "ayah"  = "ayah_id"
  )
  
  # Validate that the group column exists in the data
  if (!group_col %in% names(text_df)) {
    stop(sprintf("Column '%s' not found in translation text. Check your data.", group_col))
  }
  
  # --- Filter by selection ---
  if (!is.null(selection)) {
    selection <- as.integer(selection)
    text_df <- text_df %>% filter(.data[[group_col]] %in% selection)
    if (nrow(text_df) == 0) {
      stop(sprintf("No rows found for %s selection: %s", config$by, paste(selection, collapse = ", ")))
    }
  }
  
  # --- Special character removal ---
  if (config$remove_special) {
    text_df <- text_df %>%
      mutate(translation = str_replace_all(translation, "[^[:alpha:][:space:]]", " ")) %>%
      mutate(translation = str_squish(translation))
  }
  
  # --- Tokenization (unigram, bigram, trigram) ---
  ngram_n <- switch(config$ngram,
    "unigram" = 1L,
    "bigram"  = 2L,
    "trigram" = 3L
  )
  
  if (ngram_n == 1L) {
    tokens <- text_df %>%
      unnest_tokens(word, translation, token = "words")
  } else {
    tokens <- text_df %>%
      unnest_tokens(word, translation, token = "ngrams", n = ngram_n)
  }
  
  # --- Stop word removal (only applicable for unigrams) ---
  if (config$remove_stopwords && ngram_n == 1L) {
    iso_langs    <- stopwords::stopwords_getlanguages("stopwords-iso")
    snowball_sw_langs <- stopwords::stopwords_getlanguages("snowball")
    lang <- config$stopword_lang
    
    # Determine which source to use
    sw_source <- if (lang %in% iso_langs) {
      "stopwords-iso"
    } else if (lang %in% snowball_sw_langs) {
      "snowball"
    } else {
      NULL
    }
    
    if (!is.null(sw_source)) {
      sw_list <- tryCatch(
        stopwords::stopwords(lang, source = sw_source),
        error = function(e) character(0)
      )
      if (length(sw_list) > 0) {
        sw_df <- data.frame(word = sw_list, stringsAsFactors = FALSE)
        tokens <- tokens %>% anti_join(sw_df, by = "word")
      }
    } else {
      message(sprintf(
        "[XploreQuran] Stop word removal skipped: language '%s' not supported.",
        lang
      ))
    }
  }
  
  # --- Remove custom words ---
  if (!is.null(config$remove_words) && length(config$remove_words) > 0 && ngram_n == 1L) {
    custom_sw_df <- data.frame(word = config$remove_words, stringsAsFactors = FALSE)
    tokens <- tokens %>% anti_join(custom_sw_df, by = "word")
  }
  
  # --- Word stemming / normalization ---
  if (config$normalize && !is.null(config$.stem_lang) && ngram_n == 1L) {
    tokens <- tokens %>%
      mutate(word = SnowballC::wordStem(word, language = config$.stem_lang))
  }
  
  # Remove any empty tokens that may result from preprocessing
  tokens <- tokens %>% filter(nchar(word) > 0)
  
  return(tokens)
}
