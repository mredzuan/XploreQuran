library(testthat)
library(XploreQuran)

# =========================================================================
# Mock translation helper
# =========================================================================
create_mock_translation <- function() {
  text_df <- data.frame(
    ayah_id = 1:4,
    surah_id = c(1, 1, 114, 114),
    surah_ayah_id = c(1, 2, 1, 2),
    juz = c(1, 1, 30, 30),
    surah_title_ar = c("الفاتحة", "الفاتحة", "الناس", "الناس"),
    surah_title_en = c("Al-Fatihah", "Al-Fatihah", "An-Nas", "An-Nas"),
    surah_title_en_trans = c("The Opening", "The Opening", "Mankind", "Mankind"),
    revelation_type = c("Meccan", "Meccan", "Meccan", "Meccan"),
    translation = c(
      "In the name of Allah the Entirely Merciful the Especially Merciful",
      "All praise is due to Allah Lord of the worlds",
      "Say I seek refuge in the Lord of mankind",
      "The Sovereign of mankind"
    ),
    stringsAsFactors = FALSE
  )

  info_df <- data.frame(
    info = c("Language", "Translator"),
    value = c("English", "Mock Translator"),
    stringsAsFactors = FALSE
  )

  trans_list <- list("translation_text" = text_df, "translation_info" = info_df)
  class(trans_list) <- append("translationList", class(trans_list))
  attr(trans_list, "trans_indicator") <- "Mock-Translator"
  return(trans_list)
}

# =========================================================================
# quran_config() tests
# =========================================================================
test_that("quran_config() creates a valid QuranConfig object", {
  cfg <- quran_config()
  expect_s3_class(cfg, "QuranConfig")
  expect_equal(cfg$by, "surah")
  expect_equal(cfg$ngram, "unigram")
  expect_equal(cfg$top_n, 20L)
  expect_true(cfg$remove_stopwords)
  expect_equal(cfg$stopword_lang, "en")
  expect_true(cfg$normalize)
  expect_true(cfg$remove_special)
  expect_null(cfg$remove_words)
})

test_that("quran_config() accepts custom values", {
  cfg <- quran_config(
    by = "juz", ngram = "bigram", top_n = 10,
    stopword_lang = "ms", normalize = FALSE,
    remove_words = c("allah")
  )
  expect_equal(cfg$by, "juz")
  expect_equal(cfg$ngram, "bigram")
  expect_equal(cfg$top_n, 10L)
  expect_equal(cfg$stopword_lang, "ms")
  expect_false(cfg$normalize)
  expect_equal(cfg$remove_words, "allah")
})

test_that("quran_config() rejects invalid by argument", {
  expect_error(quran_config(by = "chapter"))
})

test_that("quran_config() rejects invalid ngram argument", {
  expect_error(quran_config(ngram = "fourgram"))
})

test_that("quran_config() rejects invalid top_n", {
  expect_error(quran_config(top_n = -5))
})

test_that("quran_config() warns for unsupported stopword language", {
  expect_warning(quran_config(stopword_lang = "zz"))
})

test_that("quran_config() accepts 'english' as snowball language name", {
  # 'english' is valid via snowball source — should produce no warning
  expect_no_warning(quran_config(stopword_lang = "english"))
})

test_that("quran_config() messages user when normalize is skipped for Malay", {
  expect_message(quran_config(normalize = TRUE, stopword_lang = "ms"))
})

test_that("print.QuranConfig() runs without error", {
  cfg <- quran_config()
  expect_output(print(cfg), "XploreQuran Analysis Configuration")
})

# =========================================================================
# tf_trans() tests
# =========================================================================
test_that("tf_trans() returns a data frame with expected columns", {
  mock <- create_mock_translation()
  cfg <- quran_config(remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("word", "n", "total", "tf", "tf_idf") %in% names(result)))
})

test_that("tf_trans() respects top_n limit", {
  mock <- create_mock_translation()
  cfg <- quran_config(top_n = 3, remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
  expect_true(all(table(result$surah_id) <= 3))
})

test_that("tf_trans() filters by surah selection", {
  mock <- create_mock_translation()
  cfg <- quran_config(remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg, selection = 1)
  expect_true(all(result$surah_id == 1))
})

test_that("tf_trans() filters by juz selection", {
  mock <- create_mock_translation()
  cfg <- quran_config(by = "juz", remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg, selection = 30)
  expect_true(all(result$juz == 30))
})

test_that("tf_trans() errors on invalid input object", {
  expect_error(tf_trans(list()))
  expect_error(tf_trans(create_mock_translation(), config = list()))
})

# =========================================================================
# wordcloud_trans() tests
# =========================================================================
test_that("wordcloud_trans() returns invisible word freq data frame", {
  mock <- create_mock_translation()
  cfg <- quran_config(remove_stopwords = FALSE, normalize = FALSE)
  pdf(NULL)
  on.exit(dev.off())
  result <- wordcloud_trans(mock, config = cfg, selection = 114)
  expect_s3_class(result, "data.frame")
  expect_true("word" %in% names(result))
  expect_true("n" %in% names(result))
})

test_that("wordcloud_trans() errors on invalid input object", {
  expect_error(wordcloud_trans(list()))
  expect_error(wordcloud_trans(create_mock_translation(), config = list()))
})

# =========================================================================
# tanzil_translation() integration test (requires internet)
# =========================================================================
test_that("tanzil_translation downloads and parses correctly", {
  if (class(try(curl::nslookup("tanzil.net"), silent = TRUE)) == "try-error") {
    skip("Offline - skipping Tanzil download test")
  }
  res <- tanzil_translation("https://tanzil.net/trans/en.qarai")
  expect_s3_class(res, "translationList")
  expect_equal(nrow(res$translation_text), 6236)
  expect_true("juz" %in% names(res$translation_text))
})
