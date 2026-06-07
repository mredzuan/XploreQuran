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
# trans_analytic_config() tests
# =========================================================================
test_that("trans_analytic_config() creates a valid TransAnalyticConfig object", {
  cfg <- trans_analytic_config()
  expect_s3_class(cfg, "TransAnalyticConfig")
  expect_equal(cfg$by, "surah")
  expect_null(cfg$sub_by)
  expect_equal(cfg$ngram, "unigram")
  expect_equal(cfg$top_n, 20L)
  expect_true(cfg$remove_stopwords)
  expect_equal(cfg$stopword_lang, "en")
  expect_true(cfg$normalize)
  expect_true(cfg$remove_special)
  expect_null(cfg$remove_words)
})

test_that("trans_analytic_config() accepts valid sub_by for surah", {
  cfg <- trans_analytic_config(by = "surah", sub_by = c(1, 57, 114))
  expect_equal(cfg$sub_by, c(1L, 57L, 114L))
})

test_that("trans_analytic_config() accepts valid sub_by for juz", {
  cfg <- trans_analytic_config(by = "juz", sub_by = c(1, 15, 30))
  expect_equal(cfg$sub_by, c(1L, 15L, 30L))
})

test_that("trans_analytic_config() accepts valid sub_by for ayah", {
  cfg <- trans_analytic_config(by = "ayah", sub_by = c(1, 6236))
  expect_equal(cfg$sub_by, c(1L, 6236L))
})

test_that("trans_analytic_config() rejects out-of-range sub_by for surah", {
  expect_error(trans_analytic_config(by = "surah", sub_by = 115))
  expect_error(trans_analytic_config(by = "surah", sub_by = 0))
})

test_that("trans_analytic_config() rejects out-of-range sub_by for juz", {
  expect_error(trans_analytic_config(by = "juz", sub_by = 31))
  expect_error(trans_analytic_config(by = "juz", sub_by = c(1, 31)))
})

test_that("trans_analytic_config() rejects out-of-range sub_by for ayah", {
  expect_error(trans_analytic_config(by = "ayah", sub_by = 6237))
})

test_that("trans_analytic_config() accepts sub_by = NULL (all data)", {
  cfg <- trans_analytic_config(sub_by = NULL)
  expect_null(cfg$sub_by)
})

test_that("trans_analytic_config() accepts custom values", {
  cfg <- trans_analytic_config(
    by = "juz", sub_by = 30, ngram = "bigram", top_n = 10,
    stopword_lang = "ms", normalize = FALSE, remove_words = c("allah")
  )
  expect_equal(cfg$by, "juz")
  expect_equal(cfg$sub_by, 30L)
  expect_equal(cfg$ngram, "bigram")
  expect_equal(cfg$top_n, 10L)
  expect_equal(cfg$stopword_lang, "ms")
  expect_false(cfg$normalize)
  expect_equal(cfg$remove_words, "allah")
})

test_that("trans_analytic_config() rejects invalid by argument", {
  expect_error(trans_analytic_config(by = "chapter"))
})

test_that("trans_analytic_config() rejects invalid ngram argument", {
  expect_error(trans_analytic_config(ngram = "fourgram"))
})

test_that("trans_analytic_config() rejects invalid top_n", {
  expect_error(trans_analytic_config(top_n = -5))
})

test_that("trans_analytic_config() warns for unsupported stopword language", {
  expect_warning(trans_analytic_config(stopword_lang = "zz"))
})

test_that("trans_analytic_config() accepts 'english' as full language name", {
  expect_no_warning(trans_analytic_config(stopword_lang = "english"))
})

test_that("trans_analytic_config() messages user when normalize is skipped for Malay", {
  expect_message(trans_analytic_config(normalize = TRUE, stopword_lang = "ms"))
})

test_that("print.TransAnalyticConfig() runs without error", {
  cfg <- trans_analytic_config()
  expect_output(print(cfg), "XploreQuran Translation Analytic Config")
})

test_that("print.TransAnalyticConfig() shows sub_by selection in output", {
  cfg <- trans_analytic_config(by = "juz", sub_by = c(29, 30))
  expect_output(print(cfg), "29, 30")
})

test_that("print.TransAnalyticConfig() shows '(all)' when sub_by is NULL", {
  cfg <- trans_analytic_config()
  expect_output(print(cfg), "\\(all\\)")
})

# =========================================================================
# tf_trans() tests
# =========================================================================
test_that("tf_trans() returns a data frame with expected columns", {
  mock <- create_mock_translation()
  cfg  <- trans_analytic_config(remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("word", "n", "total", "tf", "tf_idf") %in% names(result)))
})

test_that("tf_trans() respects top_n limit", {
  mock <- create_mock_translation()
  cfg  <- trans_analytic_config(top_n = 3, remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
  expect_true(all(table(result$surah_id) <= 3))
})

test_that("tf_trans() filters by surah using sub_by in config", {
  mock <- create_mock_translation()
  cfg  <- trans_analytic_config(by = "surah", sub_by = 1,
                                remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
  expect_true(all(result$surah_id == 1))
})

test_that("tf_trans() filters by juz using sub_by in config", {
  mock <- create_mock_translation()
  cfg  <- trans_analytic_config(by = "juz", sub_by = 30,
                                remove_stopwords = FALSE, normalize = FALSE)
  result <- tf_trans(mock, config = cfg)
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
  cfg  <- trans_analytic_config(by = "surah", sub_by = 114,
                                remove_stopwords = FALSE, normalize = FALSE)
  pdf(NULL)
  on.exit(dev.off())
  result <- wordcloud_trans(mock, config = cfg)
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
