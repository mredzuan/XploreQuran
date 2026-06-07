library(testthat)
library(XploreQuran)

# Create a mock translation list for local testing without internet
create_mock_translation <- function() {
  text_df <- data.frame(
    surah_id = c(1, 1, 114, 114),
    ayah_id = c(1, 2, 1, 2),
    juz_id = c(1, 1, 30, 30),
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

test_that("tanzil_translation downloads and parses correctly", {
  # Skip if offline
  if (class(try(curl::nslookup("tanzil.net"), silent = TRUE)) == "try-error") {
    skip("Offline - skipping Tanzil download test")
  }
  
  # Test with a real URL
  res <- tanzil_translation("https://tanzil.net/trans/en.qarai")
  expect_s3_class(res, "translationList")
  expect_equal(nrow(res$translation_text), 6236)
  expect_true("juz_id" %in% names(res$translation_text))
})

test_that("tf_trans calculates term frequencies correctly", {
  mock_data <- create_mock_translation()
  
  # Calculate TF for Al-Fatihah (surah 1)
  tf_results <- tf_trans(mock_data, surah_number = 1)
  
  expect_s3_class(tf_results, "data.frame")
  expect_true("tf" %in% names(tf_results))
  expect_true(all(tf_results$surah_id == 1))
  
  # Total words in Surah 1 = 11 + 9 = 20 words
  # Check if "allah" is counted (it appears twice)
  allah_row <- tf_results[tf_results$word == "allah", ]
  expect_equal(sum(allah_row$n), 2)
})

test_that("tf_trans throws error on invalid inputs", {
  expect_error(tf_trans(list()))
  expect_error(tf_trans(create_mock_translation(), surah_number = 150))
})

test_that("wordcloud_trans executes without error", {
  mock_data <- create_mock_translation()
  
  # wordcloud usually plots to device. We check if it completes without crashing.
  # We wrap in pdf(NULL) to prevent opening an actual window on the user's PC.
  pdf(NULL)
  on.exit(dev.off())
  
  expect_silent(
    wordcloud_trans(mock_data, surah_number = 114, max_word = 5)
  )
})

test_that("wordcloud_trans throws error on invalid inputs", {
  expect_error(wordcloud_trans(list()))
  expect_error(wordcloud_trans(create_mock_translation(), surah_number = 999))
})
