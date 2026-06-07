# Suppress R CMD check NOTE about undefined global variables used
# inside dplyr/tidytext pipelines (NSE - Non-Standard Evaluation)
utils::globalVariables(c(
  # Used in tanzil_translation() (pull_translation.r)
  ".", "quran_ar", "surah_id", "ayah_id", "surah_ayah_id", "X1", "info", "value",
  # Used in preprocess_tokens()
  "translation", "word",
  # Used in tf_trans()
  "total", "tf",
  # Used in wordcloud_trans()
  "n"
))
