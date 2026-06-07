#Load library
library(XploreQuran)

#Pull transalation--------------
trans_ms_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
head(trans_ms_basmeih$translation_text)


#Use package default/available dataset 
trans_en_sahih <- trans_en_sahih


# Create a Quran Analysis Configuration Object---------------
cfg_surah <- quran_config(
  by = "surah",
  ngram = "unigram",
  top_n = 20L,
  remove_stopwords = TRUE,
  stopword_lang = "en",
  normalize = TRUE,
  remove_special = TRUE,
  remove_words = NULL
)

cfg_juz <- quran_config(
  by = "juz",
  ngram = "unigram",
  top_n = 20L,
  remove_stopwords = TRUE,
  stopword_lang = "en",
  normalize = TRUE,
  remove_special = TRUE,
  remove_words = NULL
)

print(cfg)


# Compute Term Frequency and TF-IDF for Quran Translation-------------

tf_surah <- tf_trans(trans_en_sahih, config = cfg_surah)
tf_juz <- tf_trans(trans_en_sahih, config = cfg_juz)