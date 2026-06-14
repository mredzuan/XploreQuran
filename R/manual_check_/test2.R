#Load library
library(XploreQuran)

#Pull transalation--------------
trans_ms_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
head(trans_ms_basmeih$translation_text)


#Use package default/available dataset 
trans_en_sahih <- trans_en_sahih


# Create a Quran Analysis Configuration Object---------------

cfg <- trans_analytic_config()
cfg


cfg_juz30 <- trans_analytic_config(by = "juz", sub_by = 30)

cfg_multiple_juz <- trans_analytic_config(by = "juz", sub_by = c(28, 29, 30))


cfg_ms <- trans_analytic_config(
  by               = "surah",
  sub_by           = c(1, 2, 36),
  ngram            = "unigram",
  top_n            = 10,
  stopword_lang    = "ms",
  normalize        = FALSE,
  remove_word = c("aku")
)



# Test term frequency and TF-IDF-----------

term_freq_shahih <- tf_trans(tanzil_trans_object = trans_en_sahih, config = cfg)
tf_trans(trans_en_sahih, cfg_juz30)
tf_trans(trans_en_sahih, trans_analytic_config(by = "juz", sub_by = 30, ngram = "bigram"))

tf_multipleJuz <- tf_trans(trans_en_sahih, cfg_multiple_juz)
tf_malay <- tf_trans(trans_ms_basmeih, cfg_ms)






# Test WOrd cloud function-----------

wordcloud_trans(trans_en_sahih, cfg)