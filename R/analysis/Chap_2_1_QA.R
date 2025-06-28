#Load library-----------

library(quRan)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(dplyr)
library(exploreQuran)
library(tm)

#wordcloud
library(wordcloud)
library(RColorBrewer)


#Load quran data----------
quran_ar <- quran_ar
qur_trans_sahih <- quran_en_sahih
qur_trans_ysf <- quran_en_yusufali
qur_trans_mal_bas <- trans_malay_basmeih$translation_text


#Unnest token-------


## Sahih-----
token_sahih <- quran_en_sahih |> 
  unnest_tokens(word, text)


## Yusof-----
token_yusof <- quran_en_yusufali |> 
  unnest_tokens(word, text)


## Malay Basmeih------

token_malay_bas <- qur_trans_mal_bas |> 
  unnest_tokens(word, translation)


#Word cloud------------
## Sahih -----
token_sahih |> count(word) |> 
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words = 200,
                 random.order = FALSE,
                 rot.per = 0.35,
                 colors = brewer.pal(8, "Dark2")))


## Yusof Ali-----
token_yusof |> count(word) |> 
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words = 200,
                 random.order = FALSE,
                 rot.per = 0.35,
                 colors = brewer.pal(8, "Dark2")))

## Malay Basmeih-----
token_malay_bas |> count(word) |> 
  with(wordcloud(words=word, 
                 freq=n, 
                 max.words = 200,
                 random.order = FALSE,
                 rot.per = 0.35,
                 colors = brewer.pal(8, "Dark2")))


#Term frequency------

## Sahih-----
wc_by_surah_sahih <-  token_sahih |> 
  count(surah_title_en, word, sort = TRUE)

sum_wc_by_surah_sahih <- wc_by_surah_sahih |>  
  group_by(surah_title_en) |> 
  summarize(total = sum(n))

term_freq_by_surah_sahih <- wc_by_surah_sahih |> 
  left_join(sum_wc_by_surah_sahih, by = "surah_title_en") |> 
  mutate(tf = n/total)


## Yusof Ali---------

wc_by_surah_yusof <-  token_yusof |> 
  count(surah_title_en, word, sort = TRUE)

sum_wc_by_surah_yusof <- wc_by_surah_yusof |>  
  group_by(surah_title_en) |> 
  summarize(total = sum(n))

term_freq_by_surah_yusof <- wc_by_surah_yusof |> 
  left_join(sum_wc_by_surah_yusof, by = "surah_title_en") |> 
  mutate(tf = n/total)


## Malay Basmeih---------
wc_by_surah_malay_bas <-  token_malay_bas |> 
  count(surah_title_en, word, sort = TRUE)

sum_wc_by_surah_malay_bes <- wc_by_surah_malay_bas |>  
  group_by(surah_title_en) |> 
  summarize(total = sum(n))

ter_freq_by_surah_malay_bas <- wc_by_surah_malay_bas |> 
  left_join(sum_wc_by_surah_malay_bes, by = "surah_title_en") |> 
  mutate(tf = n/total)

#Plotting tf-----------

last6_surahs = c("An-Naas", "Al-Falaq", "Al-Ikhlaas", "Al-Masad",
                 "An-Nasr", "Al-Kaafiroon")


surah_al_baqarah <- "Al-Baqara"

## Last 6 surah---------

### Sahih-----
term_freq_by_surah_sahih |> 
  filter(surah_title_en %in% last6_surahs) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
    geom_histogram() +
    facet_wrap(~surah_title_en, ncol = 2) +
    theme_bw() + 
    ggtitle("Sahih Translation")


### Yusof ------

term_freq_by_surah_yusof |> 
  filter(surah_title_en %in% last6_surahs) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
  geom_histogram() +
  facet_wrap(~surah_title_en, ncol = 2) +
  theme_bw() +
  ggtitle("Yusof Ali Translation")


### Malay Basmeih------
ter_freq_by_surah_malay_bas |> 
  filter(surah_title_en %in% last6_surahs) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
  geom_histogram() +
  facet_wrap(~surah_title_en, ncol = 2) +
  theme_bw() +
  ggtitle("Malay Basmeih Translation")


## Surah Al-Baqarah-------

### Sahih-------
term_freq_by_surah_sahih |> 
  filter(surah_title_en == surah_al_baqarah) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
  geom_histogram() +
  theme_bw() + 
  ggtitle("Sahih Translation - Surah Al-Baqarah")


### Malay Basmeih-------
ter_freq_by_surah_malay_bas |> 
  filter(surah_title_en == surah_al_baqarah) |> 
  ggplot(aes(tf, fill = surah_title_en)) + 
  geom_histogram() +
  theme_bw() + 
  ggtitle("Malay Basmeih Translation - Surah Al-Baqarah")



#Unique words % per surah---------------
## Sahih-------
unique_words_percent_sahih <- token_sahih |> 
  group_by(surah_id, surah_title_en) |> 
  summarize(total = n_distinct(word)) |> 
  ungroup() |>
  mutate(percent = total/sum(total) * 100) |> 
  mutate(translation = "Sahih")

## Yusof Ali-------
unique_words_percent_yusof <- token_yusof |> 
  group_by(surah_id, surah_title_en) |> 
  summarize(total = n_distinct(word)) |> 
  ungroup() |>
  mutate(percent = total/sum(total) * 100) |> 
  mutate(translation = "Yusof Ali")


## Malay Basmeih-------
unique_words_percent_malay_bas <- token_malay_bas |> 
  group_by(surah_no, surah_title_en) |> 
  summarize(total = n_distinct(word)) |> 
  ungroup() |>
  mutate(percent = total/sum(total) * 100) |> 
  rename(surah_id = surah_no) |> 
  mutate(translation = "Malay Basmeih")

#Combine all translations
unique_words_percent_all <- bind_rows(unique_words_percent_sahih, 
                                   unique_words_percent_yusof, 
                                   unique_words_percent_malay_bas)

#Scatter plot percentage vs surah id
unique_words_percent_all |> 
  ggplot(aes(surah_id, percent, color = translation)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Surah ID", y = "Unique Words Percentage (%)") +
  ggtitle("Unique Words Percentage by Surah ID for Different Translations") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")



# tf-idf----------

# Term frequency example---------
docs <- c(
  "data science is fun",
  "machine learning is part of data science",
  "deep learning advances science"
)

# Create corpus and convert to tidy format
corpus <- VCorpus(VectorSource(docs))

# Convert to a tidy dataframe
tidy_docs <- tidy(corpus)

# Add document id
tidy_docs <- tidy_docs %>% 
  mutate(document = row_number())

# Tokenize words
tokens <- tidy_docs %>%
  unnest_tokens(word, text)


term_freq <- tokens %>%
  count(document, word, sort = TRUE)

# Use bind_tf_idf from tidytext
tfidf_result <- term_freq %>%
  bind_tf_idf(word, document, n)



# tf-idf for Quran translations---------
## Sahih Translation tf-idf ----------


wc_by_surah_sahih_tfidf <- wc_by_surah_sahih |> 
  bind_tf_idf(word, surah_title_en, n) |> 
  arrange(desc(tf_idf))




## Plot tf-idf for Sahih Translation ---------
wc_by_surah_sahih_tfidf |> 
  ggplot(aes(x=1:length(tf_idf), y = log(tf_idf))) +
  geom_point(color = "blue", size = 0.5) +
  labs(title = "tf-idf VS number of words in Surah (Sahih Translation)",
       x = "n",
       y = "log(tf-idf)")



### Cont page 66

