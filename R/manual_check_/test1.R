# Load required libraries----------

packages <- c("dplyr", "RColorBrewer", "readr", "stringr", "tidyr", "tidytext", "wordcloud", "shiny", "bslib", "plotly", "quRan")
lapply(packages, library, character.only = TRUE)


urlLink <- "https://tanzil.net/trans/en.yusufali"
urlLink2 <- "https://tanzil.net/trans/en.qarai"
urlink3_japan <- "https://tanzil.net/trans/ja.japanese" 


# We use the quran_en dataset from the quRan package as a base structure
data("quran_ar", package = "quRan", envir = environment())

quran_meta <- quran_ar %>%
  distinct()



#Load sample translation data from Tanzil----------------
trans <- read_delim(urlLink, delim = "\n", col_names = FALSE, show_col_types = FALSE)
# 1st delim = surah num
# 2nd delim = ayah num

trans2 <- read_delim(urlLink2, delim = "\n", col_names = FALSE, show_col_types = FALSE)
trans3_jpn <- read_delim(urlink3_japan, delim = "\n", col_names = FALSE, show_col_types = FALSE)
  
# The Tanzil text has exactly 6236 ayahs
trans_text <- trans[[1]][1:6236] %>% 
  str_split_fixed("\\|", 3) %>% 
  as.data.frame() %>% 
  rename("surah_id" = names(.)[1], "ayah_id" = names(.)[2], "translation" = names(.)[3]) %>% 
  as_tibble(.) %>% 
  mutate(surah_id = as.integer(surah_id)) %>% 
  mutate(ayah_id = as.integer(ayah_id)) %>% 
  left_join(quran_meta, by = c("surah_id" = "surah_id", "ayah_id" = "ayah"))




  
# Extract info from bottom of file
trans_info <- trans[6237:nrow(trans), ] %>% 
  filter(str_detect(X1, "\\w+")) %>% 
  mutate(X1 = str_remove(X1,"^#")) %>% 
  mutate(X1 = str_trim(X1)) %>% 
  # Remove info = "Quran Translation"
  filter(!str_detect(X1, "^Quran Translation")) %>%
  separate(X1, c("info", "value"), sep = ":", extra = "merge", fill = "right") %>% 
  filter(!is.na(info)) %>%
  mutate(value = trimws(value))
  
trans_list <- list("translation_text" = trans_text, "translation_info" = trans_info)
  class(trans_list) <- append("translationList", class(trans_list))
  
attr(trans_list, "trans_indicator") <- trans_info %>% 
  filter(info == "Translator") %>% 
  pull(value) %>% 
  paste(collapse = "-")


#test-------

trans_ms_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
head(trans_ms_basmeih$translation_text)
