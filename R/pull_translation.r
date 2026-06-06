#' Pull Quran Translation From tanzil.net
#'
#' @param urlLink url translation link from https://tanzil.net/trans/
#'
#' @return list of translation text and info
#' 
#' @importFrom readr read_delim
#' @importFrom stringr str_split_fixed str_detect str_remove str_trim
#' @importFrom tidyr separate
#' @import dplyr
#' @import quRan
#' 
#' @examples
#' \dontrun{
#' # Requires internet connection to download from tanzil.net
#' trans_ms_basmeih <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
#' head(trans_ms_basmeih$translation_text)
#' }
#' 
#' @export
tanzil_translation <- function(urlLink) {
  
  trans <- read_delim(urlLink, delim = "\n", col_names = FALSE, show_col_types = FALSE)
  
  # The Tanzil text has exactly 6236 ayahs
  trans_text <- trans[[1]][1:6236] %>% 
    str_split_fixed("\\|", 3) %>% 
    as.data.frame() %>% 
    rename("surah_id" = names(.)[1], "ayah_id" = names(.)[2], "translation" = names(.)[3]) %>% 
    mutate(surah_id = as.integer(surah_id)) %>% 
    mutate(ayah_id = as.integer(ayah_id))
    
  # Join with quRan dataset to get juz, surah name, etc.
  # We use the quran_en dataset from the quRan package as a base structure
  data("quran_en", package = "quRan", envir = environment())
  
  quran_meta <- quran_en %>%
    select(surah_id, ayah_id, surah_title_ar, surah_title_en, surah_title_en_trans, revelation_type, juz_id, ruku_id) %>%
    distinct()
    
  trans_text <- trans_text %>%
    left_join(quran_meta, by = c("surah_id", "ayah_id")) %>%
    select(surah_id, ayah_id, juz_id, ruku_id, surah_title_ar, surah_title_en, surah_title_en_trans, revelation_type, translation)
  
  # Extract info from bottom of file
  trans_info <- trans[6237:nrow(trans), ] %>% 
    filter(str_detect(X1, "\\w+")) %>% 
    mutate(X1 = str_remove(X1,"^#")) %>% 
    mutate(X1 = str_trim(X1)) %>% 
    separate(X1, c("info", "value"), sep = ":", extra = "merge", fill = "right") %>% 
    filter(!is.na(info)) %>%
    mutate(value = trimws(value))
  
  trans_list <- list("translation_text" = trans_text, "translation_info" = trans_info)
  class(trans_list) <- append("translationList", class(trans_list))
  
  attr(trans_list, "trans_indicator") <- trans_info %>% 
    filter(info == "Translator") %>% 
    pull(value) %>% 
    paste(collapse = "-")
  
  return(trans_list)
}
