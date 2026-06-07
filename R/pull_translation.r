#' Pull Quran Translation From tanzil.net
#'
#' @param urlLink url translation link from https://tanzil.net/trans/
#'
#' @return list of translation text and info
#' 
#' @importFrom readr read_delim
#' @importFrom stringr str_split_fixed str_detect str_remove str_trim
#' @importFrom tidyr separate
#' @importFrom utils data
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
  
# We use the quran_en dataset from the quRan package as a base structure
data("quran_ar", package = "quRan", envir = environment())
  
quran_meta <- quran_ar %>%
  distinct()

# Import data from Tanzil  
trans <- read_delim(urlLink, delim = "\n", col_names = FALSE, show_col_types = FALSE)


# The Tanzil text has exactly 6236 ayahs.
# Join with meta data from quRan package to get the surah and ayah names, and other info.
trans_text <- trans[[1]][1:6236] %>% 
  str_split_fixed("\\|", 3) %>% 
  as.data.frame() %>% 
  rename("surah_id" = names(.)[1], "ayah_id" = names(.)[2], "translation" = names(.)[3]) %>% 
  as_tibble(.) %>% 
  mutate(surah_id = as.integer(surah_id)) %>% 
  mutate(ayah_id = as.integer(ayah_id)) %>% 
  left_join(quran_meta, by = c("surah_id" = "surah_id", "ayah_id" = "ayah")) %>% 
  # Rename "ayah_id" column to "surah_ayah_id" and "ayah_id.y" to "ayah_id"
  rename("surah_ayah_id" = "ayah_id", "ayah_id" = "ayah_id.y") %>%
  # Ensure all character-like columns are stored as character, not factor
  # (quRan package datasets may carry factor columns from older R behaviour)
  mutate(across(where(is.factor), as.character)) %>%
  select(ayah_id, surah_id, surah_ayah_id, everything())


  
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
  
return(trans_list)
}
