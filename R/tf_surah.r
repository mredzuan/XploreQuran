#' Create term frequency by surah
#'
#' @param tanzil_trans_object Tanzil translation object created with `tanzil_translation()` function
#' @param surah_number Surah number. Default to all 1-114 surah. Can refer to `quran_index` for surah number and Quran index.
#'
#' @return A term frequency data frame for selected surah 
#' 
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' 
#' 
#' 
#' @export
#'
#' @examples
#' # Example using package default data (if data is built)
#' # data(trans_en_sahih, package = "XploreQuran")
#' # tf_trans(trans_en_sahih, surah_number = 1)
#' 
#' 
tf_trans <- function(tanzil_trans_object, surah_number = (1:114)){
  
  surah_number <- as.integer(surah_number)
  
  
  if(!"translationList" %in% class(tanzil_trans_object)){
    stop("Input argument is not tanzil translation object, please create this object with tanzil_translation() function")
  }
  
  
  if(!is.integer(surah_number) || surah_number > 114 || surah_number < 1) {
    stop("surah_number input argument must be a integer value between 1 to 114")
  }
  
  count_word_trans <- tanzil_trans_object[[1]] %>% 
    filter(surah_id %in% surah_number) %>% 
    unnest_tokens(word, translation) %>% 
    count(surah_id, surah_title_en, word, sort = TRUE)
  
  total_word_trans <- count_word_trans %>% 
    group_by(surah_id) %>% 
    summarize(total = sum(n))
  
  join_count_total_word_trans <- left_join(count_word_trans, total_word_trans, by = "surah_id") %>% 
    mutate(tf = n/total)
}
