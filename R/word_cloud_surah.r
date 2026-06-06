#' Create Quran translation's word cloud 
#'
#' @param tanzil_trans_object Tanzil translation object created with `tanzil_translation()` function
#' @param surah_number Surah number. Default to all 1-114 surah. Can refer to `quran_index` for surah number and Quran index.
#' @param max_word Maximum number of words to be plotted. least frequent terms dropped
#' @param min_freq words with frequency below min.freq will not be plotted
#'
#' @return Plot of word cloud
#' 
#' @import dplyr
#' @importFrom tidytext unnest_tokens
#' @importFrom wordcloud wordcloud
#' @importFrom RColorBrewer brewer.pal
#' 
#' @export
#'
#' @examples
#' # Example using package default data (if data is built)
#' # data(trans_en_sahih, package = "XploreQuran")
#' # wordcloud_trans(trans_en_sahih, surah_number = 114, max_word = 10)
#' 
wordcloud_trans <- function(tanzil_trans_object, surah_number = (1:114), max_word = 200, min_freq = 1){
  
  surah_number <- as.integer(surah_number)
  
  if(!"translationList" %in% class(tanzil_trans_object)){
    stop("Input argument is not tanzil translation object, please create this object with tanzil_translation() function")
  }
  
  if(!is.integer(surah_number) || surah_number > 114 || surah_number < 1) {
    stop("surah_number input argument must be a integer value between 1 to 114")
  }
  
  token_trans <- tanzil_trans_object[[1]] %>% 
    filter(surah_id %in% surah_number) %>% 
    unnest_tokens(word, translation) %>% 
    count(word)
  
  wordcloud(token_trans$word, token_trans$n, max.words = max_word, min.freq = min_freq,
            random.order = FALSE, rot.per = 0.1, colors = brewer.pal(8, "Dark2"))
  
  
}