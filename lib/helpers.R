# helper.function <- function()
# {
#   return(1)
# }

string_cleaner = function(unclean_str){
  weekly_sent_txt = ""
  for(txt in unclean_str){
    weekly_sent_txt = paste(weekly_sent_txt, txt)
  }
  weekly_sent_txt = Corpus(VectorSource(weekly_sent_txt))
  weekly_sent_txt = tm_map(weekly_sent_txt, content_transformer(tolower))
  weekly_sent_txt = tm_map(weekly_sent_txt, removePunctuation)
  weekly_sent_txt = tm_map(weekly_sent_txt, removeWords, stopwords("english"))
  weekly_sent_txt = str_replace_all(weekly_sent_txt, "[^[:alnum:]]", " ")  # Removing all non-alphanumeric
  weekly_sent_txt = paste(str_extract_all(weekly_sent_txt, '\\w{4,}')[[1]], collapse=' ')
  
  return(tolower(weekly_sent_txt[1]))
}