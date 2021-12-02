########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to helper functions: helper functions are snippet of code that   #
# allow us to use it again and again by simply calling it. By keeping all the helper functions in this #
# file we can ensure code manageability, and re-usability as all the user defined functions will be in #
# place.                                                                                               #
########################################################################################################


# This function us used to clean the text...
string_cleaner = function(unclean_str){ # unclean_str: vector of strings
  
  # Creating one large string by concatinating them together...
  weekly_sent_txt = ""
  for(txt in unclean_str){
    weekly_sent_txt = paste(weekly_sent_txt, txt)
  }
  
  
  weekly_sent_txt = Corpus(VectorSource(weekly_sent_txt)) # Creating a dictionary of words...
  weekly_sent_txt = tm_map(weekly_sent_txt, content_transformer(tolower)) # string to lower case...
  weekly_sent_txt = tm_map(weekly_sent_txt, removePunctuation) # removing punctuations...
  weekly_sent_txt = tm_map(weekly_sent_txt, removeWords, stopwords("english")) # removing stop words...
  weekly_sent_txt = str_replace_all(weekly_sent_txt, "[^[:alnum:]]", " ")  # Removing all non-alphanumeric
  
  # removing all words with length less than or equal to 3
  weekly_sent_txt = paste(str_extract_all(weekly_sent_txt, '\\w{4,}')[[1]], collapse=' ') 
  
  return(tolower(weekly_sent_txt[1]))
}