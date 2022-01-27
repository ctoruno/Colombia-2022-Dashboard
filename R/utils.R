## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Utilities
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 25th, 2022
##
## This version:      January 25th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining a cleaning function
data2counts <- function(data, switch) {
  
  # Tokenizing text and removing stop words
  twitter_tokenized.df <- data %>%
    mutate(text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i",
                                                   "ó" = "o", "ú|ü" = "u"))) %>%
    select(1:5) %>%
    mutate(tweet_id = row_number()) %>%
    unnest_tokens(words, text, token = "tweets", strip_url = T) %>%
    anti_join(data.frame(words = stopwords("es")) %>%
                mutate(words = str_replace_all(words,
                                               c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú|ü" = "u"))))
  
  # Removing custom stop words
  twitter_tokenized.df <- twitter_tokenized.df %>%      # we remove stopwords in the top-150
    anti_join(tribble(~words, "ahora", "hacer", "hace", "puede", "mismo", "tan", "señor", "ud", "siempre",
                      "menos","dice", "debe", "ver", "hoy", "sabe", "van", "quiere", "creo", "ustedes",
                      "decir", "pues", "cabal", "vamos", "nunca", "claro", "ahi", "jajaja", "jajajaja",
                      "entonces", "gran", "vez", "da", "toda", "d", "favor", "parte", "quieren", "cada",
                      "hizo", "hecho", "tener", "dijo", "aqui", "cree", "tal", "parece", "hacen",
                      "despues", "que", "usted", "solo", "ser", "asi", "va", "años", "habla", "tipo",
                      "misma", "cosas", "5", "necesita", "alguien", "todas", "aun", "sino", "cosa",
                      "x", "q", "pais", "colombia"))
  
  # Creating word counts
  wcount_raw.df <- twitter_tokenized.df %>% count(words) %>% arrange(desc(n))
  keywords <- paste(candidates_query1, candidates_query2, parties_query1, parties_query2, sep = " ") %>%
    str_replace_all(" OR ", "|")
  wcount_flt.df <- wcount_raw.df %>%    # Removing keywords used to extract tweets
    filter(!(str_detect(words, regex(keywords, ignore_case = T)))) %>%
    { if (switch == T) filter(.,!str_detect(words, "^@")) else . }
  
  # Defining returning lits
  list(wcount_flt.df,
       twitter_tokenized.df) # We need this tokenized data for Topic Modelling in the speech panel
}


# Defining a collapsing function
collapse4Server <- function(filtered_data, xdata, glob){
  
  # Defining candidates selection
  candidate <- glob$main_candidate
  others    <- glob$sec_candidates
  
  if (xdata == "social") {
    
    # The input in the UI gives you {candidates.ls %>%  map_chr(1)} as value, but we need its equivalent 
    # value as {candidates.ls %>%  map_chr(2)} which contains the name of the columns in master_data.df
    # So we need to match the selected candidates to obtain their index within candidates.ls
    
    if (is.null(others)) {
      index <- match(candidate, candidates.ls %>%  map_chr(1))
    } else {
      index <- match(c(candidate, others), candidates.ls %>%  map_chr(1))
    }
    
    # Defining the {candidates.ls %>% map_chr(2)} value and the overall selection
    selection <- candidates.ls[index] %>% map_chr(2)
    selection.here <- paste0("filter_", selection)
    
    # Collapsing data
    mentions.df <- filtered_data %>%
      select(created_at, selection.here) %>%
      mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>%
      group_by(day = cut(date, breaks = "day")) %>%
      summarise(across(starts_with("filter_"), 
                       ~sum(.x),
                       .names = "{gsub('filter_', 'sum_', {.col}, fixed = TRUE)}")) %>%
      mutate(day = as.Date(day, format="%Y-%m-%d")) %>%
      pivot_longer(!day, 
                   names_to = c(".value", "user"),
                   names_sep = "_")
  } else {
    
    # Collapsing data
    mentions.df <- filtered_data %>%
      select(created_at, screen_name) %>% 
      mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>% 
      group_by(day = cut(date, breaks = "day"), screen_name) %>% 
      count(day, screen_name) %>%
      mutate(date = as.Date(day)) %>%
      rename(user = screen_name,
             sum = n)
  }
  
  return(mentions.df)
}