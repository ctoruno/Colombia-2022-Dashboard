## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Frequency Analysis Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      December 31st, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Frequency Analysis UI                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tagsFilter_UI <- function(id) {
  ns <- NS(id)
  tagList(
    materialSwitch(
      inputId = ns("tags_filter"),
      label = "Remove tags from data?", 
      value = T,
      right = T,
      status = "success")
    )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Frequency Analysis SERVER                                                             ----
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
}



frequency_server <- function(id, filtered_data, glob){
  
  moduleServer(
    id,
    function(input, output, session){
      
      input_data  <- filtered_data() 
      tags_switch <- input$tags_filter
      others      <- glob$sec_candidates
      
      # Generating selected candidate counts
      main_data.df <- input_data %>% filter(candidate == 1)
      main_counts.df <- data2counts(data = main_data.df,
                                    switch = tags_switch)
      
      # Creating wordcloud
      wordcloud <- wordcloud2(main_counts.df[1:200,], size = 0.9,
                              color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                                              nrow(main_counts.df[1:200,])),
                              ellipticity = 0.2, shuffle = F)

      # Top words: selected candidate
      top_words_main <- datatable(main_counts.df %>%
                                    rename(Mentions = n,
                                           Words = words))

      # Top hashtags ussed by selected candidate
      top_hashtags <- datatable(main_counts.df %>%
                                  filter(str_detect(words, "^#")) %>%
                                  rename(Mentions = n,
                                         Hashtags = words))
      
      # Generating comparison candidates counts
      if (!is.null(others)) {
        comparison_data.df <- input_data %>% filter(comparison == 1)
        comparison_counts.df <- data2counts(data = comparison_data.df,
                                            switch = tags_switch) 
        # Top words: comparison candidates
        top_words_comparison <- datatable(comparison_counts.df %>%
                                            rename(Mentions = n,
                                                   Words = words))
      } else {
        top_words_comparison <- datatable(data.frame())
      }

      # Multiple ouput list
      list("Wordcloud"              = wordcloud,
           "Top Words - Main"       = top_words_main,
           "Top Words - Comparison" = top_words_comparison,
           "Top Hashtags"           = top_hashtags)
      
    }
  )
}     

