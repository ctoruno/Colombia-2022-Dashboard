## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Frequency Analysis Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      January 25th, 2022
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

# Defining SERVER function
frequency_server <- function(id, filtered_data, glob){
  
  moduleServer(
    id,
    function(input, output, session){
      
      input_data  <- filtered_data() 
      tags_switch <- input$tags_filter
      others      <- glob$sec_candidates
      
      # Generating selected candidate counts
      main_data.df <- input_data %>% filter(candidate == 1)
      main_counts_raw.df <- data2counts(data = main_data.df,
                                        switch = tags_switch)
      main_counts.df <- main_counts_raw.df[[1]]
      
      # Creating wordcloud
      wordcloud <- wordcloud2(main_counts.df[1:200,], size = 0.9,
                              color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                                              nrow(main_counts.df[1:200,])),
                              ellipticity = 0.2, shuffle = F)

      # Top words: selected candidate
      top_words_main <- datatable(
        main_counts.df %>% 
          rename(Mentions = n,  Words = words) %>%
          slice_max(Mentions, n = 30, with_ties = F),
        options = list(dom = 'tp',
                       scrollX = T,      # This solves all the columns width issue
                       autoWidth = T,    # Required to modify columns width
                       columnDefs = list(list(width = '50px',
                                              targets = "_all"))
        )
      )

      # Top hashtags used by selected candidate
      top_hashtags <- datatable(
        main_counts.df %>% 
          filter(str_detect(words, "^#")) %>% 
          rename(Mentions = n, Hashtags = words) %>%
          slice_max(Mentions, n = 10, with_ties = F),
        options = list(dom = 't',
                       scrollX = T,          # This solves all the columns width issue
                       autoWidth = T,        # Required to modify columns width
                       columnDefs = list(list(width = '50px',
                                              targets = "_all"))
        )
      )
      
      # Generating comparison candidates counts
      if (!is.null(others)) {
        comparison_data.df <- input_data %>% filter(comparison == 1)
        comparison_counts.df <- data2counts(data = comparison_data.df,
                                            switch = tags_switch)[[1]] 
        # Top words: comparison candidates
        top_words_comparison <- datatable(
          comparison_counts.df %>% 
            rename(Mentions = n, Words = words) %>%
            slice_max(Mentions, n = 30, with_ties = F),
          options = list(
            dom = 'tp',
            scrollX = T,
            autoWidth = T,
            columnDefs = list(list(width = '50px',
                                   targets = "_all"))
          )
        )
      } else {
        top_words_comparison <- datatable(data.frame())
      }

      # Multiple ouput list
      list("Wordcloud"              = wordcloud,
           "Top Words - Main"       = top_words_main,
           "Top Words - Comparison" = top_words_comparison,
           "Top Hashtags"           = top_hashtags,
           "Tokens for T.Modelling" = main_counts_raw.df[[2]])
      
    }
  )
}     

