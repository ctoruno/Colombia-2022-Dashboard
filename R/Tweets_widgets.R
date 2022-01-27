## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Tweets Widgets Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     January 14th, 2022
##
## This version:      January 19th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Widgets UI                                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# fav_output <- function(id){
#   ns <- NS(id)
#   tagList(
#     tags$head(
#       tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
#     ),
#     uiOutput(ns("most_fav"))
#   )
# }
# 
# ret_output <- function(id){
#   ns <- NS(id)
#   tagList(
#     tags$head(
#       tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
#     ),
#     uiOutput(ns("most_rtw"))
#   )
# }

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Widgets SERVER                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

popular_tweets_UI <- function(id, panel, filtered_data, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      eventReactive(glob$submitted, {
        
        # Setting dataset
        url_data  <- filtered_data() %>%
          filter(candidate == 1)
        
        if (panel == "speech") {

          url <- url_data %>%
            slice_max(favorite_count, n = 3, with_ties = F) %>%
            pull(status_url)

        } else {

          # Getting URL of most fav and most rtw tweet
          lapply(list("most_fav",
                      "most_rtw"),
                 function(widget){

                   # Identifying tweet
                   if (widget == "most_fav") {
                     tweet_id <- url_data %>%
                       slice_max(favorite_count, n = 1, with_ties = F)
                   } else {
                     tweet_id <- url_data %>%
                       slice_max(retweet_count, n = 1, with_ties = F)
                   }

                   url <- tweet_id %>% pull(status_url)

                 })
        }
      })
    })
}
