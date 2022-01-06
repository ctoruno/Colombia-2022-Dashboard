## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Candidates Twitter Value Boxes Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      January 3rd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Twitter Boxes UI                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# valueBox_UI <- function(id){
#   ns <- NS(id)
#   tagList(
#     valueBoxOutput(ns("user_since"), width = NULL),
#     valueBoxOutput(ns("tweets_count")),
#     valueBoxOutput(ns("followers_count"))
#   )
# }

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Twitter Boxes SERVER                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

valueBox_SERVER <- function(id, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      eventReactive(glob$submitted, {
      
        # Defining candidate
        selection <- glob$main_candidate
        
        # Getting candidate info
        info <- users_data(lookup_users(candidates.ls %>% map_chr(1) %>% str_sub(2))) %>%
          filter(screen_name == selection %>% str_sub(2))
        
        user_since <- as.Date(info %>% pull(account_created_at), format =  "%d-%m-%Y")
        followers  <- formatC(info %>% pull(followers_count),
                              format="f", big.mark = ",", digits=0)
        tweets_count <- formatC(timelines.df %>%
                                  group_by(screen_name) %>%
                                  summarise(tweets = length(screen_name)) %>%
                                  filter(screen_name == selection %>% str_sub(2)) %>%
                                  pull(tweets),
                                format="f", big.mark = ",", digits=0)
        
        
        list(user_since,
             tweets_count,
             followers)
        
      })
    })
}
