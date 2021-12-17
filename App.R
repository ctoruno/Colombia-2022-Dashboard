## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     December 5th, 2021
##
## This version:      December 16th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/GitHub/Political-Observatory-Colombia/")
# rm(list=ls())

# Required packages
lapply(list("rtweet", "haven", "qdap", "tm", "syuzhet", "SnowballC", "wesanderson",
            "tidytext", "dplyr", "purrr", "readr", "stringr", "magrittr", 
            "shiny", "shinydashboard", "shinythemes", "shinyWidgets", "waiter",
            "wordcloud2", "ggplot2"),
       library, character.only = T)

# Loading Twitter data
load("twitter_data4dash.RData")

# Notes: 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Dashboard UI                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1.1 Dashboard header ========================================================================================
header <- dashboardHeader(
  title = "Colombia 2022: A Twitter companion",
  dropdownMenu(type = "notifications",
               icon = icon("exclamation-triangle"),
               notificationItem(
                 text = paste0("Last successful Twitter extraction: ", batches.df$Date[1]),
                 icon("calendar-alt"))
               )
)

## 1.2 Dashboard sidebar =======================================================================================
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", 
             tabName = "overview", icon = icon("dharmachakra")),
    menuItem("Wordclouds",
             tabName = "wordclouds", icon = icon("cloud")),
    menuItem("Mentions per candidate",
             tabName = "mentions", icon = icon("chart-line")),
    menuItem("Top hashtags",
             tabName = "hashtags", icon = icon("clipboard")),
    menuItem("Sentiment trends",
             tabName = "dashboard", icon = icon("comment-dots")),
    menuItem("Methodology",
             tabName = "methodology", icon = icon("book")),
    menuItem("About",
             tabName = "about", icon = icon("chess"))
  )
)

## 1.3 Dashboard body ==========================================================================================

# JavaScript to add an id to the <section> tag so we can overlay waiter on top of it
# Thanks to John Coene for the explanation (https://waiter.john-coene.com/#/integrations#shinydashboard)
contentPanel <- "
$( document ).ready(function() {
  var section = document.getElementsByClassName('content');
  section[0].setAttribute('id', 'waiter-content');
});"

body <- dashboardBody(
  tags$head(
    tags$script(contentPanel)
  ),
  use_waiter(),
  tabItems(

    # Wordclouds
    tabItem(
      tabName = "wordclouds",
      fluidRow(
        box(title = "Filters",
            width = 4,
            height = 500,
            multiInput(
              inputId = "selected_candidates",
              label = "Candidates:",
              choices = NULL,
              choiceNames = as.list(names(candidates.ls)),
              choiceValues = candidates.ls %>%  map_chr(2)),
            dateRangeInput("date_range",
                           label = h3("Date range"),
                           start = "2021-11-17",
                           end = "2021-12-08"),
            actionBttn(
              inputId = "submit_wordcloud",
              label = "Submit",
              style = "unite", 
              color = "success",
              size  =  "sm")),
        box(wordcloud2Output("wordcloud"),
          height = 500,
            width = 8)
      )
    ),
    
    # Mentions per candidate
    tabItem(
      tabName = "mentions_candidate",
      fluidRow(
        box(title = "Filters",
            width = 4,
            height = 500,
            multiInput(
              inputId = "selected_candidates",
              label = "Candidates:",
              choices = NULL,
              choiceNames = as.list(names(candidates.ls)),
              choiceValues = candidates.ls %>%  map_chr(2)),
            dateRangeInput("date_range",
                           label = h3("Date range"),
                           start = "2021-11-17",
                           end = "2021-12-08"),
            actionBttn(
              inputId = "submit_mentions",
              label = "Submit",
              style = "unite", 
              color = "success",
              size  =  "sm")),
        box(plotlyOutput("mentionsPlot"),
            height = 500,
            width = 8)
      )
    )
  )
)

## 1.4 Back front ==============================================================================================

ui <- dashboardPage(header, sidebar, body,
                    skin = "green")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Defining Functions                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 2.1 Filter: Defining function that will filter the data
filter4love <- function(selection_filter, dates_filter){
  
  # Defining filtered data
  selection.here <- selection_filter
  dates.here     <- dates_filter
  filtered_data <- master_data.df %>%
    filter(str_detect(.data$text, regex(paste(selection.here, collapse = "|"))) &
             between(as.Date(.data$created_at), dates.here[1], dates.here[2]))
}
  
## 2.2 Wordcloud: Defining function that will clean the data and produce a word count

counting4love <- function(filtered_data){
  
  # Tokenizing text and removing stop words
  twitter_tokenized.df <- filtered_data %>%
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
                      "x", "q"))
  # check <- twitter_tokenized.df %>%
  #   count(words) %>%
  #   arrange(desc(n))  # just to check remaining stopwords
  
  # Creating word counts
  wcount_raw.df <- twitter_tokenized.df %>% count(words) %>% arrange(desc(n))
  keywords <- paste(candidates_query1, candidates_query2, parties_query1, parties_query2, sep = " ") %>%
    str_replace_all(" OR ", "|")
  wcount_flt.df <- wcount_raw.df %>%    # Removing keywords used to extract tweets
    filter(!(str_detect(words, regex(keywords, ignore_case = T))))
}

## 2.2 Mentions per candidate: Defining a function that will collapse the data

collapse4love <- function(filtered_data, selection_filter){
  
  selection.here <- paste0("filter_", selection_filter)
  mentions.df <- filtered_data %>%
    select(created_at, selection.here) %>%
    mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(day = cut(date, breaks = "day")) %>%
    summarise(across(starts_with("filter_"), 
                     ~sum(.x),
                     .names = "{gsub('filter_', 'sum_', {.col}, fixed = TRUE)}")) %>%
    mutate(day = as.Date(day, format="%Y-%m-%d"))
  
  
  
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3.  Dashboard Server                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(31478)

server <- function(input, output){
  
  # Defining spinner on loading
  coolwaiter <- Waiter$new(id = "waiter-content",
                           html = tagList(spin_whirly(),
                                          br("..."),
                                          br("..."),
                                          h4("Loading your wordcloud...", 
                                               style="color:green;")),
                           color = "white")
  
  ## 3.1 Wordcloud Panel =======================================================================================
  
  # Creating wordcloud
  wordcloud.wc2 <- eventReactive(input$submit_wordcloud, {
    
    coolwaiter$show()
      
    # Applying cleaning function
    selection4counting.df <- filter4love(selectionWC = input$selected_candidates, 
                                     datesWC     = input$date_range)
    data4wordcloud.df <- counting4love(filtered_data = selection4counting.df)
    
    coolwaiter$hide()
    
    # Running wordcloud
    wordcloud2(data4wordcloud.df[1:200,], size = 0.9,
               color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                               nrow(data4wordcloud.df[1:200,])),
               ellipticity = 0.2, shuffle = F)
  })
  
  # Assigning wordcloud to output 
  output$wordcloud <- renderWordcloud2({
    wordcloud.wc2()
  })
  
}


shiny::shinyApp(ui, server)
