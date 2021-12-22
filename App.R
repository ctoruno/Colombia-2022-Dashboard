## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     December 5th, 2021
##
## This version:      December 22th, 2021
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
            "shiny", "shinydashboard", "shinythemes", "shinyWidgets", "waiter",
            "plotly", "wordcloud2", "tidytext", "tidyverse", "magrittr"),
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
            height = 550,
            materialSwitch(
              inputId = "tagsFilter",
              label = "Remove tags from data?", 
              value = T,
              right = T,
              status = "success"),
            multiInput(
              inputId = "selected_candidates.wcd",
              label = "Candidates:",
              choices = NULL,
              choiceNames = as.list(names(candidates.ls)),
              choiceValues = candidates.ls %>%  map_chr(2)),
            dateRangeInput("date_range.wcd",
                           label = h3("Date range"),
                           start = "2021-11-17",
                           min   = "2021-11-17",
                           end   = Sys.Date()-2,
                           max   = Sys.Date()-2),
            actionBttn(
              inputId = "submit_wordcloud",
              label = "Submit",
              style = "unite", 
              color = "success",
              size  =  "sm")),
        box(wordcloud2Output("wordcloud"),
          height = 550,
            width = 8)
      )
    ),
    
    # Mentions per candidate
    tabItem(
      tabName = "mentions",
      fluidRow(
        box(title = "Filters",
            width = 4,
            height = 400,
            pickerInput(
              inputId = "selected_candidate.mnts",
              label = "Candidate:", 
              choices = candidates.ls %>%  map_chr(2),
              options = pickerOptions(
                actionsBox = T,
                liveSearch = T)),
            pickerInput(
              inputId = "comparison_candidates.mnts",
              label = "Compare with:", 
              choices = candidates.ls %>% map_chr(2),
              multiple = T,
              options = pickerOptions(
                liveSearch = T,
                size = 6,
                maxOptions = 4,
                maxOptionsText = "A maximum of four options are allowed")),
            actionBttn(
              inputId = "submit_mentions",
              label = "Submit",
              style = "unite",
              color = "success",
              size  =  "sm")),
        box(plotlyOutput("mentions"),
            height = 450,
            width = 8)
      ),
      fluidRow(
        box(
          tags$head(
            tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
          ),
          title = "Most popular tweet from candidate",
          #uiOutput("most_pop.mnts"),
          width = 6,
          height = 800),
        box(
          tags$head(
            tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
          ),
          title = "Most popular tweet:",
          uiOutput("most_fav.mnts"),
          width = 3,
          height = 800),
        
        box(
          tags$head(
            tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
          ),
          title = "Tweet with the most rets:",
          uiOutput("most_rtw.mnts"),
          width = 3,
          height = 800)
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
filter4Server <- function(selection_filter, dates_filter = NULL){
  
  # Defining filtered data
  selection.here <- selection_filter
  if (is.null(dates_filter) == T) {
    filtered_data <- master_data.df %>%
      filter(str_detect(.data$text, regex(paste(selection.here, collapse = "|"))))
  } else {
    dates.here     <- dates_filter
    filtered_data <- master_data.df %>%
      filter(str_detect(.data$text, regex(paste(selection.here, collapse = "|"))) &
               between(as.Date(.data$created_at), dates.here[1], dates.here[2]))
  }
  
}

## 2.2 Wordcloud: Defining function that will clean the data and produce a word count

counting4Server <- function(filtered_data, tags){
  
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
    filter(!(str_detect(words, regex(keywords, ignore_case = T)))) %>%
    { if (tags == TRUE) filter(.,!str_detect(words, "^@")) else . }
}

## 2.2 Mentions per candidate: Defining a function that will collapse the data to use in mentions

collapse4Server <- function(filtered_data, selection_filter){
  
  selection.here <- paste0("filter_", selection_filter)
  mentions.df <- filtered_data %>%
    select(created_at, selection.here) %>%
    mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>%
    group_by(day = cut(date, breaks = "day")) %>%
    summarise(across(starts_with("filter_"), 
                     ~sum(.x),
                     .names = "{gsub('filter_', 'sum_', {.col}, fixed = TRUE)}")) %>%
    mutate(day = as.Date(day, format="%Y-%m-%d")) %>%
    pivot_longer(!day, 
                 names_to = c(".value", "candidate"),
                 names_sep = "_")
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3.  Dashboard Server                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(31478)

server <- function(input, output, session){
  
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
      
    # Applying cleaning function (waiter needed)
    coolwaiter$show()
    selection4counting.df <- filter4Server(selection_filter = input$selected_candidates.wcd, 
                                           dates_filter     = input$date_range.wcd)
    data4wordcloud.df <- counting4Server(filtered_data = selection4counting.df,
                                         tags = input$tagsFilter)
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
  
  
  ## 3.2 Mentions Panel ========================================================================================
  
  # Reactive comparison list
  observeEvent(input$selected_candidate.mnts, {
    updatePickerInput(session = session,
                      inputId = "comparison_candidates.mnts",
                      choices = (candidates.ls %>% 
                                   map_chr(2))[- which((candidates.ls %>% map_chr(2)) == "galan")],)
  })
  
  
  
  # Creating mentions plot
  mentions.ptly <- eventReactive(input$submit_mentions, {
    
    # Applying cleaning function (waiter needed)
    coolwaiter$show()
    selection4counting.df <- filter4Server(selection_filter = c(input$selected_candidate.mnts, 
                                                                input$comparison_candidates.mnts), 
                                           dates_filter     = NULL)
    data4plotly.df <- collapse4Server(filtered_data    = selection4counting.df,
                                      selection_filter = c(input$selected_candidate.mnts, 
                                                           input$comparison_candidates.mnts))
    
    # Running ggplot
    mentions.plot <- ggplot(data4plotly.df, aes(x = day, group=1)) +
      geom_line(aes(y = sum, color = candidate)) +
      theme_bw() +
      labs(title = "Timeline per Candidate",
           x = NULL, 
           y = "Mentions") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "top",
            text = element_text(size = 18, family = "Ledger"),
            plot.title = element_text(size = 22),
            plot.subtitle = element_text(size = 20, face = "italic"),
            plot.caption = element_text(vjust = -0.5, hjust = 1, size = 14))
    
    coolwaiter$hide()
    
    # Converting to plotly element
    ggplotly(mentions.plot, dynamicTicks = T) %>%
      rangeslider() %>%
      layout(hovermode = "x") %>%
      config(displaylogo = F)
    
  })
  
  # Assigning plotly to output 
  output$mentions <- renderPlotly({
    mentions.ptly()
  })
  
  # Filtering data for tweet boxes
  url4tabs.mnts <- eventReactive(input$submit_mentions, {
    selection4counting.df <- filter4Server(selection_filter = input$selected_candidate.mnts,
                                           dates_filter     = NULL)
  })
  
  # Assigning tweets to outputs
  output$most_fav.mnts <- renderUI({
    most_rtw.mnts <- url4tabs.mnts() %>%
      slice_max(favorite_count, n = 1, with_ties = F) %>%
      pull(status_id)
    url_most_rtw.mnts <- lookup_tweets(most_rtw.mnts)["status_url"]
    
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = url_most_rtw.mnts)),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })
  
  output$most_rtw.mnts <- renderUI({
    most_rtw.mnts <- url4tabs.mnts() %>%
      slice_max(retweet_count, n = 1, with_ties = F) %>%
      pull(status_id)
    url_most_rtw.mnts <- lookup_tweets(most_rtw.mnts)["status_url"]
    
    tagList(
      tags$blockquote(class = "twitter-tweet",
                      tags$a(href = url_most_rtw.mnts)),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
  })
  
}


shiny::shinyApp(ui, server)
