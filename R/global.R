# Load libraries
library(haven) 
library(wesanderson)
library(shiny) 
library(shinydashboard) 
library(shinydashboardPlus) 
library(shinythemes) 
library(shinyWidgets)
library(shinyjs)
library(waiter)
library(plotly) 
library(wordcloud2) 
library(DT) 
library(tidytext) 
library(tidyverse) 
library(magrittr)

# Load data
overview.ls               <- readRDS("data/overview.rds")
freq_analysis.ls          <- readRDS("data/freq_analysis.rds")
daily_activity.ls         <- readRDS("data/daily_activity.rds")
user_info.df              <- readRDS("data/user_info.rds")
tmodels.ls                <- readRDS("data/tmodels.rds")
twitter_widgets_urls.ls   <- readRDS("data/twitter_widgets_urls.rds")
candidateINFO.df          <- readRDS("data/candidateINFO.rds")
sentiment.df.ls          <- readRDS("data/sentiment_nrc.rds") 

# Source scripts
source("R/overview_UI.R")
source("R/candidates_input.R")
source("R/candidateINFO_output.R")
source("R/valueBoxes_output.R")
source("R/timelinePlot_output.R")
source("R/frequency_analysis.R")
source("R/tmodels_output.R")
source("R/widgets_output.R")

# ID vectors
candidates.ls <- list( "Amaya, Carlos" = c("@CarlosAmayaR", "amaya"),
                       "Barguil, David" = c("@davidbarguil", "barguil"),
                       "Barreras, Roy Leonardo" = c("@RoyBarreras", "roy"), 
                       "Char, Alejandro" = c("@AlejandroChar", "char"),
                       "Cristo, Juan Fernando" = c("@CristoBustos", "cristo"),
                       "Echeverry, Juan Carlos" = c("@JCecheverryCol", "echeverry"),
                       "Fajardo, Sergio" = c("@sergio_fajardo", "fajardo"),
                       "Galán, Juan Manuel" = c("@juanmanuelgalan", "galan"),
                       "Gaviria, Alejandro" = c("@agaviriau", "gaviria"),
                       "Gutiérrez, Federico" = c("@FicoGutierrez", "federico"),
                       "Hernández, Rodolfo" = c("@ingrodolfohdez", "hernandez"),
                       "Lizarazo, Aydeé" = c("@aydeelizarazoc", "lizarazo"),
                       "Márquez, Francia" = c("@FranciaMarquezM", "francia"),
                       "Peñalosa, Enrique" = c("@EnriquePenalosa", "penalosa"),
                       "Pérez, Luis"= c("@Luis_Perez_G", "perez"),
                       "Petro, Gustavo" = c("@petrogustavo", "petro"), 
                       "Robledo, Jorge Enrique" = c("@JERobledo", "robledo"),
                       "Rodríguez, John Milton" = c("@JohnMiltonR_", "milton"),
                       "Romero, Camilo" = c("@CamiloRomero", "camilo"),
                       "Toro, Dilian Francisca" = c("@DilianFrancisca", "toro"),
                       "Uriana, Arelis" = c("@urianaguariyu", "arelis"),
                       "Velasco, Luis Fernando" = c("@velascoluisf", "velasco"),
                       "Verano, Eduardo" = c("@veranodelarosa", "verano"),
                       "Zuluaga, Óscar Iván" = c("@OIZuluaga", "zuluaga"))
