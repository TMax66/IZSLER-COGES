library(tidyverse)
library(shiny)
library(rhandsontable)
library(readxl)
ui <- basicPage(
  
  mainPanel(
    selectInput("rep", "Seleziona il reparto", 
                c("BG", "SO")),
    rHandsontableOutput("hotable1")))