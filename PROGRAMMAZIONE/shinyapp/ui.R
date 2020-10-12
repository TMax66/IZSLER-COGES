library("tidyverse")
library("shiny")
library("rhandsontable")
library("readxl")
library("here")
ui <- basicPage(
  
  mainPanel(
    selectInput("rep", "Seleziona il reparto", 
                c("BG", "SO")),
    rHandsontableOutput("hotable1")))