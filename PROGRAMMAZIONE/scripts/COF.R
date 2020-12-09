library("tidyverse")
library("readxl")
library("RColorBrewer")
library("shiny")
library("shinydashboard")
library("here")
library("janitor")
library("here")
library("flextable")
library("shinyBS")
library("officer")
library("fmsb")

radar <- data.frame(
  Pubblicazioni = c(1.25, 0.51,1.24,1.47),
  Progetti = c(1.91, 1.41, 2.32, 3.65), 
  RFTE = c(1.27, 0.91, 1.00, 0.83), 
  Esami = c(0.35, 0.31, 0.20, 0.13), 
  FTED = c(0.2, 0.27, 0.27, 0.25), 
  FTEC = c(0.25, 0.24, 0.22, 0.28), 
  FTET = c(0.24, 0.25, 0.23, 0.28), 
  RT = c(0.31, 0.23, 0.23, 0.23)
)  
rownames(radar) <- c("DSA","ATLOM", "ATER", "DTSA")
radar <- rbind(c(4,4,4,1,1,1,1,1) , rep(0,8) , radar)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


radarchart( radar  ,
            #custom polygon
            #pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1), cglwd=0.8,
            #custom labels
            #vlcex=0.8 
)


# Add a legend
legend(x=1, y=0.7, legend = rownames(radar[-c(1,2),]), bty = "n", , pch=16 , cex=0.8, pt.cex=1)
