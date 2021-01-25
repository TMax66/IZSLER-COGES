ui<-fluidPage(theme = shinytheme("cerulean"),
               titlePanel("Strumento di programmazione "),
 
           
     
                wellPanel(
                    fluidRow( 
                        column(4,  
                h3("Programmazione"),            
                numericInput("rt", "Ricavo Totale previsto",  value = "1000000"), 
                br(), 
                numericInput("fte", "Full Time Equivalenti disponibili ",  value = "100"), 
                br(), 
                # numericInput("ftet", "Full Time Equivalenti teorico",  value = ""), 
                # br(),
                sliderInput("pc", "percentuale FTE allocata agli obiettivi", min=0, max= 50,  value = "0")), 
                
                column(8, 
                       tableOutput("tb")  
                    
                ))),
                
                br(),br(),br(),
                
                wellPanel(
                    fluidRow(
                        column(4, 
                h3("Verifica"),
                sliderInput("Vrt", "Variazione percentuale del Ricavo Totale previsto", min=-50, max= 50,  value = 0),
                br(), 
                sliderInput("Vfte", "Variazione percentuale del FTE programmato ", min=-50, max= 50,  value = 0)),
                
                column(8, 
                   tableOutput("tb2") )))
                

             
    )
 
   
             

