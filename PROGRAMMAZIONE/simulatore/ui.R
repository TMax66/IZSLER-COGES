ui<-navbarPage("",
    theme = shinytheme("cerulean"),
           
    tabPanel("",
             sidebarLayout(
               sidebarPanel("", 
                
                h3("Programmazione"),            
                numericInput("rt", "Ricavo Totale previsto",  value = "1000000"), 
                br(), 
                numericInput("fte", "Full Time Equivalenti disponibili ",  value = "100"), 
                br(), 
                # numericInput("ftet", "Full Time Equivalenti teorico",  value = ""), 
                # br(),
                sliderInput("pc", "percentuale FTE allocata agli obiettivi", min=0, max= 50,  value = "0"), 
                
                hr(),
                br(),
                
                h3("Verifica"),
                sliderInput("Vrt", "Variazione percentuale del Ricavo Totale previsto", min=-50, max= 50,  value = 0),
                br(), 
                sliderInput("Vfte", "Variazione percentuale del FTE disponibile ", min=-15, max= 15,  value = 0),
                ), 
               mainPanel(
                 tableOutput("tb"),
                 br(),br(),br(), 
                 tableOutput("tb2")
               )
             )
    )
)
   
             

