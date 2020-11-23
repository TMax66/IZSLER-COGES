server <- function(input, output, session) { 
  
###Quadro Generale Dashboard#####
  
output$esami <- renderValueBox({
    valueBox(
      tabella %>% 
        filter(Dipartimento == "Total") %>% 
        select("N.esami"), "# esami", icon = icon("keyboard"),
      color = "navy"
    )
  })
  
  output$ra <- renderValueBox({
    valueBox(
      tabella %>% 
        filter(Dipartimento == "Total") %>% 
        select("RA"), "Ricavi per attività analitica", icon = icon("keyboard"),
      color = "yellow"
    )
  })
  
  
  prettyNum(totes,big.mark = ","),  icon = "fa-flask"
  
  
  
  
  
  
  
  }
