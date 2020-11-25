server <- function(input, output, session) { 
  
###Quadro Generale Dashboard#####
  
  es <- reactive(tabella %>% 
    filter(Dipartimento == "Total") %>% 
    select("N.esami"))
  
output$esami <- renderValueBox({
    valueBox(prettyNum(es(), big.mark = ","), "N.esami",  icon = icon("flask"),
      color = "blue"
    )
  })
  
ra <- reactive(tabella %>% 
  filter(Dipartimento == "Total") %>% 
  select("RA"))

  output$ra <- renderValueBox({
    valueBox(prettyNum(ra(), big.mark = ",") , "Ricavi per attività analitica", icon = icon("euro"),
      color = "yellow"
    )
  })

  vp <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RVP"))
  
  output$vp <- renderValueBox({
    valueBox(prettyNum(vp(), big.mark = ",") , "Ricavi per vendita prodotti", icon = icon("euro"),
             color = "yellow"
    )
  })
  
  
  ai <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RAI"))
  
  output$ai <- renderValueBox({
    valueBox(prettyNum(ai(), big.mark = ",") , "Ricavi per attività interna", icon = icon("euro"),
             color = "yellow"
    )
  })
  
  rt <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RT"))
  
  output$rt <- renderValueBox({
    valueBox(prettyNum(rt(), big.mark = ",") , "Ricavi totali", icon = icon("euro"),
             color = "yellow"
    )
  })
  
  rfte <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("R/FTET"))
  
  output$rfte <- renderValueBox({
    valueBox(prettyNum(rfte(), big.mark = ",") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
             color = "yellow"
    )
  })
  
  
  
output$t <- renderTable({tabella[-5, ]})
  
  
 
ric <- reactive({
  ricerca %>% 
  group_by(tipologia) %>% 
  count(nr) %>%
  summarise(n.articoli = n())
  })

output$IF <- renderValueBox({
  valueBox(
      (ric() %>% 
        filter(tipologia == "IF") %>% 
        select(n.articoli)), "Articoli pubblicati su riveste peer-review con IF", icon = icon("book"), color = "light-blue")
  })
  
  }
