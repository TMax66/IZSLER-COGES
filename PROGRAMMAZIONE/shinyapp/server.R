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
      color = "aqua"
    )
  })

  vp <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RVP"))
  
  output$vp <- renderValueBox({
    valueBox(prettyNum(vp(), big.mark = ",") , "Ricavi per vendita prodotti", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  
  ai <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RAI"))
  
  output$ai <- renderValueBox({
    valueBox(prettyNum(ai(), big.mark = ",") , "Ricavi per attività interna", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rt <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RT"))
  
  output$rt <- renderValueBox({
    valueBox(prettyNum(rt(), big.mark = ",") , "Ricavi totali", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rfte <- reactive(tabella %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("R/FTET"))
  
  output$rfte <- renderValueBox({
    valueBox(prettyNum(rfte(), big.mark = ",") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
             color = "aqua"
    )
  })
  

tabDip <- reactive(
  ricerca %>% 
    group_by(Dipartimento, tipologia) %>% 
    count(nr) %>%  
    summarise(n.articoli = n()) %>% 
    pivot_wider(names_from = tipologia, values_from = n.articoli) %>% 
    right_join(tabella[-5, ], by = "Dipartimento") %>% 
    select(N.esami, FTED, FTEC, FTET, RA, RVP, RAI, RT, "R/FTET", IF, Int, Naz)
)
  
output$t <- renderUI({
    flextable(tabDip()) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=18) %>% 
    fontsize(part = "header", size = 18) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = "," , digits = 2, prefix = "€") %>% 
    autofit() %>% 
    htmltools_value()
})




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
        select(n.articoli)), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
  })

output$Int <- renderValueBox({
  valueBox(
    (ric() %>% 
       filter(tipologia == "Int") %>% 
       select(n.articoli)), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})

output$Naz <- renderValueBox({
  valueBox(
    (ric() %>% 
       filter(tipologia == "Naz") %>% 
       select(n.articoli)), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
})

  
  }
