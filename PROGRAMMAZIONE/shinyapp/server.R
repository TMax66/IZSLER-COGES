server <- function(input, output, session) { 
  
###Quadro Generale Dashboard#####
  
  es <- reactive(tizsler %>% 
    filter(Dipartimento == "Total") %>% 
    select("N.esami"))
  
output$esami <- renderValueBox({
    valueBox(prettyNum(es(), big.mark = "."), "N. esami",  icon = icon("flask"),
      color = "blue"
    )
  })
  
ra <- reactive(tizsler %>% 
  filter(Dipartimento == "Total") %>% 
  select("RA"))

  output$ra <- renderValueBox({
    valueBox(prettyNum(ra(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
      color = "aqua"
    )
  })

  vp <- reactive(tizsler %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RVP"))
  
  output$vp <- renderValueBox({
    valueBox(prettyNum(vp(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  
  ai <- reactive(tizsler %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RAI"))
  
  output$ai <- renderValueBox({
    valueBox(prettyNum(ai(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rt <- reactive(tizsler %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("RT"))
  
  output$rt <- renderValueBox({
    valueBox(prettyNum(rt(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rfte <- reactive(tizsler %>% 
                   filter(Dipartimento == "Total") %>% 
                   select("R/FTET"))
  
  output$rfte <- renderValueBox({
    valueBox(prettyNum(rfte(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
             color = "aqua"
    )
  })
  

tabDip <- reactive(
  ricerca %>% 
    group_by(Dipartimento, tipologia) %>% 
    count(nr) %>%  
    summarise(n.articoli = n()) %>% 
    pivot_wider(names_from = tipologia, values_from = n.articoli) %>% 
    right_join(tizsler, by = "Dipartimento") %>% 
    select(N.esami, FTED, FTEC, FTET, RA, RVP, RAI, RT, "R/FTET")
)
  
output$t <- renderUI({
    flextable(tabDip()) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
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


####grafico benchmarking#########################################

tb <- reactive({tizsler %>% 
  filter(Dipartimento != "Total") %>% 
  mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
         "RA" = round(100*(RA/sum(RA)),1), 
         "FTED" = round(100*(FTED/sum(FTED)),1), 
         "FTEC" = round(100*(FTEC/sum(FTEC)),1),
         "RVP" =round(100*(RVP/sum(RVP)),1), 
         "RAI" = round(100*(RAI/sum(RAI)), 1),
         "RT" = round(100*(RT/sum(RT)),1),
         "FTET" = round(100*(FTET/ sum(FTET)), 1),
         "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
  ) %>% 
  select(Dipartimento, Esami, "FTED", "FTEC", "FTET",   "RT", "Ricavo per FTE") %>% 
  pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
  mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT", "Ricavo per FTE"  )))
  })



output$tbd <- renderPlot( 
  
  if(input$ind == "Dipartimento")
    
  {
  
   ggplot(tb(),  aes( 
    x = KPI, 
    y = valore, 
    fill = KPI
  )) + geom_col(width = 0.9, color = "black")+
  coord_polar(theta = "x")+ facet_wrap(~Dipartimento, nrow = 1)+
  scale_fill_brewer(palette = "Blues")+
  geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
    theme(legend.position = "blank",
          panel.background= element_blank(),
          plot.background = element_blank(), 
          strip.text.x = element_text(size = 15, colour = "blue"), 
          axis.text.x = element_text(size = 10, color = "black"))+
    labs(x = "", y = "") 
 
}

else
  
{
   tb() %>% 
    mutate(Dipartimento = recode(Dipartimento, "Dipartimento Sicurezza Alimentare" = "DSA", 
                                 "Dipartimento Tutela e  Salute Animale" = "DTSA", 
                                 "Area Territoriale Lombardia" = "ATLOMB", 
                                 "Area Territoriale Emilia Romagna" = "ATER")) %>% 
    ggplot(aes( 
      x = Dipartimento, 
      y = valore, 
      fill = Dipartimento
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~KPI, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 15, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "")

}, bg = "transparent")
###############################################################################################
 

####tabella modale pubblicazioni#####

paper <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})
 
output$articoli <- renderTable(paper())
 


####tabella modale convegni####

Cint <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$articoli <- renderTable(Cint())

#####DSA#####

es2 <- reactive(tizsler %>% 
                 filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                 select("N.esami"))

output$esami2 <- renderValueBox({
  valueBox(prettyNum(es2(), big.mark = "."), "N. esami",  icon = icon("flask"),
           color = "blue"
  )
})

ra2 <- reactive(tizsler %>% 
                 filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                 select("RA"))

output$ra2 <- renderValueBox({
  valueBox(prettyNum(ra2(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
           color = "aqua"
  )
})

vp2 <- reactive(tizsler %>% 
                 filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                 select("RVP"))

output$vp2 <- renderValueBox({
  valueBox(prettyNum(vp2(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
           color = "aqua"
  )
})


ai2 <- reactive(tizsler %>% 
                 filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                 select("RAI"))

output$ai2 <- renderValueBox({
  valueBox(prettyNum(ai2(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
           color = "aqua"
  )
})

rt2 <- reactive(tizsler %>% 
                 filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                 select("RT"))

output$rt2 <- renderValueBox({
  valueBox(prettyNum(rt2(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
           color = "aqua"
  )
})

rfte2 <- reactive(tizsler %>% 
                   filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
                   select("R/FTET"))

output$rfte2 <- renderValueBox({
  valueBox(prettyNum(rfte2(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
           color = "aqua"
  )
})
















}
