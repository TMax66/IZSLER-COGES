server <- function(input, output, session) { 
  
###Quadro Generale Dashboard#####

  
###value boxes######  
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
  
  



###tabella x dipartimenti####
output$t <- renderUI({
    flextable(tizsler) %>%
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

####grafici benchmarking#########################################

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

####tabelle modali pubblicazioni e convegni####################################################

paper <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})
 
output$articoli <- renderTable(paper())
 
###tabella modale convegni internazionali

Cint <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni <- renderTable(Cint())


###tabella modale convegni Nazionali

Cnaz <- reactive({
  ricerca %>% filter(tipologia == "Naz") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali <- renderTable(Cnaz())








#####DSA#####

###value boxes dsa####
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

ric2 <- reactive({
  ricerca %>%
    filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
    group_by(tipologia) %>% 
    count(nr) %>%
    summarise(n.articoli = n())
})

output$IF2 <- renderValueBox({
  valueBox(
    (ric2() %>% 
       filter(tipologia == "IF") %>% 
       select(n.articoli)), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
})


output$Int2 <- renderValueBox({
  valueBox(
    (ric2() %>% 
       filter(tipologia == "Int") %>% 
       select(n.articoli)), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})

output$Naz2 <- renderValueBox({
  valueBox(
    (ric2() %>% 
       filter(tipologia == "Naz") %>% 
       select(n.articoli)), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
})



#### tabella x reparti dsa######
output$t2 <- renderUI({
  flextable(tdsa) %>%
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





#### grafico benchmarking dsa####
tb2 <- reactive({tdsa %>% 
    filter(Reparto != "Total") %>% 
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
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT", "Ricavo per FTE") %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT", "Ricavo per FTE"  )))
})

output$tbd2 <- renderPlot( 
  
  if(input$ind2 == "Reparto")
  
  {  
  
  ggplot(tb2(),  aes( 
    x = KPI, 
    y = valore, 
    fill = KPI
  )) + geom_col(width = 0.9, color = "black")+
    coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
    scale_fill_brewer(palette = "Blues")+
    geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
    theme(legend.position = "blank",
          panel.background= element_blank(),
          plot.background = element_blank(), 
          strip.text.x = element_text(size = 12, colour = "blue"), 
          axis.text.x = element_text(size = 10, color = "black"))+
    labs(x = "", y = "") 
 
}

else

{
  
  tb2() %>% 
    mutate(Reparto = recode(Reparto, "REPARTO PRODUZIONE PRIMARIA" = "RPP", 
                                 "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "RChAM", 
                                 "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "RChAB", 
                                 "REPARTO CONTROLLO ALIMENTI" = "RCA")) %>% 
    ggplot(aes( 
      x = Reparto, 
      y = valore, 
      fill = Reparto
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


### tabelle modali pubblicazioni e convegni dsa####
paper2 <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})

output$articoli2 <- renderTable(paper2())


###tabella modale convegni internazionali

Cint2 <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni2 <- renderTable(Cint2())


###tabella modale convegni Nazionali

Cnaz2 <- reactive({
  ricerca %>% filter(tipologia == "Naz") %>% 
    filter(Dipartimento == "Dipartimento Sicurezza Alimentare") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali2 <- renderTable(Cnaz2())





###DTSA####

###value boxes dsa####
es3 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                  select("N.esami"))

output$esami3 <- renderValueBox({
  valueBox(prettyNum(es3(), big.mark = "."), "N. esami",  icon = icon("flask"),
           color = "blue"
  )
})

ra3 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                  select("RA"))

output$ra3 <- renderValueBox({
  valueBox(prettyNum(ra3(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
           color = "aqua"
  )
})

vp3 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                  select("RVP"))

output$vp3 <- renderValueBox({
  valueBox(prettyNum(vp3(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
           color = "aqua"
  )
})


ai3 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                  select("RAI"))

output$ai3 <- renderValueBox({
  valueBox(prettyNum(ai3(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
           color = "aqua"
  )
})

rt3 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                  select("RT"))

output$rt3 <- renderValueBox({
  valueBox(prettyNum(rt3(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
           color = "aqua"
  )
})

rfte3 <- reactive(tizsler %>% 
                    filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
                    select("R/FTET"))

output$rfte3 <- renderValueBox({
  valueBox(prettyNum(rfte3(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
           color = "aqua"
  )
})

ric3 <- reactive({
  ricerca %>%
    filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
    group_by(tipologia) %>% 
    count(nr) %>%
    summarise(n.articoli = n())
})

output$IF3 <- renderValueBox({
  valueBox(
    (ric3() %>% 
       filter(tipologia == "IF") %>% 
       select(n.articoli)), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
})


output$Int3 <- renderValueBox({
  valueBox(
    (ric3() %>% 
       filter(tipologia == "Int") %>% 
       select(n.articoli)), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})

output$Naz3 <- renderValueBox({
  valueBox(
    (ric3() %>% 
       filter(tipologia == "Naz") %>% 
       select(n.articoli)), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
})



#### tabella x reparti dsa######
output$t3 <- renderUI({
  flextable(tdtsa) %>%
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





#### grafico benchmarking dsa####
tb3 <- reactive({tdtsa %>% 
    filter(Reparto != "Total") %>% 
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
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT", "Ricavo per FTE") %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT", "Ricavo per FTE"  )))
})

output$tbd3 <- renderPlot( 
  
  if(input$ind3 == "Reparto")
    
  {  
    
    ggplot(tb3(),  aes( 
      x = KPI, 
      y = valore, 
      fill = KPI
    )) + geom_col(width = 0.9, color = "black")+
      coord_polar(theta = "x")+ facet_wrap(~Reparto, nrow = 1)+
      scale_fill_brewer(palette = "Blues")+
      geom_text(aes(y = valore-8, label = paste0(valore, "%")), color = "black", size=3)+
      theme(legend.position = "blank",
            panel.background= element_blank(),
            plot.background = element_blank(), 
            strip.text.x = element_text(size = 12, colour = "blue"), 
            axis.text.x = element_text(size = 10, color = "black"))+
      labs(x = "", y = "") 
    
  }
  
  else
    
  {
    
    tb3() %>% 
      mutate(Reparto = recode(Reparto, "REPARTO VIROLOGIA" = "RVIR", 
                              "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "RVVPB", 
                              "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "RPCB", 
                              "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "RTBA")) %>% 
      ggplot(aes( 
        x = Reparto, 
        y = valore, 
        fill = Reparto
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


### tabelle modali pubblicazioni e convegni dsa####
paper3 <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})

output$articoli3 <- renderTable(paper3())


###tabella modale convegni internazionali

Cint3 <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni3 <- renderTable(Cint3())


###tabella modale convegni Nazionali

Cnaz3 <- reactive({
  ricerca %>% filter(tipologia == "Naz") %>% 
    filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali3 <- renderTable(Cnaz3())


}
