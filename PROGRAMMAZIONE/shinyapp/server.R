server <- function(input, output, session) { 
  
###Quadro Generale Dashboard#####

  
###value boxes######  
es <- reactive(tizsler %>% 
    filter(Dipartimento == "Totale") %>% 
    select("N.esami"))
  
output$esami <- renderValueBox({
    valueBox(prettyNum(es(), big.mark = "."), "N. esami",  icon = icon("flask"),
      color = "blue"
    )
  })
  
ra <- reactive(tizsler %>% 
  filter(Dipartimento == "Totale") %>% 
  select("RA"))

  output$ra <- renderValueBox({
    valueBox(prettyNum(ra(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
      color = "aqua"
    )
  })

  vp <- reactive(tizsler %>% 
                   filter(Dipartimento == "Totale") %>% 
                   select("RVP"))
  
  output$vp <- renderValueBox({
    valueBox(prettyNum(vp(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  
  ai <- reactive(tizsler %>% 
                   filter(Dipartimento == "Totale") %>% 
                   select("RAI"))
  
  output$ai <- renderValueBox({
    valueBox(prettyNum(ai(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rt <- reactive(tizsler %>% 
                   filter(Dipartimento == "Totale") %>% 
                   select("RT"))
  
  output$rt <- renderValueBox({
    valueBox(prettyNum(rt(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
             color = "aqua"
    )
  })
  
  rfte <- reactive(tizsler %>% 
                   filter(Dipartimento == "Totale") %>% 
                   select("R/FTET"))
  
  output$rfte <- renderValueBox({
    valueBox(prettyNum(rfte(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
             color = "aqua"
    )
  })

  # ric <- reactive({
  #   ricerca %>% 
  #     group_by(tipologia) %>% 
  #     count(nr) %>%
  #     summarise(n.articoli = n())
  # })
  
  
  # ric <- reactive({
  #   ricerca %>% 
  #   filter(IF == IF) %>% 
  #     group_by(nr) %>% 
  #     count(nr) %>% 
  #     select(nr) %>% 
  #     nrow()
  # })
  
  
  output$IF <- renderValueBox({
    valueBox(
      (ricerca %>% 
         filter(IF == "IF") %>% 
         group_by(nr) %>% 
         count(nr) %>% 
         select(nr) %>% 
         nrow()),  "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
  })
  
  
  output$Int <- renderValueBox({
    valueBox(
      (ricerca %>% 
         filter(INT == "Int") %>% 
         group_by(nr) %>% 
         count(nr) %>% 
         select(nr) %>% 
         nrow()
        # ric() %>% 
        #  filter(tipologia == "Int") %>% 
        #  select(n.articoli)
        ), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
  })
  
  output$Naz <- renderValueBox({
    valueBox(
      (ricerca %>% 
         filter(NAZ == "Naz") %>% 
         group_by(nr) %>% 
         count(nr) %>% 
         select(nr) %>% 
         nrow()
        
        
        # ric() %>% 
        #  filter(tipologia == "Naz") %>% 
        #  select(n.articoli))
      
      
      ), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
  })
  
  



###tabella x dipartimenti####
output$t <- renderUI({
  border <- officer::fp_border()
    flextable(tizsler) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
    autofit() %>% 
    color(j= "R/FTET", color = "red", part = "all") %>% 
    vline(j= "RT", border = border, part = "all") %>% 
      footnote(i=1, j=3:10,
               value = as_paragraph(
                 c("Full Time Equivalenti Dirigenza",
                   "Full Time Equivalenti Comparto",
                   "Full Time Equivalenti Totale",
                   "Ricavo da Analisi",
                   "Ricavo Vendita Prodotti",
                   "Ricavo Attività Interna",
                   "Ricavo Totale",
                   "Ricavo per Full Equivalenti Totale")
                 ),
                 ref_symbols = c("a","b","c","d","e","f","g","h"),
                 part = "header", inline = T) %>%
      fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
    htmltools_value()
})

####grafici benchmarking#########################################

tb <- reactive({tizsler %>% 
  filter(Dipartimento != "Totale") %>% 
  mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
         "RA" = round(100*(RA/sum(RA)),1), 
         "FTED" = round(100*(FTED/sum(FTED)),1), 
         "FTEC" = round(100*(FTEC/sum(FTEC)),1),
         "RVP" =round(100*(RVP/sum(RVP)),1), 
         "RAI" = round(100*(RAI/sum(RAI)), 1),
         "RT" = round(100*(RT/sum(RT)),1),
         "FTET" = round(100*(FTET/ sum(FTET)), 1)
         # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
  ) %>% 
  select(Dipartimento, Esami, "FTED", "FTEC", "FTET",   "RT") %>% 
  pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
  mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT")))
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
  ricerca %>% filter(IF == "IF") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})
 
output$articoli <- renderTable(paper())
 
###tabella modale convegni internazionali

Cint <- reactive({
  ricerca %>% filter(INT == "Int") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni <- renderTable(Cint())


###tabella modale convegni Nazionali

Cnaz <- reactive({
  ricerca %>% filter(NAZ == "Naz") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali <- renderTable(Cnaz())


###tabelle modali percentuali KPI

output$tbw <- renderTable(tb() %>% 
pivot_wider(names_from = "KPI", values_from = "valore"))







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
  border <- officer::fp_border()
  flextable(tdsa) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
    autofit() %>% 
    color(j= "R/FTET", color = "red", part = "all") %>%
    vline(j= "RT", border = border, part = "all") %>%
    footnote(i=1, j=3:10, 
             value = as_paragraph(
               c("Full Time Equivalenti Dirigenza",
                 "Full Time Equivalenti Comparto", 
                 "Full Time Equivalenti Totale",
                 "Ricavo da Analisi", 
                 "Ricavo Vendita Prodotti", 
                 "Ricavo Attività Interna",
                 "Ricavo Totale", 
                 "Ricavo per Full Equivalenti Totale"
               )
             ),
             ref_symbols = c("a","b", "c","d", "e", "f", "g","h"), 
             part = "header", inline = T
    ) %>%
    fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
    htmltools_value()
})





#### grafico benchmarking dsa####
tb2 <- reactive({tdsa %>% 
    filter(Reparto != "Totale") %>% 
    mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
           "RA" = round(100*(RA/sum(RA)),1), 
           "FTED" = round(100*(FTED/sum(FTED)),1), 
           "FTEC" = round(100*(FTEC/sum(FTEC)),1),
           "RVP" =round(100*(RVP/sum(RVP)),1), 
           "RAI" = round(100*(RAI/sum(RAI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTET/ sum(FTET)), 1) 
           # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
    ) %>% 
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT") %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
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


###tabelle modali percentuali KPI

output$tbw2 <- renderTable(tb2() %>% 
                            pivot_wider(names_from = "KPI", values_from = "valore"))



###DTSA####

###value boxes dtsa####
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



#### tabella x reparti dtsa######
output$t3 <- renderUI({
  border <- officer::fp_border()
  flextable(tdtsa) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
    autofit() %>% 
    color(j= "R/FTET", color = "red", part = "all") %>% 
    vline(j= "RT", border = border, part = "all") %>%   
    footnote(i=1, j=3:10, 
             value = as_paragraph(
               c("Full Time Equivalenti Dirigenza",
                 "Full Time Equivalenti Comparto", 
                 "Full Time Equivalenti Totale",
                 "Ricavo da Analisi", 
                 "Ricavo Vendita Prodotti", 
                 "Ricavo Attività Interna",
                 "Ricavo Totale", 
                 "Ricavo per Full Equivalenti Totale"
               )
             ),
             ref_symbols = c("a","b", "c","d", "e", "f", "g","h"), 
             part = "header", inline = T
    ) %>%
    fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
    htmltools_value()
})





#### grafico benchmarking dtsa####
tb3 <- reactive({tdtsa %>% 
    filter(Reparto != "Totale") %>% 
    mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
           "RA" = round(100*(RA/sum(RA)),1), 
           "FTED" = round(100*(FTED/sum(FTED)),1), 
           "FTEC" = round(100*(FTEC/sum(FTEC)),1),
           "RVP" =round(100*(RVP/sum(RVP)),1), 
           "RAI" = round(100*(RAI/sum(RAI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTET/ sum(FTET)), 1) 
           # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
    ) %>% 
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT")))
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


###tabelle modali percentuali KPI

output$tbw3 <- renderTable(tb3() %>% 
                            pivot_wider(names_from = "KPI", values_from = "valore"))




###ATLOMB####
###value boxes atlomb####
es4 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                  select("N.esami"))

output$esami4 <- renderValueBox({
  valueBox(prettyNum(es4(), big.mark = "."), "N. esami",  icon = icon("flask"),
           color = "blue"
  )
})

ra4 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                  select("RA"))

output$ra4 <- renderValueBox({
  valueBox(prettyNum(ra4(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
           color = "aqua"
  )
})

vp4 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                  select("RVP"))

output$vp4 <- renderValueBox({
  valueBox(prettyNum(vp4(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
           color = "aqua"
  )
})


ai4 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                  select("RAI"))

output$ai4 <- renderValueBox({
  valueBox(prettyNum(ai4(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
           color = "aqua"
  )
})

rt4 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                  select("RT"))

output$rt4 <- renderValueBox({
  valueBox(prettyNum(rt4(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
           color = "aqua"
  )
})

rfte4 <- reactive(tizsler %>% 
                    filter(Dipartimento == "Area Territoriale Lombardia") %>% 
                    select("R/FTET"))

output$rfte4 <- renderValueBox({
  valueBox(prettyNum(rfte4(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
           color = "aqua"
  )
})

ric4 <- reactive({
  ricerca %>%
    filter(Dipartimento == "Area Territoriale Lombardia") %>% 
    group_by(tipologia) %>% 
    count(nr) %>%
    summarise(n.articoli = n())
})

output$IF4 <- renderValueBox({
  valueBox(
    (ric4() %>% 
       filter(tipologia == "IF") %>% 
       select(n.articoli)), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
})


output$Int4 <- renderValueBox({
  valueBox(
    (ric4() %>% 
       filter(tipologia == "Int") %>% 
       select(n.articoli)), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})

output$Naz4 <- renderValueBox({
  valueBox(
    (ric4() %>% 
       filter(tipologia == "Naz") %>% 
       select(n.articoli)), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
})


#### tabella x reparti atlomb######
output$t4 <- renderUI({
  border <- officer::fp_border()
  flextable(tatlomb) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
    autofit() %>% 
    color(j= "R/FTET", color = "red", part = "all") %>% 
    vline(j= "RT", border = border, part = "all") %>% 
    footnote(i=1, j=3:10, 
             value = as_paragraph(
               c("Full Time Equivalenti Dirigenza",
                 "Full Time Equivalenti Comparto", 
                 "Full Time Equivalenti Totale",
                 "Ricavo da Analisi", 
                 "Ricavo Vendita Prodotti", 
                 "Ricavo Attività Interna",
                 "Ricavo Totale", 
                 "Ricavo per Full Equivalenti Totale"
               )
             ),
             ref_symbols = c("a","b", "c","d", "e", "f", "g","h"), 
             part = "header", inline = T
    ) %>%
    fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
    htmltools_value()
})

#### grafico benchmarking dtsa####
tb4 <- reactive({tatlomb %>% 
    filter(Reparto != "Totale") %>% 
    mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
           "RA" = round(100*(RA/sum(RA)),1), 
           "FTED" = round(100*(FTED/sum(FTED)),1), 
           "FTEC" = round(100*(FTEC/sum(FTEC)),1),
           "RVP" =round(100*(RVP/sum(RVP)),1), 
           "RAI" = round(100*(RAI/sum(RAI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTET/ sum(FTET)), 1) 
           # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
    ) %>% 
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
})

output$tbd4 <- renderPlot( 
  
  if(input$ind4 == "Reparto")
    
  {  
    
    ggplot(tb4(),  aes( 
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
    
    tb4() %>% 
      mutate(Reparto = recode(Reparto, "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "CR-MN", 
                              "SEDE TERRITORIALE DI BRESCIA" = "BS", 
                              "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "BG-BI-SO", 
                              "SEDE TERRITORIALE DI LODI - MILANO" = "LO-MI", 
                              "SEDE TERRITORIALE DI PAVIA" = "PV")) %>% 
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

### tabelle modali pubblicazioni e convegni atlomb####
paper4 <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    filter(Dipartimento == "Area Territoriale Lombardia") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})

output$articoli4 <- renderTable(paper4())


###tabella modale convegni internazionali

Cint4 <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    filter(Dipartimento == "Area Territoriale Lombardia") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni4 <- renderTable(Cint4())


###tabella modale convegni Nazionali

Cnaz4 <- reactive({
  ricerca %>% filter(tipologia == "Naz") %>% 
    filter(Dipartimento == "Area Territoriale Lombardia") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali4 <- renderTable(Cnaz4())


###tabelle modali percentuali KPI

output$tbw4 <- renderTable(tb4() %>% 
                            pivot_wider(names_from = "KPI", values_from = "valore"))




###ATER####
###value boxes ater####
es5 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                  select("N.esami"))

output$esami5 <- renderValueBox({
  valueBox(prettyNum(es5(), big.mark = "."), "N. esami",  icon = icon("flask"),
           color = "blue"
  )
})

ra5 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                  select("RA"))

output$ra5 <- renderValueBox({
  valueBox(prettyNum(ra5(), big.mark = ".") , "Ricavi per attività analitica", icon = icon("euro"),
           color = "aqua"
  )
})

vp5<- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                  select("RVP"))

output$vp5 <- renderValueBox({
  valueBox(prettyNum(vp5(), big.mark = ".") , "Ricavi per vendita prodotti", icon = icon("euro"),
           color = "aqua"
  )
})


ai5 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                  select("RAI"))

output$ai5 <- renderValueBox({
  valueBox(prettyNum(ai5(), big.mark = ".") , "Ricavi per attività interna", icon = icon("euro"),
           color = "aqua"
  )
})

rt5 <- reactive(tizsler %>% 
                  filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                  select("RT"))

output$rt5 <- renderValueBox({
  valueBox(prettyNum(rt5(), big.mark = ".") , "Ricavi totali", icon = icon("euro"),
           color = "aqua"
  )
})

rfte5 <- reactive(tizsler %>% 
                    filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
                    select("R/FTET"))

output$rfte5 <- renderValueBox({
  valueBox(prettyNum(rfte5(), big.mark = ".") , "Ricavo per Full Time Equivalente", icon = icon("euro"),
           color = "aqua"
  )
})

ric5 <- reactive({
  ricerca %>%
    filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
    group_by(tipologia) %>% 
    count(nr) %>%
    summarise(n.articoli = n())
})

output$IF5 <- renderValueBox({
  valueBox(
    (ric5() %>% 
       filter(tipologia == "IF") %>% 
       select(n.articoli)), "Articoli pubblicati su riviste peer-review con IF", icon = icon("book"), color = "light-blue")
})


output$Int5 <- renderValueBox({
  valueBox(
    (ric5() %>% 
       filter(tipologia == "Int") %>% 
       select(n.articoli)), "Lavori presentati a convegni internazionali", icon = icon("book"), color = "light-blue")
})

output$Naz5 <- renderValueBox({
  valueBox(
    (ric5() %>% 
       filter(tipologia == "Naz") %>% 
       select(n.articoli)), "Lavori presentati a convegni nazionali", icon = icon("book"), color = "light-blue")
})

#### tabella x reparti ater######
output$t5 <- renderUI({
  border <- officer::fp_border()
  flextable(tater) %>%
    theme_booktabs() %>% 
    color(i = 1, color = "blue", part = "header") %>% 
    bold( part = "header") %>% 
    fontsize(size=15) %>% 
    fontsize(part = "header", size = 15) %>% 
    line_spacing(space = 2.5) %>% 
    colformat_num(j = c( "RA", "RVP", "RAI", "RT", "R/FTET"), big.mark = ".", decimal.mark = ",", digits = 2, prefix = "€") %>% 
    autofit() %>% 
    color(j= "R/FTET", color = "red", part = "all") %>% 
    vline(j= "RT", border = border, part = "all") %>% 
    footnote(i=1, j=3:10, 
             value = as_paragraph(
               c("Full Time Equivalenti Dirigenza",
                 "Full Time Equivalenti Comparto", 
                 "Full Time Equivalenti Totale",
                 "Ricavo da Analisi", 
                 "Ricavo Vendita Prodotti", 
                 "Ricavo Attività Interna",
                 "Ricavo Totale", 
                 "Ricavo per Full Equivalenti Totale"
               )
             ),
             ref_symbols = c("a","b", "c","d", "e", "f", "g","h"), 
             part = "header", inline = T
    ) %>%
    fontsize( i = NULL, j = NULL, size = 13, part = "footer") %>% 
    htmltools_value()
})

#### grafico benchmarking dtsa####
tb5 <- reactive({tater %>% 
    filter(Reparto != "Totale") %>% 
    mutate(Esami = round(100*(N.esami/sum(N.esami)), 1), 
           "RA" = round(100*(RA/sum(RA)),1), 
           "FTED" = round(100*(FTED/sum(FTED)),1), 
           "FTEC" = round(100*(FTEC/sum(FTEC)),1),
           "RVP" =round(100*(RVP/sum(RVP)),1), 
           "RAI" = round(100*(RAI/sum(RAI)), 1),
           "RT" = round(100*(RT/sum(RT)),1),
           "FTET" = round(100*(FTET/ sum(FTET)), 1) 
           # "Ricavo per FTE" = round(100*(`R/FTET`/sum(`R/FTET`)), 1)
    ) %>% 
    select(Reparto, Esami, "FTED", "FTEC", "FTET",   "RT" ) %>% 
    pivot_longer(!Reparto, names_to = "KPI", values_to = "valore") %>% 
    mutate(KPI = factor(KPI, levels = c("Esami", "FTED", "FTEC", "FTET", "RT"  )))
})

output$tbd5 <- renderPlot( 
  
  if(input$ind5 == "Reparto")
    
  {  
    
    ggplot(tb5(),  aes( 
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
    
    tb5() %>% 
      mutate(Reparto = recode(Reparto, "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "BO-MO-FE", 
                              "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "FO-RA", 
                              "SEDE TERRITORIALE DI PIACENZA - PARMA" = "PC-PR", 
                              "SEDE TERRITORIALE DI REGGIO EMILIA" = "RE")) %>% 
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

### tabelle modali pubblicazioni e convegni ater####
paper5 <- reactive({
  ricerca %>% filter(tipologia == "IF") %>% 
    filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
    select("AUTORI" = autori, "JOURNAL" = `TITOLO RIVISTA`, "TITOLO" = titinglese) %>% 
    unique()
})

output$articoli5 <- renderTable(paper5())


###tabella modale convegni internazionali

Cint5 <- reactive({
  ricerca %>% filter(tipologia == "Int") %>% 
    filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$convegni5 <- renderTable(Cint5())


###tabella modale convegni Nazionali

Cnaz5 <- reactive({
  ricerca %>% filter(tipologia == "Naz") %>% 
    filter(Dipartimento == "Area Territoriale Emilia Romagna") %>% 
    select("AUTORI" = autori, "CONGRESSO" = convegno, "TITOLO" = titinglese) %>% 
    unique()
  
})

output$nazionali5 <- renderTable(Cnaz5())


###tabelle modali percentuali KPI

output$tbw5 <- renderTable(tb5() %>% 
                            pivot_wider(names_from = "KPI", values_from = "valore"))


}
