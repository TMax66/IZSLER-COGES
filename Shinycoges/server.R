
server <- function(input, output, session) {

####SEZIONE ESAMI####    
  output$attv <- renderValueBox({
    valueBox(
      prettyNum(sum(r$esami), big.mark = ","), 
      "#Totale esami ", icon = icon("keyboard"),
      color = "red"
    )
  })
  
  output$attuff <- renderValueBox({
    valueBox(
      riepilogo %>% 
        filter(Attività=="Ufficiale") %>% 
      summarise(prettyNum(sum(N.esami), big.mark = ",")), 
      "#Attività Ufficiale", icon = icon("keyboard"),
      color = "yellow"
    )
  })
  
  
  output$attnuff <- renderValueBox({
    valueBox(
      riepilogo %>% 
        filter(Attività=="Non Ufficiale") %>% 
        summarise(prettyNum(sum(N.esami), big.mark = ",")), 
      "#Attività Non Ufficiale", icon = icon("keyboard"),
      color = "blue"
    )
  })
  
  
med<-reactive({
  riepilogo %>% 
    group_by(Reparto, Attività) %>% 
    filter(Attività==input$attività) %>% 
    summarise(esami=round(sum(N.esami/1000),0), ricavi=round(sum(Valore/1000),0)) %>% 
    ungroup %>% 
    arrange(esami) %>% 
    mutate(Reparto = factor(Reparto, unique(Reparto))) %>% 
    summarise(M=median(esami*1000))
  
})
  

output$nesami<-renderPlot({  
  
  if (input$attività=="all")
  {
    dt %>% 
      group_by(Reparto ) %>% 
      summarise(esami=round(sum(esami/1000),0), ricavi=round(sum(Rtot/1000),0)) %>% 
      arrange(esami) %>% 
      mutate(Reparto = factor(Reparto, unique(Reparto))) %>% 
      ggplot(aes(x=Reparto,y=esami, label=esami))+
      geom_segment( aes(x=Reparto, xend=Reparto, y=0, yend=esami), color="black")+  
      geom_point( aes(x=Reparto, y=esami, size=ricavi),shape=21,color="darkblue" )+
      scale_size("Ricavi (€x1000)",  range=c(5, 25))+
      geom_text(color="black", size=4)+
      coord_flip()+
      theme_ipsum_rc(axis_title_just = "mc")+
      labs(y="N.esamix1000 ",x="")+
      geom_hline(yintercept= round(235, 0), col="red")+
      geom_label(
        label="Mediana (234565 esami)", 
        x=5,
        y=300,
        label.padding = unit(0.55, "lines"), # Rectangle size around label
        label.size = 0.35,
        color = "black",
        fill="white"
      )
  }
  else
  
  {   
  riepilogo %>% 
    group_by(Reparto, Attività) %>% 
    filter(Attività==input$attività) %>% 
    summarise(esami=round(sum(N.esami/1000),0), ricavi=round(sum(Valore/1000),0)) %>% 
    ungroup %>% 
    arrange(esami) %>% 
    mutate(Reparto = factor(Reparto, unique(Reparto))) %>% 
    ggplot(aes(x=Reparto,y=esami, label=esami))+
    geom_segment( aes(x=Reparto, xend=Reparto, y=0, yend=esami), color="black")+  
    geom_point( aes(x=Reparto, y=esami, size=ricavi),shape=21,color="darkblue" )+
    scale_size("Ricavi (€x1000)",  range=c(5, 25))+
    geom_text(color="black", size=4)+
    coord_flip()+
    theme_ipsum_rc(axis_title_just = "mc")+
    labs(y="N.esamix1000 ",x="")+
    geom_hline(yintercept= round(104, 0), col="red")+
    geom_label(data=med(),
      aes(label=paste("Mediana N.esami", M)), 
      x=5,
      y=300,
      label.padding = unit(0.55, "lines"), # Rectangle size around label
      label.size = 0.35,
      color = "black",
      fill="white"
    )
  } 
  
})
  
#sezione RICAVI######
    
output$rict <- renderValueBox({
  valueBox(
    prettyNum(sum(dt$ricavi), big.mark = ","), 
    "Totale Ricavi € ",
    color = "red"
  )
})

output$ricuff <- renderValueBox({
  valueBox(
    riepilogo %>% 
      filter(Attività=="Ufficiale") %>% 
      summarise(prettyNum(sum(Valore),big.mark = ",")), 
    "Ricavi da Attività Ufficiale €",
    color = "yellow"
  )
})

output$ricnuff <- renderValueBox({
  valueBox(
    riepilogo %>% 
      filter(Attività=="Non Ufficiale") %>% 
      summarise(prettyNum(sum(Valore),big.mark = ",")), 
    "Ricavi da Attività Non Ufficiale €",
    color = "yellow"
  )
})


output$vprod <- renderValueBox({
  valueBox(
    riepilogo %>% 
      select(Reparto,`Vendita Prodotti`) %>% 
      drop_na(`Vendita Prodotti`) %>% 
      summarise(prettyNum(sum(`Vendita Prodotti`), big.mark = ",")), 
    "Ricavi da Vendita Prodotti €",
    color = "yellow"
  )
})

output$attint <- renderValueBox({
  valueBox(
    riepilogo %>% 
      select(Reparto,`Attività Interna`) %>% 
      drop_na(`Attività Interna`)  %>% 
      summarise(prettyNum(round(sum(`Attività Interna`),0), big.mark = ",")), 
    "Ricavi da Attività Interna €",
    color = "yellow"
  )
})



# ricMed<-reactive({
#   riepilogo %>% 
#     group_by(Reparto, Attività) %>% 
#     filter(Attività==input$attività) %>% 
#     summarise(esami=round(sum(N.esami/1000),0), ricavi=round(sum(Valore/1000),0)) %>% 
#     ungroup %>% 
#     arrange(esami) %>% 
#     mutate(Reparto = factor(Reparto, unique(Reparto))) %>% 
#     summarise(M=median(esami*1000))
#   
# })



}


# r<-riepilogo %>% 
#   group_by(Reparto) %>% 
#   summarise(esami=round(sum(N.esami/1000),0), ricavi=round(sum(Valore/1000),0))  
# 
# vp<-riepilogo %>% 
#   select(Reparto,`Vendita Prodotti`) %>% 
#   drop_na(`Vendita Prodotti`) 
# 
# rvp<-left_join(r,vp)
# 

  