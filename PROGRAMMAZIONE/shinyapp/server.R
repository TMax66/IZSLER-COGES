server<-function(input, output) { 

#### Dipartimento ##### 
  dt <-reactive(
    dati %>% ungroup() %>% 
      group_by(Dipartimento) %>% 
      summarise(hworked = sum(hworked)/1000, 
                hprev = sum(hprev)/1000, 
                esami = sum(esami)/1000, 
                ricavi = sum (ricavi)/1000) %>% 
      mutate("FTEpr" = (1000*hprev)/(36*45.6), 
             "FTEr" = (1000*hworked)/(36*45.6),
             "Perchwd" = 100*(hworked/hprev),
             "tempo medio esame" = 60*(hworked/esami)) %>% 
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>%  
      filter(indicatore==input$indicatore))
  
  
  output$diplot <-renderPlot({
    dt() %>%
      arrange(valore) %>%
      mutate(Dipartimento = factor(Dipartimento, levels = .$Dipartimento)) %>%
      mutate(media = mean(valore, na.rm = TRUE)) %>%  
      
      ggplot(aes(x=Dipartimento, y=valore, label = round(valore, 1)))+
      geom_point( aes(x=Dipartimento, y=valore),color="lightblue", size = 15 )+
      geom_text(color = "black", size = 5)+
      geom_segment(aes(y = media, x = Dipartimento, yend = valore, xend = Dipartimento ),color = "grey50") +
      coord_flip() +
      theme_ipsum_rc(axis_title_just = "mc")+
      labs(y=input$indicatore,x="")+
      theme(axis.text.x = element_text(color="blue", size=12, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=12, face="bold"))+
      theme(axis.title.x = element_text(size = 12, face = "bold"))
  })
  
  dtx <-reactive(dati %>% ungroup() %>% 
                   group_by(Dipartimento) %>% 
                   summarise(hworked = sum(hworked), 
                             hprev = sum(hprev), 
                             esami = sum(esami), 
                             ricavi = sum (ricavi)) %>% 
                   mutate("FTE-previsto" = hprev/(36*45.6), 
                          "FTE-reale" = hworked/(36*45.6),
                          "%tempo-utilizzato" = 100*(hworked/hprev),
                          "tempo-medio-esame" = 60*(hworked/esami)) %>% 
                   mutate(across(where(is.numeric), function(x) scale(x)))
  )
  
  output$dipheat <- renderPlot(
    dtx() %>% 
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
      mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "%tempo-utilizzato",
                                                        "FTE-previsto", "FTE-reale", "tempo-medio-esame"))) %>%
      ggplot(aes(Dipartimento,indicatore, label = round(valore,1))) + 
      geom_tile(aes(fill = valore), colour = "white") + geom_text(color = "black", size = 5)+
      scale_fill_gradient(low = "snow2", high = "steelblue")+theme_grey(base_size = 18) +
      coord_flip()+labs(x = "", y = "", title="") + theme(legend.position = "none") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0), position = "right")+
      theme(axis.text.x = element_text(color="blue", size=10, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=10, face="bold"))
  )
  
  output$tdip <- renderTable(
    dati %>% ungroup() %>%
      group_by(Dipartimento) %>%
      summarise(hworked = sum(hworked),
                hprev = sum(hprev),
                esami = sum(esami),
                ricavi = sum (ricavi)) %>%
      mutate("FTE-previsto" = hprev/(36*45.6),
             "FTE-reale" = hworked/(36*45.6),
             "%tempo-utilizzato" = 100*(hworked/hprev),
             "tempo-medio-esame" = 60*(hworked/esami))
  )
    
  ####Dipartimento/Reparti#####
  
  dt3 <-reactive(
    dati %>% ungroup() %>%
      filter(Dipartimento == input$dip) %>%
      group_by(Reparto) %>%
      summarise(hworked = sum(hworked)/1000,
                hprev = sum(hprev)/1000,
                esami = sum(esami)/1000,
                ricavi = sum (ricavi)/1000) %>%
      mutate("FTEpr" = (1000*hprev)/(36*45.6),
             "FTEr" = (1000*hworked)/(36*45.6),
             "Perchwd" = 100*(hworked/hprev),
             "tempo medio esame" = 60*(hworked/esami)) %>%
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>%
      filter(indicatore==input$indicatore2))
  
  
  output$drplot <- renderPlot({
    
    dt3() %>%
      arrange(valore) %>%
      mutate(Reparto = factor(Reparto, levels = .$Reparto)) %>%
      mutate(mediana = median(valore, na.rm = TRUE)) %>%
      ggplot(aes(x=Reparto, y=valore, label = round(valore, 1)))+
      geom_point( aes(x=Reparto, y=valore),color="lightblue", size = 15 )+
      geom_text(color = "black", size = 5)+
      geom_segment(aes(y = mediana, x = Reparto, yend = valore, xend = Reparto ),color = "grey50") +
      coord_flip() +
      theme_ipsum_rc(axis_title_just = "mc")+
      labs(y=input$indicatore2,x="")+
      theme(axis.text.x = element_text(color="blue", size=12, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=12, face="bold"))+
      theme(axis.title.x = element_text(size = 12, face = "bold"))
  })
  
  
  dtx3 <-reactive(dati %>% ungroup() %>% 
                    filter(Dipartimento == input$dip) %>%
                    group_by(Reparto) %>% 
                    summarise(hworked = sum(hworked), 
                              hprev = sum(hprev), 
                              esami = sum(esami), 
                              ricavi = sum (ricavi)) %>% 
                    mutate("FTE-previsto" = hprev/(36*45.6), 
                           "FTE-reale" = hworked/(36*45.6),
                           "%tempo-utilizzato" = 100*(hworked/hprev),
                           "tempo-medio-esame" = 60*(hworked/esami)) %>% 
                    mutate(across(where(is.numeric), function(x) scale(x)))
  )
  
  output$drheat <- renderPlot(
    dtx3() %>% 
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
      mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "%tempo-utilizzato",
                                                        "FTE-previsto", "FTE-reale", "tempo-medio-esame"))) %>%
      ggplot(aes(Reparto,indicatore, label = round(valore,1))) + 
      geom_tile(aes(fill = valore), colour = "white") + geom_text(color = "black", size = 5)+
      scale_fill_gradient(low = "snow2", high = "steelblue")+theme_grey(base_size = 18) +
      coord_flip()+labs(x = "", y = "", title="") + theme(legend.position = "none") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0), position = "right")+
      theme(axis.text.x = element_text(color="blue", size=10, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=10, face="bold"))
  )
  
  output$tdiprep <- renderTable(
    dati %>% ungroup() %>%
      filter(Dipartimento == input$dip) %>% 
      group_by(Reparto) %>%
      summarise(hworked = sum(hworked),
                hprev = sum(hprev),
                esami = sum(esami),
                ricavi = sum (ricavi)) %>%
      mutate("FTE-previsto" = hprev/(36*45.6),
             "FTE-reale" = hworked/(36*45.6),
             "%tempo-utilizzato" = 100*(hworked/hprev),
             "tempo-medio-esame" = 60*(hworked/esami))
  )
  
####Dipartimento/Laboratori

  dt5 <-reactive(
    dati %>% ungroup() %>%
      filter(Dipartimento == input$dip2) %>%
      group_by(Laboratorio) %>%
      summarise(hworked = sum(hworked)/1000,
                hprev = sum(hprev)/1000,
                esami = sum(esami)/1000,
                ricavi = sum (ricavi)/1000) %>%
      mutate("FTEpr" = (1000*hprev)/(36*45.6),
             "FTEr" = (1000*hworked)/(36*45.6),
             "Perchwd" = 100*(hworked/hprev),
             "tempo medio esame" = 60*(hworked/esami)) %>%
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>%
      filter(indicatore==input$indicatore3))
  
  
  
  output$dlplot <- renderPlot({
    dt5() %>%
      arrange(valore) %>%
      mutate(Laboratorio = factor(Laboratorio, levels = .$Laboratorio)) %>%
      mutate(media = mean(valore, na.rm = TRUE)) %>%
      ggplot(aes(x=Laboratorio, y=valore, label = round(valore, 1)))+
      geom_point( aes(x=Laboratorio, y=valore),color="lightblue", size = 15 )+
      geom_text(color = "black", size = 5)+
      geom_segment(aes(y = media, x = Laboratorio, yend = valore, xend = Laboratorio ),color = "grey50") +
      coord_flip() +
      theme_ipsum_rc(axis_title_just = "mc")+
      labs(y=input$indicatore3,x="")+
      theme(axis.text.x = element_text(color="blue", size=12, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=12, face="bold"))+
      theme(axis.title.x = element_text(size = 12, face = "bold"))
  })
  
  
  dtx4 <-reactive(dati %>% ungroup() %>% 
                    filter(Dipartimento == input$dip2) %>%
                    group_by(Laboratorio) %>% 
                    summarise(hworked = sum(hworked)/1000, 
                              hprev = sum(hprev)/1000, 
                              esami = sum(esami), 
                              ricavi = sum (ricavi)) %>% 
                    mutate("FTE-previsto" = (1000*hprev)/(36*45.6), 
                           "FTE-reale" = (1000*hworked)/(36*45.6),
                           "%tempo-utilizzato" = 100*(hworked/hprev),
                           "tempo-medio-esame" = 60*(hworked/esami)) %>% 
                    mutate(across(where(is.numeric), function(x) scale(x)))
  )
  
  output$dlheat <- renderPlot(
    dtx4() %>% 
      pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
      mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "%tempo-utilizzato",
                                                        "FTE-previsto", "FTE-reale", "tempo-medio-esame"))) %>%
      ggplot(aes(Laboratorio,indicatore, label = round(valore,1))) + 
      geom_tile(aes(fill = valore), colour = "white") + geom_text(color = "black", size = 5)+
      scale_fill_gradient(low = "snow2", high = "steelblue")+theme_grey(base_size = 18) +
      coord_flip()+labs(x = "", y = "", title="") + theme(legend.position = "none") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0), position = "right")+
      theme(axis.text.x = element_text(color="blue", size=10, face="bold"))+
      theme(axis.text.y = element_text(color="blue", size=10, face="bold"))
  )
  
  output$tdiplab <- renderTable(
    dati %>% ungroup() %>%
      filter(Dipartimento == input$dip2) %>% 
      group_by(Laboratorio) %>%
      summarise(hworked = sum(hworked),
                hprev = sum(hprev),
                esami = sum(esami),
                ricavi = sum (ricavi)) %>%
      mutate("FTE-previsto" = hprev/(36*45.6),
             "FTE-reale" = hworked/(36*45.6),
             "%tempo-utilizzato" = 100*(hworked/hprev),
             "tempo-medio-esame" = 60*(hworked/esami))
  )

  #### DATASET####
 
  output$dataset <- DT::renderDataTable(dati)
 

###PivotTable####

output$test<- rpivotTable::renderRpivotTable({
  rpivotTable(dati)
})

}
