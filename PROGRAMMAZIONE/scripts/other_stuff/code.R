library("here")
library("tidyverse")
library("hrbrthemes")
library("knitr")
library("kableExtra")
library("formattable")
library("ztable")
library("knitr")
dati <- readRDS( here("programmazione", "data", "processed", "dati.rds"))

###Dipartimento

dati %>% ungroup() %>% 
  group_by(Dipartimento) %>% 
  summarise(hworked = sum(hworked), 
            hprev = sum(hprev), 
            esami = sum(esami), 
            ricavi = sum (ricavi)) %>% 
  mutate("FTE-previsto" = hprev/(36*45.6), 
         "FTE-reale" = hworked/(36*45.6),
         "%tempo-utilizzato" = 100*(hworked/hprev),
         "tempo-medio-esame" = 60*(hworked/esami)) %>% 
  pivot_longer(2:8, names_to = "indicatore", values_to ="valore") %>% 
  filter(indicatore=="tempo-medio-esame") %>% 
  arrange(valore) %>%
  mutate(Dipartimento = factor(Dipartimento, levels = .$Dipartimento)) %>%
  mutate(mediana = median(valore, na.rm = TRUE)) %>% 
  ggplot(aes(Dipartimento, valore, label = round(valore, 1) ))+
  geom_point( aes(x=Dipartimento, y=valore),color="lightblue", size = 8 )+
  geom_text(color = "black", size = 2.5)+
  geom_segment(aes(y = mediana, x = Dipartimento, yend = valore, xend = Dipartimento ),color = "grey50") +
  coord_flip() +
  theme_ipsum_rc(axis_title_just = "mc")+
  labs(y="FTE-reale",x="")+
  geom_hline(yintercept= round(88.7, 0), col="red")+
  geom_label(
    label="Mediana (89%)",
    x=25,
    y=25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="red"
  )+
  theme(axis.text.x = element_text(color="blue", size=8, face="bold"))+
  theme(axis.text.y = element_text(color="blue", size=8, face="bold")) 
            `
  
dati %>% ungroup() %>% 
  group_by(Dipartimento) %>% 
  summarise(hworked = sum(hworked), 
            hprev = sum(hprev), 
            esami = sum(esami), 
            ricavi = sum (ricavi)) %>% 
  mutate("FTEpr" = hprev/(36*45.6), 
         "FTEr" = hworked/(36*45.6),
         "Perchwd" = 100*(hworked/hprev),
         "tempo medio esame" = 60*(hworked/esami)) %>% 
  pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
  filter(indicatore == "FTEpr") %>% 
  mutate(mediana = median(valore)) %>% 
  select(1,3) %>% 
  kable() %>% 
  kable_styling() 

  
dati %>% ungroup() %>% 
  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale", Reparto == "REPARTO VIROLOGIA" ) %>% 
  group_by(Laboratorio) %>% 
  summarise(hworked = sum(hworked), 
            hprev = sum(hprev), 
            esami = sum(esami), 
            ricavi = sum (ricavi)) %>% 
  mutate("FTEpr" = hprev/(36*45.6), 
         "FTEr" = hworked/(36*45.6),
         "Perchwd" = 100*(hworked/hprev),
         "tempo medio esame" = 60*(hworked/esami)) %>% 
  pivot_longer(2:9, names_to = "indicatore", values_to ="valore")




dati %>% ungroup() %>% 
  filter(Dipartimento == "Dipartimento Tutela e  Salute Animale") %>% 
  group_by(Laboratorio) %>% 
  summarise(hworked = sum(hworked), 
            hprev = sum(hprev), 
            esami = sum(esami), 
            ricavi = sum (ricavi)) %>% 
  mutate("FTEpr" = hprev/(36*45.6), 
         "FTEr" = hworked/(36*45.6),
         "Perchwd" = 100*(hworked/hprev),
         "tempo medio esame" = 60*(hworked/esami)) %>% 
  pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
  filter(indicatore=="Perchwd")%>%
  arrange(valore) %>%
  mutate(Laboratoro = factor(Laboratorio, levels = .$Laboratorio)) %>%
  mutate(mediana = median(valore, na.rm = TRUE)) %>%
  ggplot(aes(x=Laboratorio, y=valore, label = round(valore, 1)))+
  geom_point( aes(x=Laboratorio, y=valore),color="lightblue", size = 15 )+
  geom_text(color = "black", size = 5)+
  geom_segment(aes(y = mediana, x = Laboratorio, yend = valore, xend = Laboratorio ),color = "grey50") +
  coord_flip() +
  theme_ipsum_rc(axis_title_just = "mc")+
  #labs(y=input$indicatore,x="")+
  #geom_hline(yintercept= round(.,mediana, 0), col="red")+
  # geom_label(
  #   label="Mediana (89%)",
  #   x=25,
  #   y=25,
  #   label.padding = unit(0.55, "lines"), # Rectangle size around label
  #   label.size = 0.35,
  #   color = "black",
  #   fill="red"
  # )+
  theme(axis.text.x = element_text(color="blue", size=12, face="bold"))+
  theme(axis.text.y = element_text(color="blue", size=12, face="bold"))
 
####Laboratorio####
dati %>% ungroup() %>% 
  arrange(`%tempo-utilizzato`) %>%
  mutate(Laboratorio = factor(Laboratorio, levels = .$Laboratorio)) %>%
  mutate(mediana = median(`%tempo-utilizzato`, na.rm = TRUE),
         sopra = ifelse(`%tempo-utilizzato` - mediana > 0, TRUE, FALSE)) %>% 
  ggplot(aes( Laboratorio, `%tempo-utilizzato`, label = round(`%tempo-utilizzato`, 1) ))+
  geom_point( aes(x=Laboratorio, y=`%tempo-utilizzato`),color="lightblue", size = 8 )+
  geom_text(color = "black", size = 2.5)+
  geom_segment(aes(y = mediana, x = Laboratorio, yend = `%tempo-utilizzato`, xend = Laboratorio ),color = "grey50") +
  coord_flip() +
  theme_ipsum_rc(axis_title_just = "mc")+
  labs(y="ore erogate /ore da contratto  (%)",x="")+
  geom_hline(yintercept= round(88.7, 0), col="red")+
  geom_label(
    label="Mediana (89%)",
    x=25,
    y=25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="red"
  )+
  theme(axis.text.x = element_text(color="blue", size=8, face="bold"))+
  theme(axis.text.y = element_text(color="blue", size=8, face="bold")) 




#heatmap


 dt <-dati %>% ungroup() %>% 
  group_by(Dipartimento) %>% 
  summarise(hworked = sum(hworked), 
            hprev = sum(hprev), 
            esami = sum(esami), 
            ricavi = sum (ricavi)) %>% 
  mutate("FTE-previsto" = hprev/(36*45.6), 
         "FTE-reale" = hworked/(36*45.6),
         "%tempo-utilizzato" = 100*(hworked/hprev),
         "tempo-medio-esame" = 60*(hworked/esami)) 

dt[2:9]<- round(apply(dt[,2:9], 2, function(x) x-median(x)),2)



output$hmap<-renderPlot(
  
  
  
  dt %>% 
    pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
    # arrange("valore") %>% 
    # mutate(Dipartimento = factor(Dipartimento, levels = .$Dipartimento)) %>%
    ggplot(aes(Dipartimento,indicatore, label = valore)) + 
    geom_tile(aes(fill = valore), colour = "white") + geom_text(color = "black", size = 5)+
    scale_fill_gradient(low = "snow2", high = "steelblue")+theme_grey(base_size = 9) + 
    coord_flip()
    
  labs(x = "", y = "", title="", caption=Sys.Date()) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) 
)

dt %>% 
  pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
  # arrange("valore") %>% 
  # mutate(Dipartimento = factor(Dipartimento, levels = .$Dipartimento)) %>%
  ggplot(aes(Dipartimento,indicatore, label = valore)) + 
  geom_tile(aes(fill = valore), colour = "white") + geom_text(color = "black", size = 5)+
  scale_fill_gradient(low = "snow2", high = "steelblue")+theme_grey(base_size = 18) +
  coord_flip()+labs(x = "", y = "", title="") + theme(legend.position = "none") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), position = "right")



dt <- dati %>% ungroup() %>% 
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
  mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "Perchwd","FTEpr", "FTEr","tempo medio esame")))





dati %>% ungroup() %>% 
                 group_by(Dipartimento) %>% 
                 summarise(hworked = sum(hworked), 
                           hprev = sum(hprev), 
                           esami = sum(esami), 
                           ricavi = sum (ricavi)) %>% 
                 mutate("FTE-previsto" = hprev/(36*45.6), 
                        "FTE-reale" = hworked/(36*45.6),
                        "%tempo-utilizzato" = 100*(hworked/hprev),
                        "tempo-medio-esame" = 60*(hworked/esami)) %>% 
                 mutate(across(where(is.numeric), function(x) scale(x))) %>% 
                 pivot_longer(2:9, names_to = "indicatore", values_to ="valore") %>% 
                 mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "%tempo-utilizzato","FTEpr", "FTEr","tempo medio esame"))) %>% 
  View()




   %>% 
    mutate(indicatore = factor(indicatore, levels = c("esami", "ricavi","hprev","hworked", "Perchwd","FTEpr", "FTEr","tempo medio esame"))) %>%
    ggplot(aes(Dipartimento,indicatore, label = round(valore,1)))
