library(here)
library(tidyverse)

dati <- readRDS( here("programmazione", "data", "processed", "dati.rds"))





dati %>% ungroup() %>% 
  arrange(`%tempo-utilizzato`) %>%
  mutate(Laboratorio = factor(Laboratorio, levels = .$Laboratorio)) %>%
  mutate(mediana = median(`%tempo-utilizzato`, na.rm = TRUE),
         sopra = ifelse(`%tempo-utilizzato` - mediana > 0, TRUE, FALSE)) %>% 
  ggplot(aes( Laboratorio, `%tempo-utilizzato`, color = sopra))+
  geom_point( aes(x=Laboratorio, y=`%tempo-utilizzato`, size=RxFTEr),shape=21,color="darkblue" )+
  geom_segment(aes(y = mediana, x = Laboratorio, yend = `%tempo-utilizzato`, xend = Laboratorio ),color = "grey50") +
  coord_flip() +
  theme_ipsum_rc(axis_title_just = "mc")+
  labs(y="ore erogate /ore da contratto  (%)",x="")+
  geom_hline(yintercept= round(88.7, 0), col="red")+
  geom_label(
    label="Mediana (88%)",
    x=25,
    y=25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="white"
  )


+scale_size("Ricavi x FTE-reale",  range=c(5, 25))



  geom_text(color="black", size=4)+
  coord_flip()+
  theme_ipsum_rc(axis_title_just = "mc")+
  labs(y="ore erogate /ore da contratto  (%)",x="")+
  geom_hline(yintercept= round(88.7, 0), col="red")+
  geom_label(
    label="Mediana (88%)",
    x=25,
    y=25,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="white"
  )



