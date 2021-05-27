library(tidyverse)
library(plyr)
library(lemon)
library(hrbrthemes)


d <- tibble("Ruolo" = c(rep("Dirigenza", 2), rep("Comparto", 2), rep("Borse di studio", 2)), 
            "Genere" = rep(c("Uomini", "Donne"), 3), 
            "Numero" = c(58, 63, 172, 360, 15, 35))

 d %>% 
  ggplot(aes(x = Ruolo, y = Numero, fill = Genere))+
  geom_bar(data = subset(d, Genere == "Donne"), stat = "identity") + 
  geom_bar(data = subset(d, Genere == "Uomini"), stat = "identity") +
  scale_y_continuous(breaks = seq(-1000, 1000, 100)) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()
  
  
 test <- data.frame(v=sample(1:20,1000,replace=T), g=c('M','F'))
 
 
 ggplot(data = d, 
        mapping = aes(
          x = Ruolo, 
          y = ifelse(test = Genere == "Uomini",  yes = -Numero, no = Numero), 
          fill = Genere,
          label= Numero
        )) +
   geom_bar(stat = "identity")+
   geom_text(hjust=ifelse(test = d$Genere == "Uomini",  yes = 1.2, no = -0.2), size=4, colour="#505050") +
   scale_y_continuous(labels = abs, limits = c(-400, 400)) +
   coord_flip()+
   labs(
     x = "",
     y = "",
     fill=""
   ) +
   theme_ipsum_rc()+
   theme( 
     panel.grid.major = element_blank(), 
     panel.grid.minor = element_blank(),
     axis.text.x=element_blank(), 
     legend.position="bottom",
     legend.text=element_text(size=12), 
     axis.text.y=element_text(size=16),
     strip.text.x=element_text(size=16),
   )
   
   
   
 