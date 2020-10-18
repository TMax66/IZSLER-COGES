library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")

# datiatt <- read_excel(here("programmazione", "scripts", "BGSOBI2019.xlsx")
# anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/HR.xlsx")
# time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/personaleBgSoVa.xlsx")
# 
# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
# 
# datiatt <- read_excel("BGSOBI2019.xlsx")
# anag <- read_excel("HR.xlsx")
# time <- read_excel("personaleBgSoVa.xlsx")
#  


#standard time
#time std
anag$htot <- anag$hsett*48
anag$hot_m <- anag$hsett*4.3
anag$stdtime <- anag$htot*(anag$attività/100)
anag$stdtime_m <-anag$hot_m*(anag$attività/100)

#worked time
mat2019 <- unique(factor(anag$Matricola))


## tabella ripartizione risorse in laboratorio


anag %>% select(-dtnascita, -stdtime) %>% 
  pivot_wider(names_from = laboratorio, values_from = c(attività)) %>%
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  collapse_rows(columns = 1, valign = "top")

## tabella ore disponibili per reparto 

anag %>% select(-dtnascita, -attività) %>% 
  filter(reparto=="bg") %>% 
  mutate(Matricola = as.character(Matricola), 
         hsett = as.character(hsett)) %>% 
  pivot_wider(names_from = laboratorio, values_from = c(stdtime)) %>%
  janitor::adorn_totals(where = "row") %>% 
  select(-reparto) %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") 



time %>% 
  filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
  select ( rep, Matricola, Mese, Minuti) %>% 
  group_by(rep, Matricola) %>% 
  summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
  filter(., Matricola %in% mat2019) %>% 
  left_join(anag, by = "Matricola") %>% 
  mutate(wkdtime = wdmin*(attività/100)) %>% 
  group_by(rep, laboratorio) %>% 
  summarise(hstd = sum(stdtime),
            hsettw = sum(wkdtime)) %>% 
  mutate(FTEp = hstd/1728, 
         FTEe = hsettw/1728,
         perc = 100*(hsettw/hstd)) %>% 
  select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-previsto" = FTEp, "FTE-effettivo" = FTEe)




options(digits = 2)
time %>% 
  filter(rep != "VA" & Anno==2019) %>% 
  select ( rep, Matricola, Mese, Minuti) %>% 
  group_by(rep, Matricola) %>% 
  summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
  filter(., Matricola %in% mat2019) %>% 
  right_join(anag, by = "Matricola") %>% 
  View()
  mutate(wkdtime = wdmin*(attività/100)) %>% 
  group_by(rep, laboratorio) %>%
  summarise(hstd = sum(stdtime, na.rm=T),
            hsettw = sum(wkdtime, na.rm = T)) %>% 
  mutate(FTEp = hstd/1728, 
         FTEe = hsettw/1728,
         perc = 100*(hsettw/hstd)) %>% 
  select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-disponibile" = FTEp, "FTE-effettivo" = FTEe) %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  collapse_rows(columns = 1, valign = "top")


 

  datiatt$dtreg<-as.Date(datiatt$datareg, format="%Y-%m-%d")
  
  datiatt$anno <- year(datiatt$dtreg)
  datiatt$mese <- month(datiatt$dtreg)
  
  datiatt$repanalisi2 <- ifelse(datiatt$repanalisi== "Sede Territoriale di Bergamo", "Sede Territoriale di Bergamo", 
                                ifelse(datiatt$repanalisi== "Sede Territoriale di Binago","Sede Territoriale di Binago",
                                       ifelse(datiatt$repanalisi== "Sede Territoriale di Sondrio","Sede Territoriale di Sondrio","Altri reparti")))
  
  
  
  
  
  accett <- datiatt %>% 
    filter(anno==2019 ) %>% 
    group_by(repacc) %>% 
    summarise(totconf = sum(conf, na.rm = T)) %>% 
    arrange(repacc, desc(totconf)) %>% 
    mutate(laboratorio = rep("Accettazione", 3)) %>% 
    select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
    filter(Reparto != "Sede Territoriale di Binago") %>% 
    bind_rows(
      datiatt %>% 
        filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
        group_by(repanalisi2, labs) %>% 
        summarise(totesami = sum(esami, na.rm = T)) %>% 
        arrange(repanalisi2, desc(totesami)) %>% 
        select( "Reparto" = repanalisi2, laboratorio =labs, "Totale esami" = totesami) ) 
  
  accett %>% 
    kbl() %>% 
    kable_classic(full_width = F, html_font = "Cambria") %>% 
    collapse_rows(columns = 1, valign = "top")



  att <-datiatt %>% 
    filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
    group_by(repanalisi2, labs) %>% 
    summarise(totesami = sum(esami, na.rm = T)) %>% 
    arrange(repanalisi2, desc(totesami)) %>% 
    select( "Reparto" = repanalisi2, "laboratorio" =labs, "Totale esami" = totesami)
  
  accett <- datiatt %>% 
    filter(anno==2019 ) %>% 
    group_by(repacc) %>% 
    summarise(totconf = sum(conf, na.rm = T)) %>% 
    arrange(repacc, desc(totconf)) %>% 
    mutate(laboratorio = rep("Accettazione", 3)) %>% 
    select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
    filter(Reparto != "Sede Territoriale di Binago") %>% 
    bind_rows(att) %>% 
    kbl() %>% 
    kable_classic(full_width = F, html_font = "Cambria") %>% 
    collapse_rows(columns = 1, valign = "top")




x <-datiatt %>% 
  filter(repanalisi2=="Sede Territoriale di Bergamo") 



unique(datiatt$finalità)
 





time %>% 
  filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
  select ( rep, Matricola, Mese, Minuti) %>% 
  group_by(rep, Matricola, Mese) %>% 
  summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
  filter(., Matricola %in% mat2019) %>% 
  left_join(anag, by = "Matricola") %>% 
  mutate(wkdtime = wdmin*(attività/100)) %>% 
  select(- htot, - stdtime) %>% 
  group_by(rep, laboratorio, Mese) %>% 
  summarise(hstd_m = sum(stdtime_m, na.rm = T),
            hwd_m = sum(wkdtime)) %>% 
  mutate("FTE previsto" = hstd_m/154.80, 
         "FTE effettivo" = hwd_m/154.80) %>% 
  pivot_longer(cols = 6:7, names_to = "FTE") %>% 
  
  ggplot() +
  aes(x=Mese, y=value, col = FTE)+
  geom_point()+geom_line()+  
  scale_color_manual(values = c("FTE effettivo" = "black", "FTE previsto" = "red"))+
  facet_wrap(~laboratorio+rep)+
  theme_ipsum_rc(strip_text_size = 8.5, 
                 strip_text_face = "bold")+ ylab("")+
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
  
  
  




#Attività####

datiatt$dtreg<-as.Date(datiatt$datareg, format="%Y-%m-%d")

datiatt$anno <- year(datiatt$dtreg)
datiatt$mese <- month(datiatt$dtreg)

datiatt$repanalisi2 <- ifelse(datiatt$repanalisi== "Sede Territoriale di Bergamo", "Sede Territoriale di Bergamo", 
                             ifelse(datiatt$repanalisi== "Sede Territoriale di Binago","Sede Territoriale di Binago",
                                    ifelse(datiatt$repanalisi== "Sede Territoriale di Sondrio","Sede Territoriale di Sondrio","Altri reparti")))

#Numero conferimenti 2019 BG-SO ####

nconf<-datiatt %>% 
  filter(anno==2019) %>% 
  group_by(repacc) %>% 
  summarise(totconf = sum(conf, na.rm = T)) %>% 
  select( "reparto" = repacc, totconf) 



nesami <- datiatt %>% 
  filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
  group_by(repanalisi2, labs) %>% 
  summarise(totesami = sum(esami, na.rm = T)) %>% 
  arrange(repanalisi2, desc(totesami)) %>% 
  select( "reparto-analisi" = repanalisi2, "laboratorio" =labs, totesami)   
 




WL <- time %>% 
  filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
  select ( rep, Matricola, Mese, Minuti) %>% 
  group_by(rep, Matricola) %>% 
  summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
  filter(., Matricola %in% mat2019) %>% 
  left_join(anag, by = "Matricola") %>% 
  mutate(wkdtime = wdmin*(attività/100)) %>% 
  group_by(rep, laboratorio) %>% 
  summarise(hstd = sum(stdtime),
            hsettw = sum(wkdtime)) %>% 
  mutate(FTEp = hstd/1728, 
         FTEe = hsettw/1728,
         perc = 100*(hsettw/hstd)) %>% 
  select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-previsto" = FTEp, "FTE-effettivo" = FTEe)


names(WL)[1] <- "Reparto"

att <-datiatt %>% 
  filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
  group_by(repanalisi2, labs) %>% 
  summarise(totesami = sum(esami, na.rm = T)) %>% 
  arrange(repanalisi2, desc(totesami)) %>% 
  select( "Reparto" = repanalisi2, "laboratorio" =labs, "Totale esami" = totesami)

accett <- datiatt %>% 
  filter(anno==2019 ) %>% 
  group_by(repacc) %>% 
  summarise(totconf = sum(conf, na.rm = T)) %>% 
  arrange(repacc, desc(totconf)) %>% 
  mutate(laboratorio = rep("Accettazione", 3)) %>% 
  select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
  filter(Reparto != "Sede Territoriale di Binago") %>% 
  bind_rows(att)
  
options(digits = 2)
WL %>% 
  # filter(!laboratorio %in% c("Accettazione", "Amministrazione")) %>% 
  mutate(Reparto = recode(Reparto, BG = "Sede Territoriale di Bergamo", SO = "Sede Territoriale di Sondrio")) %>% 
  right_join(accett, by = c("Reparto", "laboratorio"))  
  


options(knirt.kable.NA = "")
WL %>% 
  # filter(!laboratorio %in% c("Accettazione", "Amministrazione")) %>% 
  mutate(Reparto = recode(Reparto, BG = "Sede Territoriale di Bergamo", SO = "Sede Territoriale di Sondrio")) %>% 
  right_join(accett, by = c("Reparto", "laboratorio")) %>% 
  kbl(digits = 2) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  collapse_rows(columns = 1, valign = "top")



#####
p <- read_excel("P1.xlsx")
p$'Obiettivo A' <- rep(12, 8)
p$labA <- c(84, 0,78,88,0,75,5,4)
p$labB <- c(0,88,5,0,0,0,5,0)
p$labC <- c(4,0,5,0,88,13,78,84)
hst <- function(x)
{ (x/100)*1728
}
M <- data.frame("Operatore" = p$operatore, apply(p[,-1], 2, hst))
