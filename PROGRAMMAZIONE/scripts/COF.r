library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")



##tempi esame
tempi <- read_excel(here("programmazione", "data", "raw", "tempianalisi.xlsx"))

tempi$mp <- substr(tempi$mp, start=1, stop = 9)

tempi$VNMP <- paste(tempi$vn,tempi$mp) # <- creo una chiave univoca in tempi tempi$VNMPdup <- duplicated(tempi$VNMP)



## esami
esami <- read_excel(here("programmazione", "data", "raw", "dati2019.xlsx"))

esami$VNMP <- paste(esami$chiave, esami$`Descrizione del MP`) # <- creo chiave simile a tempi$VNMP per fare collegamento tra esami e tempi

esami$REPARTO <- tolower(esami$reparto)

esami <- esami %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di milano (is)" = "sede territoriale di milano", 
                          "reparto tecnologie biologiche applicate - batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "reparto tecnologie biologiche applicate - colture cellulari" = "reparto tecnologie biologiche applicate", 
                          "reparto virologia - laboratorio proteomica" = "reparto virologia", 
                          )) %>% 
  filter(!REPARTO %in% c("analisi del rischio ed epidemiologia genomica",
                         "reparto produzione e controllo materiale biologico"))



###ore lavorate

hwd19 <- readRDS( here("programmazione", "data", "processed", "hwd19.rds")) # <- dati ore contratto e ore erogate per dip/rep/lab

hwd19$REPARTO <- tolower(hwd19$Laboratorio)

hwd19 <- hwd19 %>% 
  mutate(REPARTO = recode(REPARTO, "sede territoriale di piacenza - parma" = "sede territoriale di piacenza" , 
                          "laboratorio chimica applicata alle tecnologie alimentari" = "reparto chimica degli alimenti e mangimi",
                          "laboratorio contaminanti ambientali" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio mangimi e tossicologia" = "reparto chimica degli alimenti e mangimi", 
                          "laboratorio residui" = "reparto chimica degli alimenti e mangimi", 
                          "reparto chimico degli alimenti (bologna)" = "bologna (reparto chimico degli alimenti)", 
                          "laboratorio analisi genomiche, laboratorio diagnostica molecolare, ogm" = "reparto tecnologie biologiche applicate",
                          "laboratorio batteriologia specializzata" = "reparto tecnologie biologiche applicate", 
                          "laboratorio colture cellulari, biobanca" = "reparto tecnologie biologiche applicate", 
                          "laboratorio di proteomica e diagnostica tse" = "reparto virologia", 
                          "laboratorio di virologia e sierologia specializzata, microscopia elettronica" = "reparto virologia")) %>% 
  filter(!REPARTO %in% c("laboratorio benessere animale, biochimica clinica, immunologia veterinaria e stabulari",
                         "laboratorio di controllo di prodotti biologici, farmaceutici e convalida di processi produttivi", 
                         "laboratorio produzione terreni", 
                         "laboratorio produzione vaccini e reagenti"))




#####adtabase######

t <- tempi %>% 
  select(VNMP, timecomp, timedirig)

e <- esami %>% 
  select(VNMP, esami, reparto)


e %>% 
  left_join(t, by = "VNMP") %>% 
  mutate(tempoesami = esami*timecomp)












# ### dati presenze matricole 2019 ####
# hwd <- read_excel(here("programmazione", "data", "raw", "PresenzePersonale2019_12Ott2020.xlsx"))
# hwd$CDC <- ifelse(hwd$CodiceCDC %in% c(5502, 5501), "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)", hwd$CDC)
# 
# ###dati conversione matricole gru-sigma##
# grusigma <- read_excel(here("programmazione", "data", "raw", "MatricoleSigmaGRU.xlsx"))
# grusigma <- grusigma %>% 
#   select("matricola" = Matricola, "Matricola" = MatricolaSigma)
# 
# ### dati dei presenti nel 2019 -anagrafica con dati contrattuali ####
# anag19 <- read_excel(here("programmazione", "data", "raw", "Presenti_2019.xls"))




# # datiatt <- read_excel(here("programmazione", "scripts", "BGSOBI2019.xlsx")
# # anag <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/HR.xlsx")
# # time <- read_excel("D:/Dati/vito.tranquillo/Desktop/GitProjects/IZSLER-COGES/PROGRAMMAZIONE/personaleBgSoVa.xlsx")
# # 
# # setwd("~/Library/Mobile Documents/com~apple~CloudDocs/gitProject/IZSLER-COGES/PROGRAMMAZIONE")
# # 
# # datiatt <- read_excel("BGSOBI2019.xlsx")
# # anag <- read_excel("HR.xlsx")
# # time <- read_excel("personaleBgSoVa.xlsx")
# #  
# 
# 
# #standard time
# #time std
# anag$htot <- anag$hsett*48
# anag$hot_m <- anag$hsett*4.3
# anag$stdtime <- anag$htot*(anag$attività/100)
# anag$stdtime_m <-anag$hot_m*(anag$attività/100)
# 
# #worked time
# mat2019 <- unique(factor(anag$Matricola))
# 
# 
# ## tabella ripartizione risorse in laboratorio
# 
# 
# anag %>% select(-dtnascita, -stdtime) %>% 
#   pivot_wider(names_from = laboratorio, values_from = c(attività)) %>%
#   kbl() %>% 
#   kable_classic(full_width = F, html_font = "Cambria") %>% 
#   collapse_rows(columns = 1, valign = "top")
# 
# ## tabella ore disponibili per reparto 
# 
# anag %>% select(-dtnascita, -attività) %>% 
#   filter(reparto=="bg") %>% 
#   mutate(Matricola = as.character(Matricola), 
#          hsett = as.character(hsett)) %>% 
#   pivot_wider(names_from = laboratorio, values_from = c(stdtime)) %>%
#   janitor::adorn_totals(where = "row") %>% 
#   select(-reparto) %>% 
#   kbl() %>% 
#   kable_classic(full_width = F, html_font = "Cambria") 
# 
# 
# 
# time %>% 
#   filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
#   select ( rep, Matricola, Mese, Minuti) %>% 
#   group_by(rep, Matricola) %>% 
#   summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
#   filter(., Matricola %in% mat2019) %>% 
#   left_join(anag, by = "Matricola") %>% 
#   mutate(wkdtime = wdmin*(attività/100)) %>% 
#   group_by(rep, laboratorio) %>% 
#   summarise(hstd = sum(stdtime),
#             hsettw = sum(wkdtime)) %>% 
#   mutate(FTEp = hstd/1728, 
#          FTEe = hsettw/1728,
#          perc = 100*(hsettw/hstd)) %>% 
#   select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-previsto" = FTEp, "FTE-effettivo" = FTEe)
# 
# 
# 
# 
# options(digits = 2)
# time %>% 
#   filter(rep != "VA" & Anno==2019) %>% 
#   select ( rep, Matricola, Mese, Minuti) %>% 
#   group_by(rep, Matricola) %>% 
#   summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
#   filter(., Matricola %in% mat2019) %>% 
#   right_join(anag, by = "Matricola") %>% 
#   View()
#   mutate(wkdtime = wdmin*(attività/100)) %>% 
#   group_by(rep, laboratorio) %>%
#   summarise(hstd = sum(stdtime, na.rm=T),
#             hsettw = sum(wkdtime, na.rm = T)) %>% 
#   mutate(FTEp = hstd/1728, 
#          FTEe = hsettw/1728,
#          perc = 100*(hsettw/hstd)) %>% 
#   select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-disponibile" = FTEp, "FTE-effettivo" = FTEe) %>% 
#   kbl() %>% 
#   kable_classic(full_width = F, html_font = "Cambria") %>% 
#   collapse_rows(columns = 1, valign = "top")
# 
# 
#  
# 
#   datiatt$dtreg<-as.Date(datiatt$datareg, format="%Y-%m-%d")
#   
#   datiatt$anno <- year(datiatt$dtreg)
#   datiatt$mese <- month(datiatt$dtreg)
#   
#   datiatt$repanalisi2 <- ifelse(datiatt$repanalisi== "Sede Territoriale di Bergamo", "Sede Territoriale di Bergamo", 
#                                 ifelse(datiatt$repanalisi== "Sede Territoriale di Binago","Sede Territoriale di Binago",
#                                        ifelse(datiatt$repanalisi== "Sede Territoriale di Sondrio","Sede Territoriale di Sondrio","Altri reparti")))
#   
#   
#   
#   
#   
#   accett <- datiatt %>% 
#     filter(anno==2019 ) %>% 
#     group_by(repacc) %>% 
#     summarise(totconf = sum(conf, na.rm = T)) %>% 
#     arrange(repacc, desc(totconf)) %>% 
#     mutate(laboratorio = rep("Accettazione", 3)) %>% 
#     select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
#     filter(Reparto != "Sede Territoriale di Binago") %>% 
#     bind_rows(
#       datiatt %>% 
#         filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
#         group_by(repanalisi2, labs) %>% 
#         summarise(totesami = sum(esami, na.rm = T)) %>% 
#         arrange(repanalisi2, desc(totesami)) %>% 
#         select( "Reparto" = repanalisi2, laboratorio =labs, "Totale esami" = totesami) ) 
#   
#   accett %>% 
#     kbl() %>% 
#     kable_classic(full_width = F, html_font = "Cambria") %>% 
#     collapse_rows(columns = 1, valign = "top")
# 
# 
# 
#   att <-datiatt %>% 
#     filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
#     group_by(repanalisi2, labs) %>% 
#     summarise(totesami = sum(esami, na.rm = T)) %>% 
#     arrange(repanalisi2, desc(totesami)) %>% 
#     select( "Reparto" = repanalisi2, "laboratorio" =labs, "Totale esami" = totesami)
#   
#   accett <- datiatt %>% 
#     filter(anno==2019 ) %>% 
#     group_by(repacc) %>% 
#     summarise(totconf = sum(conf, na.rm = T)) %>% 
#     arrange(repacc, desc(totconf)) %>% 
#     mutate(laboratorio = rep("Accettazione", 3)) %>% 
#     select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
#     filter(Reparto != "Sede Territoriale di Binago") %>% 
#     bind_rows(att) %>% 
#     kbl() %>% 
#     kable_classic(full_width = F, html_font = "Cambria") %>% 
#     collapse_rows(columns = 1, valign = "top")
# 
# 
# 
# 
# x <-datiatt %>% 
#   filter(repanalisi2=="Sede Territoriale di Bergamo") 
# 
# 
# 
# unique(datiatt$finalità)
#  
# 
# 
# 
# 
# 
# time %>% 
#   filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
#   select ( rep, Matricola, Mese, Minuti) %>% 
#   group_by(rep, Matricola, Mese) %>% 
#   summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
#   filter(., Matricola %in% mat2019) %>% 
#   left_join(anag, by = "Matricola") %>% 
#   mutate(wkdtime = wdmin*(attività/100)) %>% 
#   select(- htot, - stdtime) %>% 
#   group_by(rep, laboratorio, Mese) %>% 
#   summarise(hstd_m = sum(stdtime_m, na.rm = T),
#             hwd_m = sum(wkdtime)) %>% 
#   mutate("FTE previsto" = hstd_m/154.80, 
#          "FTE effettivo" = hwd_m/154.80) %>% 
#   pivot_longer(cols = 6:7, names_to = "FTE") %>% 
#   
#   ggplot() +
#   aes(x=Mese, y=value, col = FTE)+
#   geom_point()+geom_line()+  
#   scale_color_manual(values = c("FTE effettivo" = "black", "FTE previsto" = "red"))+
#   facet_wrap(~laboratorio+rep)+
#   theme_ipsum_rc(strip_text_size = 8.5, 
#                  strip_text_face = "bold")+ ylab("")+
#   theme(legend.title = element_blank()) +
#   scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))
#   
#   
#   
# 
# 
# 
# 
# #Attività####
# 
# datiatt$dtreg<-as.Date(datiatt$datareg, format="%Y-%m-%d")
# 
# datiatt$anno <- year(datiatt$dtreg)
# datiatt$mese <- month(datiatt$dtreg)
# 
# datiatt$repanalisi2 <- ifelse(datiatt$repanalisi== "Sede Territoriale di Bergamo", "Sede Territoriale di Bergamo", 
#                              ifelse(datiatt$repanalisi== "Sede Territoriale di Binago","Sede Territoriale di Binago",
#                                     ifelse(datiatt$repanalisi== "Sede Territoriale di Sondrio","Sede Territoriale di Sondrio","Altri reparti")))
# 
# #Numero conferimenti 2019 BG-SO ####
# 
# nconf<-datiatt %>% 
#   filter(anno==2019) %>% 
#   group_by(repacc) %>% 
#   summarise(totconf = sum(conf, na.rm = T)) %>% 
#   select( "reparto" = repacc, totconf) 
# 
# 
# 
# nesami <- datiatt %>% 
#   filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
#   group_by(repanalisi2, labs) %>% 
#   summarise(totesami = sum(esami, na.rm = T)) %>% 
#   arrange(repanalisi2, desc(totesami)) %>% 
#   select( "reparto-analisi" = repanalisi2, "laboratorio" =labs, totesami)   
#  
# 
# 
# 
# 
# WL <- time %>% 
#   filter(rep %in% c("BG", "SO") & Anno==2019) %>% 
#   select ( rep, Matricola, Mese, Minuti) %>% 
#   group_by(rep, Matricola) %>% 
#   summarise(wdmin = sum(Minuti/60, na.rm = T)) %>% 
#   filter(., Matricola %in% mat2019) %>% 
#   left_join(anag, by = "Matricola") %>% 
#   mutate(wkdtime = wdmin*(attività/100)) %>% 
#   group_by(rep, laboratorio) %>% 
#   summarise(hstd = sum(stdtime),
#             hsettw = sum(wkdtime)) %>% 
#   mutate(FTEp = hstd/1728, 
#          FTEe = hsettw/1728,
#          perc = 100*(hsettw/hstd)) %>% 
#   select("reparto" = rep, laboratorio,"tempo-standard" = hstd, "tempo-lavorato" = hsettw, "%tempo-stand utilizzato" = perc,  "FTE-previsto" = FTEp, "FTE-effettivo" = FTEe)
# 
# 
# names(WL)[1] <- "Reparto"
# 
# att <-datiatt %>% 
#   filter(anno==2019 & repanalisi2 != "Altri reparti", repanalisi2 != "Sede Territoriale di Binago") %>% 
#   group_by(repanalisi2, labs) %>% 
#   summarise(totesami = sum(esami, na.rm = T)) %>% 
#   arrange(repanalisi2, desc(totesami)) %>% 
#   select( "Reparto" = repanalisi2, "laboratorio" =labs, "Totale esami" = totesami)
# 
# accett <- datiatt %>% 
#   filter(anno==2019 ) %>% 
#   group_by(repacc) %>% 
#   summarise(totconf = sum(conf, na.rm = T)) %>% 
#   arrange(repacc, desc(totconf)) %>% 
#   mutate(laboratorio = rep("Accettazione", 3)) %>% 
#   select( "Reparto" = repacc, laboratorio,  "Totale conferimenti" = totconf) %>% 
#   filter(Reparto != "Sede Territoriale di Binago") %>% 
#   bind_rows(att)
#   
# options(digits = 2)
# WL %>% 
#   # filter(!laboratorio %in% c("Accettazione", "Amministrazione")) %>% 
#   mutate(Reparto = recode(Reparto, BG = "Sede Territoriale di Bergamo", SO = "Sede Territoriale di Sondrio")) %>% 
#   right_join(accett, by = c("Reparto", "laboratorio"))  
#   
# 
# 
# options(knirt.kable.NA = "")
# WL %>% 
#   # filter(!laboratorio %in% c("Accettazione", "Amministrazione")) %>% 
#   mutate(Reparto = recode(Reparto, BG = "Sede Territoriale di Bergamo", SO = "Sede Territoriale di Sondrio")) %>% 
#   right_join(accett, by = c("Reparto", "laboratorio")) %>% 
#   kbl(digits = 2) %>% 
#   kable_classic(full_width = F, html_font = "Cambria") %>% 
#   collapse_rows(columns = 1, valign = "top")
# 
# 
# 
# #####
# p <- read_excel("P1.xlsx")
# p$'Obiettivo A' <- rep(12, 8)
# p$labA <- c(84, 0,78,88,0,75,5,4)
# p$labB <- c(0,88,5,0,0,0,5,0)
# p$labC <- c(4,0,5,0,88,13,78,84)
# hst <- function(x)
# { (x/100)*1728
# }
# M <- data.frame("Operatore" = p$operatore, apply(p[,-1], 2, hst))
# 
