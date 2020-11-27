dati <- readRDS( here("programmazione", "shinyapp", "dati.rds"))
dati <- dati %>% 
  mutate(across(where(is.numeric), function(x) round(x, 2)))
vp <- readRDS( here("programmazione", "shinyapp", "vp.rds"))
ai <- readRDS( here("programmazione", "shinyapp", "ai.rds"))



dir <- dati %>%
  filter(contratto == "DIRIGENZA") %>%
  group_by(Dipartimento) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_d = round(sum(`FTE-reale`),1))


comp <- dati %>%
  filter(contratto == "COMPARTO") %>%
  group_by(Dipartimento) %>%
  summarise(esami = sum(esami),
            ricavi = sum(ricavi),
            FTE_c = round(sum(`FTE-reale`),1))

vp <- vp %>% 
  group_by(Dipartimento) %>% 
  summarise(VP = sum(`Vendita Prodotti`))

ai <- ai %>% 
  group_by(Dipartimento) %>% 
  summarise(AI = sum(`Attività Interna`))



tabella <- dir %>%
  bind_cols((comp %>%
               select(4)), (vp %>%
                              select(2)), (ai %>%
                                             select(2))) %>%

  mutate(RT = (ricavi+VP+AI),
         FTE_t = round((FTE_d+FTE_c),1)) %>%
  arrange(desc(esami)) %>%
  adorn_totals(where = "row") %>%
  mutate( "R-FTE" = round(RT/FTE_t,0) ) %>%
  select(Dipartimento, "N.esami" = esami, "FTED" = FTE_d,   "FTEC" = FTE_c, "FTET" = FTE_t, "RA" = ricavi, "RVP" = VP,
         "RAI" = AI, "RT" = RT, "R/FTET" = "R-FTE")
 

tabella %>% 
  filter(Dipartimento != "Total") %>% 
  mutate(Analisi = round(100*(esami/sum(esami)), 0), 
         "RA" = round(100*(ricavi/sum(ricavi)),0), 
         "FTED" = round(100*(FTE_d/sum(FTE_d)), 0), 
         "FTEC" = round(100*(FTE_c/sum(FTE_c)),0),
         "RVP" =round(100*(VP/sum(VP)),0), 
         "RAI" = round(100*(AI/sum(AI)), 0),
         "RT" = round(100*(RT/sum(RT)),0),
         "FTET" = round(100*(FTE_t/ sum(FTE_t)), 0),
         "Ricavo per FTE" = round(100*(`R-FTE`/sum(`R-FTE`)), 0)
         ) %>% 
  select(Dipartimento, Analisi, "FTED", "FTEC", "FTET",   "RT", "Ricavo per FTE") %>% 
  pivot_longer(!Dipartimento, names_to = "KPI", values_to = "valore") %>% 
 mutate(KPI = factor(KPI, levels = c("Analisi", "FTED", "FTEC", "FTET", "RT", "Ricavo per FTE"  ))) %>% 
  ggplot( aes( 
             x = KPI, 
             y = valore, 
             fill = factor(KPI)
        )) + geom_col(width = 0.9, color = "white")+
  coord_polar(theta = "x")+ facet_wrap(~Dipartimento, nrow = 2)+
  scale_fill_brewer(palette = "Blues")+
  geom_text(aes(y = valore-10, label = paste0(valore, "%")), color = "black", size=3) 
