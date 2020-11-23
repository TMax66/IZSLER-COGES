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
 
tabella
