library("readxl")
library("tidyverse")
library("lubridate")
library("kableExtra")
library("gridExtra")
library("hrbrthemes")
library("knitr")
library("here")

hwd <- read_excel(here("programmazione", "data", "raw", "PresenzePersonale2019_12Ott2020.xlsx"))


hwd <- hwd %>% 
  mutate(Dipartimento = recode (Reparto, "REPARTO VIROLOGIA" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO TECNOLOGIE BIOLOGICHE APPLICATE" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO PRODUZIONE E CONTROLLO MATERIALE BIOLOGICO" = "Dipartimento Tutela e  Salute Animale",
                                "REPARTO VIRUS VESCICOLARI E PRODUZIONI BIOTECNOLOGICHE" = "Dipartimento Tutela e  Salute Animale", 
                                "REPARTO PRODUZIONE PRIMARIA" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CHIMICA DEGLI ALIMENTI E MANGIMI" = "Dipartimento Sicurezza Alimentare", 
                                "REPARTO CHIMICO DEGLI ALIMENTI (BOLOGNA)" = "Dipartimento Sicurezza Alimentare", 
                                "Controllo alimenti e trasformazioni" = "Dipartimento Sicurezza Alimentare", 
                                "SEDE TERRITORIALE DI BERGAMO - BINAGO - SONDRIO" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI BRESCIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI PAVIA" = "Area Territoriale Lombardia",
                                "SEDE TERRITORIALE DI CREMONA - MANTOVA" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI LODI - MILANO" = "Area Territoriale Lombardia", 
                                "SEDE TERRITORIALE DI FORLÌ - RAVENNA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI BOLOGNA - MODENA - FERRARA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI PIACENZA - PARMA" = "Area Territoriale Emilia Romagna", 
                                "SEDE TERRITORIALE DI REGGIO EMILIA" = "Area Territoriale Emilia Romagna", 
                                "U.O. GESTIONE SERVIZI STRUMENTALI" = "Dipartimento Amministrativo", 
                                "U.O. GESTIONE RISORSE UMANE E SVILUPPO COMPETENZE" = "Dipartimento Amministrativo", 
                                "U.O. AFFARI GENERALI E LEGALI" = "Dipartimento Amministrativo"
                                ))


hwd %>% 
  group_by(Reparto, CDC, Matricola) %>% 
  summarise(n=n()) %>% 
  pivot_wider( names_from = "CDC", values_from = n, values_fill = 0) %>% 
  View()
