library(haven)
library(tidyverse)

#### Datensatz einlesen ####

EB10 <- read_sav("ZA5234_v2-0-1.sav")

#### Datenbereinigung ####

#Auswahl relevante Variablen 

df_1 <- select(EB10,
               v6, 
               v206, 
               v553,
               v554,
               v555,
               v556   
               ) 

#Umbenennen der Variablen

df_1 <- select(df_1,
               Land = v6, 
               ZustimmungEU = v206, 
               Bildungsjahre = v553,
               BJrecoded = v554,
               Gender = v555,
               Alter = v556
               ) 
