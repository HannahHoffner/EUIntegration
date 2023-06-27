library(haven)
library(tidyverse)

#### Datensatz einlesen ####

EB10 <- read_sav("ZA5234_v2-0-1.sav")

#### Datenbereinigung ####

#Auswahl relevante Variablen 

df_1 <- select(EB10,
               v6, 
               v40,
               v206, 
               v553,
               v554,
               v555,
               v556   
               ) 

#Umbenennen der Variablen

df_1 <- select(df_1,
               Land = v6, 
               LandGewichtung = v40,
               Zustimmung_EU = v206, 
               BJ_recoded = v554,
               Gender = v555,
               Alter = v556
               ) 

#fehlt noch : BIP (BIP pro Kopf?)


#NAs löschen 

clean_data <- na.omit(df_1)


#neue Variable: 
#ZustimmungDichotom: good=1, bad&neither good nor bad=0, dk=löschen (??)

#Geschlecht auf 0 & 1

#BJrecodiert: 10 still studying 
# 11 no full time education 
# 97 refusal 
# 98 dk

unique(df_01$)

#### Deskriptive Statistik ####


#### Deskreptive Statistik ####

plot(clean_data$BJ_recoded, clean_data$Zustimmung_EU)


