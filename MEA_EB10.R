#Mehrebenenanalyse zum Einfluss des Bildungsniveaus auf Einstellung zur EU
##Daten: Eurobarometer 2010

library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)   #Diagramme
library(lme4)      #Mehrebenenanlyse
library(misty)     #Zentrierung
library(stargazer) #Output


#Daten einlesen
EB_10 <- read_sav("ZA5234_v2-0-1.sav") #Eurobarometer
gdp <- read_csv("gdp-per-capita-inflation--and-ppp-adjusted-world-bank-data-vs-penn-world-table-data.csv") #GDP


#Datensätze reinigen
##EB10
###Variablenauswahl und Umbenennung
EB10_Vars <- select(EB_10,
                    Land = v6,
                    Gewichtung_Land = v40,
                    Einstellung_EU = v206,
                    Bildungsjahre = v553,
                    Bildungsjahre_kategorisiert = v554,
                    Geschlecht = v555,
                    Alter = v556,
                    Alter_gruppiert = v557
                    )

###NAs löschen
DatensatzEB <- na.omit(EB10_Vars)

### Geschlecht recoden
DatensatzEB$Geschlecht_recoded <- NA
DatensatzEB$Geschlecht_recoded [DatensatzEB$Geschlecht == 1] <- 1 #male
DatensatzEB$Geschlecht_recoded [DatensatzEB$Geschlecht == 2] <- 0 #female


#Einstellung zur EU recoden (dichotomisieren: good=1, bad&neither good nor bad=0, dk=löschen
DatensatzEB$EinstellungDichotom <- NA

## Umkodierung der Antwortkategorien und Zuordnung zu "EinstellungDichotom"
DatensatzEB$EinstellungDichotom[DatensatzEB$Einstellung_EU == 1] <- 1          #gut
DatensatzEB$EinstellungDichotom[DatensatzEB$Einstellung_EU %in% c(2, 3)] <- 0  #schlecht, weder noch
DatensatzEB <- DatensatzEB[!DatensatzEB$Einstellung_EU %in% c(4, 9), ]         # Löschen der Fälle mit den Antworten 4 (dont know) und 9(inap)


#Bildung gruppiert neu kodiert:
DatensatzEB$Bildungsjahre_recoded <- DatensatzEB$Bildungsjahre_kategorisiert     # Umkodierung der Antwortkategorien
DatensatzEB <- DatensatzEB[!(DatensatzEB$Bildungsjahre_recoded %in% c(97, 98)), ]# Löschen der Fälle mit den Antworten 97 und 98
DatensatzEB$Bildungsjahre_recoded[DatensatzEB$Bildungsjahre_recoded == 11] <- 0  # Umkodierung der Antwortkategorie 11 (no full time) zu 0

###Ländernamen zum mergen
DatensatzEB$Entity <- NA

clean_data$Entity[clean_data$Land == 1] <- "France"
clean_data$Entity[clean_data$Land == 2] <- "Belgium"
clean_data$Entity[clean_data$Land == 3] <- "Netherlands"
clean_data$Entity[clean_data$Land == 4] <- "Germany" #West
clean_data$Entity[clean_data$Land == 5] <- "Italy"
clean_data$Entity[clean_data$Land == 6] <- "Luxembourg"
clean_data$Entity[clean_data$Land == 7] <- "France"
clean_data$Entity[clean_data$Land == 8] <- "Ireland"
clean_data$Entity[clean_data$Land == 9] <- "Great Britain"
clean_data$Entity[clean_data$Land == 10] <- "Northern Ireland"
clean_data$Entity[clean_data$Land == 11] <- "Greece"
clean_data$Entity[clean_data$Land == 12] <- "Spain"
clean_data$Entity[clean_data$Land == 13] <- "Portugal"
clean_data$Entity[clean_data$Land == 14] <- "Germany" #Ost
#clean_data$Entity[clean_data$Land == 15] <- gibts nicht
clean_data$Entity[clean_data$Land == 16] <- "Finnland"
clean_data$Entity[clean_data$Land == 17] <- "Sweden"
clean_data$Entity[clean_data$Land == 18] <- "Austria"
clean_data$Entity[clean_data$Land == 19] <- "Cyprus"
clean_data$Entity[clean_data$Land == 20] <- "Czech Republic"
clean_data$Entity[clean_data$Land == 21] <- "Estonia"
clean_data$Entity[clean_data$Land == 22] <- "Hungary"
clean_data$Entity[clean_data$Land == 23] <- "Latvia"
clean_data$Entity[clean_data$Land == 24] <- "Lithuania"
clean_data$Entity[clean_data$Land == 25] <- "Malta"
clean_data$Entity[clean_data$Land == 26] <- "Poland"
clean_data$Entity[clean_data$Land == 27] <- "Slovakia"
clean_data$Entity[clean_data$Land == 28] <- "Slovenia"
clean_data$Entity[clean_data$Land == 29] <- "Bulgaria"
clean_data$Entity[clean_data$Land == 30] <- "Romania"


table(clean_data$Entity)
