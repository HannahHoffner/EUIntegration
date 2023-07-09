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

DatensatzEB$Entity[DatensatzEB$Land == 1] <- "France"
DatensatzEB$Entity[DatensatzEB$Land == 2] <- "Belgium"
DatensatzEB$Entity[DatensatzEB$Land == 3] <- "Netherlands"
DatensatzEB$Entity[DatensatzEB$Land == 4] <- "Germany" #West
DatensatzEB$Entity[DatensatzEB$Land == 5] <- "Italy"
DatensatzEB$Entity[DatensatzEB$Land == 6] <- "Luxembourg"
DatensatzEB$Entity[DatensatzEB$Land == 7] <- "France"
DatensatzEB$Entity[DatensatzEB$Land == 8] <- "Ireland"
DatensatzEB$Entity[DatensatzEB$Land == 9] <- "Great Britain"
DatensatzEB$Entity[DatensatzEB$Land == 10] <- "Northern Ireland"
DatensatzEB$Entity[DatensatzEB$Land == 11] <- "Greece"
DatensatzEB$Entity[DatensatzEB$Land == 12] <- "Spain"
DatensatzEB$Entity[DatensatzEB$Land == 13] <- "Portugal"
DatensatzEB$Entity[DatensatzEB$Land == 14] <- "Germany" #Ost
#DatensatzEB$Entity[DatensatzEB$Land == 15] <- gibts nicht
DatensatzEB$Entity[DatensatzEB$Land == 16] <- "Finnland"
DatensatzEB$Entity[DatensatzEB$Land == 17] <- "Sweden"
DatensatzEB$Entity[DatensatzEB$Land == 18] <- "Austria"
DatensatzEB$Entity[DatensatzEB$Land == 19] <- "Cyprus"
DatensatzEB$Entity[DatensatzEB$Land == 20] <- "Czech Republic"
DatensatzEB$Entity[DatensatzEB$Land == 21] <- "Estonia"
DatensatzEB$Entity[DatensatzEB$Land == 22] <- "Hungary"
DatensatzEB$Entity[DatensatzEB$Land == 23] <- "Latvia"
DatensatzEB$Entity[DatensatzEB$Land == 24] <- "Lithuania"
DatensatzEB$Entity[DatensatzEB$Land == 25] <- "Malta"
DatensatzEB$Entity[DatensatzEB$Land == 26] <- "Poland"
DatensatzEB$Entity[DatensatzEB$Land == 27] <- "Slovakia"
DatensatzEB$Entity[DatensatzEB$Land == 28] <- "Slovenia"
DatensatzEB$Entity[DatensatzEB$Land == 29] <- "Bulgaria"
DatensatzEB$Entity[DatensatzEB$Land == 30] <- "Romania"


table(DatensatzEB$Entity)


##GDP Datensatz
GDP <- filter(gdp, Year == 2009)  #2009 rausfiltern 
#Länder filtern
eu_laender <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic",
                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
                "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
                "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

GDP2 <- subset(GDP, Entity %in% eu_laender)

#Continent, Year, Population, Code löschen

gdp_per_capita_b2$Continent <- NULL
gdp_per_capita_b2$Year <- NULL
gdp_per_capita_b2$Code <- NULL

colnames(gdp_per_capita_b2)[4] <- "Population"
gdp_per_capita_b2$Population <- NULL
