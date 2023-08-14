#Mehrebenenanalyse zum Einfluss des Bildungsniveaus auf Einstellung zur EU
##Daten: Eurobarometer 2010

library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)    #Diagramme
library(lme4)       #Mehrebenenanlyse
library(misty)      #Zentrierung
library(stargazer)  #Output
library(performance)#ICC
library(dplyr)      #BIP quadrieren
library(ordinal)    #3-geteilte AV MEA


#Daten einlesen
EuBa <- read_sav("DatensatzUKundGERmerge.sav") #Eurobarometer
gdp <- read_csv("gdp-per-capita-inflation--and-ppp-adjusted-world-bank-data-vs-penn-world-table-data.csv") #GDP

#Datensätze reinigen
##EB10
###Variablenauswahl und Umbenennung
EuBa_Vars <- select(EuBa,
                    Land = NATION,
                    Gewichtung_Land = v40,
                    Einstellung_EU = v206,
                    Benefit_EU = v207,
                    Bildungsjahre = v553,
                    Bildungsjahre_kategorisiert = v554,
                    Geschlecht = v555,
                    Alter = v556,
                    Alter_gruppiert = v557
)

###NAs löschen
EuBaDaten <- na.omit(EuBa_Vars)

#Benefit recoden
EuBaDaten$Benefit_recoded <- NA
EuBaDaten$Benefit_recoded[EuBaDaten$Benefit_EU == 1] <- 1          #benefited
EuBaDaten$Benefit_recoded[EuBaDaten$Benefit_EU == 2] <- 0  #not benefited 
EuBaDaten <- EuBaDaten[!EuBaDaten$Einstellung_EU %in% c(3), ]   #dk löschen


### Geschlecht recoden
EuBaDaten$Geschlecht_recoded <- NA
EuBaDaten$Geschlecht_recoded [EuBaDaten$Geschlecht == 1] <- 1 #male
EuBaDaten$Geschlecht_recoded [EuBaDaten$Geschlecht == 2] <- 0 #female

#Einstellung zur EU recoden (dichotomisieren: good=1, bad&neither good nor bad=0, dk=löschen
EuBaDaten$EinstellungDichotom <- NA

## Umkodierung der Antwortkategorien und Zuordnung zu "EinstellungDichotom"
EuBaDaten$EinstellungDichotom[EuBaDaten$Einstellung_EU == 1] <- 1          #gut
EuBaDaten$EinstellungDichotom[EuBaDaten$Einstellung_EU %in% c(2, 3)] <- 0  #schlecht, weder noch
EuBaDaten <- EuBaDaten[!EuBaDaten$Einstellung_EU %in% c(4, 9), ]         # Löschen der Fälle mit den Antworten 4 (dont know) und 9(inap)


## Einstellung in 3 Kategorien 
#neue Variable
EuBaDaten$Einstellung3 <- NA
## Umkodierung und Zuordnung 
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 3] <- 1 #wedernoch
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 1] <- 2 #good
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 2] <- 0 #bad
## Überprüfung der neuen Variable
table(EuBaDaten$Einstellung3)

##Bildung neu kodieren ##
#neue Variable um Bildung in primär, senkundär, tertiär und noch im 
#Bildungssystem zu Kategorisieren
EuBaDaten$BJ_gruppiert <- NA

# Löschen der Fälle mit den Antworten 97 und 98
EuBaDaten <- EuBaDaten[!(EuBaDaten$Bildungsjahre_kategorisiert %in% c(97, 98)), ]
# Umkodierung von 0-15 BJ zu 0
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 1] <- 0 #bis 14
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 2] <- 0 #15
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 11] <- 0 #nofulltime
# Umkodierung von 16-19 BJ zu 1
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert >= 3 & EuBaDaten$Bildungsjahre_kategorisiert <= 6] <- 1 
# Umkodierung über 20 BJ zu 2
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert >= 7 & EuBaDaten$Bildungsjahre_kategorisiert <= 9] <- 2 
#Umkodierung noch im Bildungssystem zu 4
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 10] <- 3
# Überprüfung der neuen Variable
table(EuBaDaten$BJ_gruppiert)


#Bildung 3geteilt ohne noch in Ausbildung !!!!!!!!!!!!!
#EuBaDaten$BildKat3 <- EuBaDaten$BJ_gruppiert
#EuBaDaten$BildKat3

#Bildung gruppiert neu kodiert: falsch, weil ordinal angenommen & noch in Studium als höchstes angenommen
EuBaDaten$Bildungsjahre_recoded <- EuBaDaten$Bildungsjahre_kategorisiert     # Umkodierung der Antwortkategorien
EuBaDaten <- EuBaDaten[!(EuBaDaten$Bildungsjahre_recoded %in% c(97, 98)), ]# Löschen der Fälle mit den Antworten 97 und 98
EuBaDaten$Bildungsjahre_recoded[EuBaDaten$Bildungsjahre_recoded == 11] <- 0  # Umkodierung der Antwortkategorie 11 (no full time) zu 0

###Ländernamen zum mergen
EuBaDaten$Entity <- NA
EuBaDaten$Entity[EuBaDaten$Land == 1] <- "France"
EuBaDaten$Entity[EuBaDaten$Land == 2] <- "Belgium"
EuBaDaten$Entity[EuBaDaten$Land == 3] <- "Netherlands"
EuBaDaten$Entity[EuBaDaten$Land == 4] <- "Germany"
EuBaDaten$Entity[EuBaDaten$Land == 5] <- "Italy"
EuBaDaten$Entity[EuBaDaten$Land == 6] <- "Luxembourg"
EuBaDaten$Entity[EuBaDaten$Land == 7] <- "Denmark"
EuBaDaten$Entity[EuBaDaten$Land == 8] <- "Ireland"
EuBaDaten$Entity[EuBaDaten$Land == 9] <- "United Kingdom"
EuBaDaten$Entity[EuBaDaten$Land == 11] <- "Greece"
EuBaDaten$Entity[EuBaDaten$Land == 12] <- "Spain"
EuBaDaten$Entity[EuBaDaten$Land == 13] <- "Portugal"
EuBaDaten$Entity[EuBaDaten$Land == 16] <- "Finland"
EuBaDaten$Entity[EuBaDaten$Land == 17] <- "Sweden"
EuBaDaten$Entity[EuBaDaten$Land == 18] <- "Austria"
EuBaDaten$Entity[EuBaDaten$Land == 19] <- "Cyprus"
EuBaDaten$Entity[EuBaDaten$Land == 20] <- "Czechia"
EuBaDaten$Entity[EuBaDaten$Land == 21] <- "Estonia"
EuBaDaten$Entity[EuBaDaten$Land == 22] <- "Hungary"
EuBaDaten$Entity[EuBaDaten$Land == 23] <- "Latvia"
EuBaDaten$Entity[EuBaDaten$Land == 24] <- "Lithuania"
EuBaDaten$Entity[EuBaDaten$Land == 25] <- "Malta"
EuBaDaten$Entity[EuBaDaten$Land == 26] <- "Poland"
EuBaDaten$Entity[EuBaDaten$Land == 27] <- "Slovakia"
EuBaDaten$Entity[EuBaDaten$Land == 28] <- "Slovenia"
EuBaDaten$Entity[EuBaDaten$Land == 29] <- "Bulgaria"
EuBaDaten$Entity[EuBaDaten$Land == 30] <- "Romania"
#EuBaDaten$Entity[EuBaDaten$Land == 32] <- "Croatia"

##############################GDP logaritmiert in die MEA aufnehmen?????????? Quadriert plus normal?????
##GDP Datensatz   
GDP <- filter(gdp, Year == 2009)  #2009 rausfiltern 
#Länder filtern #kein "Croatia"
eu_laender <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia",
                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
                "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
                "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

GDP2 <- subset(GDP, Entity %in% eu_laender)

#Continent, Year, Population, Code löschen
GDP2$Continent <- NULL
GDP2$Year <- NULL
GDP2$Code <- NULL
colnames(GDP2)[4] <- "Population"
GDP2$Population <- NULL

#Finalen Datensatz erstellen
DatensatzGesamt <- merge(EuBaDaten, GDP2, by = "Entity")                     #Datensätze zusammenfügen
DatensatzGesamt$Bildungsjahre <- NULL                                        #Bildungsjahre Alter
DatensatzGesamt$Geschlecht <- NULL                                           #Geschlecht alte Kodierung
DatensatzGesamt$`GDP per capita (output, multiple price benchmarks)` <- NULL #GDP unnötig
colnames(DatensatzGesamt)[13] <- "GDPpcapita2009"                            #Umbenennung Spalte

# Neue quadrierte BIPVariable hinzufügen
DatensatzGesamt <- DatensatzGesamt %>% 
  mutate(bip_squared = GDPpcapita2009^2)  ##quadriertes BIP nicht signifikant, daher mit normalem weitergerechnet!


EUeintritt_table <- data.frame(
  Land = eu_laender,  # Ländernamen in der gleichen Reihenfolge wie in deinem Datensatz
  Mitgliedsdauer = c(1995, 1958, 2007, 2013, 2004, 2004, 1973, 2004, 1995, 1958, 1958, 1981, 2004, 1973, 1958, 2004, 2004, 1958, 2004, 1958, 2004, 1986, 2007, 2004, 2004, 1986, 1995, 1973))


DatensatzGesamt$Eintritt_in_EU <- EUeintritt_table$Mitgliedsdauer[match(DatensatzGesamt$Entity, EUeintritt_table$Land)]  

DatensatzGesamt$Mitgliedsdauer <- 2010 - DatensatzGesamt$Eintritt_in_EU



write_sav(DatensatzGesamt, "DatenProbe.sav")
