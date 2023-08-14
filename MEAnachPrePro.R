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
                    Bildungsjahre = v553,
                    Bildungsjahre_kategorisiert = v554,
                    Geschlecht = v555,
                    Alter = v556,
                    Alter_gruppiert = v557
)

###NAs löschen
EuBaDaten <- na.omit(EuBa_Vars)

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
#Umkodierung noch im Bildungssystem zu 3
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 10] <- 3
# Überprüfung der neuen Variable
table(EuBaDaten$BJ_gruppiert)


#Bildung 3geteilt: noch in Studium auch Gruppe tertiäre Bildung
#EuBaDaten$BJ3 <- EuBaDaten$BJ_gruppiert
# Ersetze alle Werte 3 in BJ3 durch den Wert 2
#EuBaDaten$BJ3[EuBaDaten$BJ_gruppiert == 3] <- 2

#Still Studying in andere einsortieren
umwandlung_bildung <- function (BJ_gruppiert, Alter){
  ifelse(BJ_gruppiert == 3,
         ifelse(Alter >=20, 2,
                ifelse (Alter >=16, 1, 0)
                ),
         ifelse(BJ_gruppiert == 3, 0, BJ_gruppiert))
}

EuBaDaten <- EuBaDaten %>%
  mutate (BJ_gruppiert = umwandlung_bildung(BJ_gruppiert, Alter))
table(EuBaDaten$BJ_gruppiert)


#Umwandlung as.factor
#EuBaDaten$BJ_gruppiert <- as.factor(EuBaDaten$BJ_gruppiert)
EuBaDaten$EinstellungDichotom <- as.factor(EuBaDaten$EinstellungDichotom)
EuBaDaten$Geschlecht_recoded <- as.factor(EuBaDaten$Geschlecht_recoded)
#EuBaDaten$Alter_gruppiert <- as.factor(EuBaDaten$Alter_gruppiert)


# Überprüfung des Typs der umgewandelten Spalte
print(paste("BJ_gruppiert ist vom Typ:", class(EuBaDaten$BJ_gruppiert)))
print(paste("Einstellung Dichotom ist vom Typ:", class(EuBaDaten$EinstellungDichotom)))
print(paste("Geschlecht_recoded ist vom Typ:", class(EuBaDaten$Geschlecht_recoded)))
print(paste("Alter ist vom Typ:", class(EuBaDaten$Alter_gruppiert)))

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
#Länder filtern #kein "Croatia" (Eintritt 2013)
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
colnames(DatensatzGesamt)[12] <- "GDPpcapita2009"                            #Umbenennung Spalte

# Neue quadrierte BIPVariable hinzufügen
DatensatzGesamt <- DatensatzGesamt %>% 
  mutate(bip_squared = GDPpcapita2009^2)  ##quadriertes BIP nicht signifikant, daher mit normalem weitergerechnet!

EUeintritt_table <- data.frame(
  Land = eu_laender,  # Ländernamen in der gleichen Reihenfolge wie in Datensatz
  Mitgliedsdauer = c(1995, 1958, 2007, 2004, 2004, 1973, 2004, 1995, 1958, 1958, 1981, 2004, 1973, 1958, 2004, 2004, 1958, 2004, 1958, 2004, 1986, 2007, 2004, 2004, 1986, 1995, 1973))
#Datensätze matchen mit Eintrittsdauer
DatensatzGesamt$Eintritt_in_EU <- EUeintritt_table$Mitgliedsdauer[match(DatensatzGesamt$Entity, EUeintritt_table$Land)]  

#2010 minus Eintritt = Mitgliedsdauer
DatensatzGesamt$Mitgliedsdauer <- 2010 - DatensatzGesamt$Eintritt_in_EU
write_sav(DatensatzGesamt, "DatenMEA2.sav")

#-----------------Mehrebenenanalyse -------------------------------------------------------

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
library(texreg)     #Tabelle html/latex
library(sjPlot)     #Random Effects plotten

DataMEA <- read_sav("DatenMEA2.sav")


#Daten zentrieren, skalieren?
#Nullmodelle

####Leermodell auf DatensatzGesamt Einstellung 2-geteilt (ohne weights-Zusatz)
Leermodell2 <- glmer(EinstellungDichotom ~ 1 + ( 1 | Entity),
              data=DataMEA,
              family = "binomial",
)
summary(Leermodell2)


htmlreg(Leermodell2)
screenreg(Leermodell2)
fileLM2="Leermodell2.html"
#ICC berechnen Adjusted ICC: 0.078 --> 7,8% der Varianz von Einstellung liegt zwischen Ländern
icc(Leermodell2)
#Fixed Effects und Random Effects (die Abweichungen von dem geschätzten fixed Wert pro Gruppe))
fixef(Leermodell2)
ranef(Leermodell2)

#Random Effects in Tabelle:
library(dplyr)
library(kableExtra)

# Annahme: ranef_result enthält die Ergebnisse von ranef(Leermodell2)
ranef_df <- as.data.frame(ranef_result$Entity)

# Erstelle eine Tabelle mit kableExtra
ranef_table <- ranef_df %>%
  kable("html") %>%
  kable_styling(full_width = FALSE)

# Drucke die Tabelle
print(ranef_table)


####Leermodell auf DatensatzGesamt Einstellung 3-geteilt (ohne weights-Zusatz)
Leermodell3 <- lmer(Einstellung3 ~ 1 + ( 1 | Entity),
             data=DataMEA)

summary(Leermodell3)

htmlreg(Leermodell3)
fileLM3="Leermodell3.html"
screenreg(Leermodell3)
#ICC berechnen Adjusted ICC: 0.058--> 5,8% der Varianz von Einstellung liegt zwischen Ländern
icc(Leermodell3)
#Fixed Effects und Random Effects (die Abweichungen von dem geschätzten fixed Wert pro Gruppe))
fixef(Leermodell3)
ranef(Leermodell3)

#Zentrierung am Gesamtmittelwert: cGM(centered Grand Mean)
#alle intervallskalierten Prädiktoren auf Individualebene
DataMEA$BJ_gruppiert_cGM <- center(DataMEA$BJ_gruppiert, type = "CGM")
DataMEA$Alter_gruppiert_cGM <- center(DataMEA$Alter_gruppiert, type = "CGM")
#nicht bei dichotomen Variablen nötig! (Durchschnittsperson = Frau, durchschnittliches Alter, durchschnittliche Bildungsgruppe)

#Random Intercept-Modell mit Variablen der Individualebene: Einstellung 2, Bildung 3
RIM2 <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded+ (1 | Entity), 
             data = DataMEA,
             family = "binomial",
             )
summary(RIM2)

#Random Slope-Modell mit Variablen der Individualebene
RSM2 <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded + 
              (1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded | Entity), 
              data = DataMEA, 
              family = "binomial")
summary(RSM2) #hier vllt nur random slope für BJ, nicht alter und geschlecht??? --> vergleichen, was bessere Ergebnisse liefert

#Random Slope-Modell mit Variablen der Individualebene aber nur Random Slope für BJ
RSM22 <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded + 
                (1 + BJ_gruppiert_cGM | Entity), 
              data = DataMEA, 
              family = "binomial")
summary(RSM22) #schlechter als random slope auf allen Variablen

#Vergleich:
anova(RIM2, RSM2) 
#     npar   AIC   BIC logLik deviance  Chisq Df
#RIM2    5 31473 31513 -15731    31463          
#RSM2   14 31381 31495 -15677    31353 109.08  9

anova(RIM2, RIM2BJ3)
#        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
#RIM2       5 32657 32697 -16323    32647                     
#RIM2BJ3    5 32649 32690 -16320    32639 7.7014  0

anova(RSM2, RSM22)
#      npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
#RSM22    7 32636 32693 -16311    32622                         
#RSM2    14 32558 32672 -16265    32530 91.989  7  < 2.2e-16 ***

anova(RIM2BJ, RSM2BJ)
#       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
#RIM2BJ    5 29925 29965 -14957    29915                         
#RSM2BJ   14 29846 29959 -14909    29818 96.489  9  < 2.2e-16 ***

anova(RSM2BJ, RSM22BJ)
#        npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
#RSM22BJ    7 29919 29975 -14952    29905                         
#RSM2BJ    14 29846 29959 -14909    29818 86.252  7  7.266e-16 ***

#--------------------Miteinbezug Kontextebene--------------------------------------
#Zentrierung am Gesamtmittelwert: cGM(centered Grand Mean)
#alle intervallskalierten Prädiktoren auf Kontextebene
DataMEA$Mitgliedsdauer_cGM <- center(DataMEA$Mitgliedsdauer, type = "CGM")
DataMEA$GDPpcapita2009_cGM <- center(DataMEA$GDPpcapita2009, type = "CGM")
DataMEA$bip_squared_cGM <- center(DataMEA$bip_squared, type = "CGM")

#auch für BJ 3
#Zentrierung am Gesamtmittelwert: cGM(centered Grand Mean)
#alle intervallskalierten Prädiktoren auf Kontextebene
DataMEA_filtered$Mitgliedsdauer_cGM <- center(DataMEA_filtered$Mitgliedsdauer, type = "CGM")
DataMEA_filtered$GDPpcapita2009_cGM <- center(DataMEA_filtered$GDPpcapita2009, type = "CGM")
DataMEA_filtered$bip_squared_cGM <- center(DataMEA_filtered$bip_squared, type = "CGM")

fixed_slope_modell <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded + 
                             GDPpcapita2009_cGM + Mitgliedsdauer_cGM + 
                             (BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded | Entity) + 
                             (GDPpcapita2009_cGM + Mitgliedsdauer_cGM | Entity), 
                           data = DataMEA, family = "binomial")


# Modell mit Random Slopes auf Individual- und Kontextebene
random_slope_modell <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded + 
                               GDPpcapita2009_cGM + Mitgliedsdauer_cGM + 
                               (1 + Alter_gruppiert_cGM + Geschlecht_recoded | Entity) + 
                               (1 + GDPpcapita2009_cGM + Mitgliedsdauer_cGM | Entity), 
                             data = DataMEA, family = "binomial")

# Modell fitting und Zusammenfassung
summary(fixed_modell)


# Modell mit Random Slopes auf Individual- und Kontextebene
random_slope_modell <- glmer(EinstellungDichotom ~ 1 + BJ_gruppiert_cGM + Alter_gruppiert_cGM + Geschlecht_recoded + 
                               GDPpcapita2009_cGM + Mitgliedsdauer_cGM + 
                               (1 + Alter_gruppiert_cGM + Geschlecht_recoded | Entity) + 
                               (1 + GDPpcapita2009_cGM + Mitgliedsdauer_cGM | Entity), 
                             data = DataMEA, family = "binomial")

# Modell fitting und Zusammenfassung
summary(random_slope_modell)

#Korrelationen überprüfen:
correlation_matrix <- cor(DataMEA[, c("BJ_gruppiert_cGM", "Alter_gruppiert_cGM", "Geschlecht_recoded", "GDPpcapita2009_cGM", "Mitgliedsdauer_cGM")])
print(correlation_matrix)



#Crosslevel-Interaktionen überprüfen:
crosslevel <- glmer(EinstellungDichotom ~ BJ_gruppiert_cGM + Alter_gruppiert_cGM + 
                  Geschlecht_recoded + GDPpcapita2009_cGM + Mitgliedsdauer_cGM + 
                  BJ_gruppiert_cGM:GDPpcapita2009_cGM + (1 | Entity), 
                  family = "binomial", data = DataMEA)
summary(crosslevel)

#Plus Cross-Level-Interaction
crosslevel <- glmer(EinstellungDichotom ~ BJ_gruppiert_cGM
                      + Alter_gruppiert_cGM 
                      + Geschlecht_recoded 
                      + GDPpcapita2009_cGM
                      + BJ_gruppiert_cGM:GDPpcapita2009_cGM
                      + (1 + BJ_gruppiert_cGM + Geschlecht_recoded + Alter_gruppiert_cGM|| Entity),
                      data = DataMEA,
                      family = "binomial",
                      )
summary(crosslevel)

#Plus Cross-Level-Interaction BJ3
crosslevelBJ <- glmer(EinstellungDichotom ~ BJ_gruppiert_cGM
                    + Alter_gruppiert_cGM 
                    + Geschlecht_recoded 
                    + GDPpcapita2009_cGM
                    + BJ_gruppiert_cGM:GDPpcapita2009_cGM
                    + (1 + BJ_gruppiert_cGM + Geschlecht_recoded + Alter_gruppiert_cGM|| Entity),
                    data = DataMEA_filtered,
                    family = "binomial",
)
summary(crosslevelBJ)