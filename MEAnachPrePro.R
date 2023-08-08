###Mehrebenenanalyse

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

DataMEA <- read_sav("DatenMEA.sav")

#Daten zentrieren, skalieren?
#Nullmodelle

####Leermodell auf DatensatzGesamt Einstellung 2-geteilt (ohne weights-Zusatz)
Leermodell2 <- glmer(EinstellungDichotom ~ ( 1 | Entity),
              data=DataMEA,
              family = "binomial",
)
summary(Leermodell2)

htmlreg(Leermodell2)
screenreg(Leermodell2)
fileLM2="Leermodell2.html"
#ICC berechnen Adjusted ICC: 0.074 --> 7,4% der Varianz von Einstellung liegt zwischen Ländern
icc(Leermodell2)
#Fixed Effects und Random Effects (die Abweichungen von dem geschätzten fixed Wert pro Gruppe))
fixef(Leermodell2)
ranef(Leermodell2)



####Leermodell auf DatensatzGesamt Einstellung 3-geteilt (ohne weights-Zusatz)
Leermodell3 <- lmer(Einstellung3 ~ 1 + ( 1 | Entity),
             data=DataMEA)

summary(Leermodell3)

htmlreg(Leermodell3)
fileLM3="Leermodell3.html"
screenreg(Leermodell3)
#ICC berechnen Adjusted ICC: 0.053--> 5,3% der Varianz von Einstellung liegt zwischen Ländern
icc(Leermodell3)
#Fixed Effects und Random Effects (die Abweichungen von dem geschätzten fixed Wert pro Gruppe))
fixef(Leermodell3)
ranef(Leermodell3)




#2 und 3 Einstellung testen
#BIP auch quadriert
#MEA

#Zentrierung am Gesamtmittelwert: cGM(centered Grand Mean)
#am Gesamtmittelwert:
#alle intervallskalierten Prädiktoren auf Individualebene
DataMEA$BJ_gruppiert_cGM <- center(DataMEA$BJ_gruppiert, type = "CGM")
DataMEA$Alter_gruppiert_cGM <- center(DataMEA$Alter_gruppiert, type = "CGM")
DataMEA$Alter_gruppiert_cGM <- center(DataMEA$Alter_gruppiert, type = "CGM")
#DataMEA$Mitgliedsdauer_cGM <- center(DataMEA$Mitgliedsdauer, type = "CGM")
