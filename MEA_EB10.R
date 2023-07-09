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
gdp_bereinigt <- read_csv("gdp-per-capita-inflation--and-ppp-adjusted-world-bank-data-vs-penn-world-table-data.csv") #GDP