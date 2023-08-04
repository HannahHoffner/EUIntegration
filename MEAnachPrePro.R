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

DataMEA <- read_sav("DatenMEA.sav")