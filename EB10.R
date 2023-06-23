library(haven)
library(tidyverse)
library(dyplr)

EB10 <- read_sav("ZA5234_v2-0-1.sav")

#### Datenbereinigung ####


df_1 <- select(EB10,
               v6, #Land
               v206, #Zustimmung EU
               v554, #BJ
               v555, #gender
               v556 #Alter  
               ) 
