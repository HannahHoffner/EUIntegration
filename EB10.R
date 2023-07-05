library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

#### To Do ####


#stargazer() hübschen Output: 
##bivariat: Bildung--> Einstellung, 
##deskriptive??
#Mehrebenenanalyse



#### Datensatz Eurobarometer einlesen ####

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


#NAs löschen 

clean_data <- na.omit(df_1)


#Geschlecht
clean_data$Geschlecht <- NA

clean_data$Geschlecht[clean_data$Gender == 1] <- 1 #male
clean_data$Geschlecht[clean_data$Gender == 2] <- 0 #female



#ZustimmungDichotom: good=1, bad&neither good nor bad=0, dk=löschen (??)
## Erstellung einer neuen Variable "EinstellungDichotom" mit NAs als Standardwert
clean_data$EinstellungDichotom <- NA

## Umkodierung der Antwortkategorien und Zuordnung zu "EinstellungDichotom"
clean_data$EinstellungDichotom[clean_data$Zustimmung_EU == 1] <- 1
clean_data$EinstellungDichotom[clean_data$Zustimmung_EU %in% c(2, 3)] <- 0

## Löschen der Fälle mit den Antworten 4 (dont know) und 9(inap)
clean_data <- clean_data[!clean_data$Zustimmung_EU %in% c(4, 9), ]

## Überprüfung der neuen Variable
table(clean_data$EinstellungDichotom)

#Bildung gruppiert neu kodiert:
# Umkodierung der Antwortkategorien von BJ_recoded in clean_data
clean_data$BJ_gruppiert <- clean_data$BJ_recoded

# Löschen der Fälle mit den Antworten 97 und 98
clean_data <- clean_data[!(clean_data$BJ_gruppiert %in% c(97, 98)), ]

# Umkodierung der Antwortkategorie 11 (no full time) zu 0
clean_data$BJ_gruppiert[clean_data$BJ_gruppiert == 11] <- 0

# Überprüfung der neuen Variable
table(clean_data$BJ_gruppiert)

table(clean_data$Land)

#Ländernamen zum mergen 


clean_data$Entity <- NA

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


##wie Deutschland zu OD und WD 
##Ireland vs. Northern Ireland, NI nicht im GDP Datensatz



#### Datensatz GDP Inflationsbereinigt und Lebensunterhaltkosten ####


gdp_per_capita_bereinigt <- read_csv("gdp-per-capita-inflation--and-ppp-adjusted-world-bank-data-vs-penn-world-table-data.csv")

#2009 rausfiltern 

gdp_per_capita_b1 <- filter(gdp_per_capita_bereinigt, Year == 2009)

#Länder filtern

eu_laender <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czech Republic",
                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
                "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
                "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")

gdp_per_capita_b2 <- subset(gdp_per_capita_b1,
                            Entity %in% eu_laender)


#Continent, Year, Population, Code löschen

gdp_per_capita_b2$Continent <- NULL
gdp_per_capita_b2$Year <- NULL
gdp_per_capita_b2$Code <- NULL

colnames(gdp_per_capita_b2)[4] <- "Population"
gdp_per_capita_b2$Population <- NULL


#### Datensätze zusammenfügen ####

df_final <- merge(clean_data, gdp_per_capita_b2, by = "Entity")

colnames(df_final)[12] <- "egal"

df_final$egal <- NULL

colnames(df_final)[11] <- "GDPpcapita2009"

#### Deskriptive Statistik #####

#UV: Bildung, AV: Einstellung

## Erstellung der Kreuztabelle mit Absolutwerten
cross_table <- table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert)
cross_table

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert), margin = 2) * 100
cross_table2

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_table2_df <- as.data.frame(cross_table)


# Plot der Verteilung
ggplot(data = cross_table2_df, 
       aes(x = Var2, 
           y = Freq, 
           fill = as.factor(Var1)
           )
       ) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bildungsjahre",
       y = "Prozentualer Anteil", 
       fill = "EinstellungDichotom") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red")) +
  theme_minimal()





#### Logistische Regression ####

##Poppe VL

fit_logit <- glm(formula = any_contact~(age+sex+eastwest),
                 family = binomial(link = "logit"),
                 data=df_allbus)

fit_logit

#Odds Ratio und individuelle marignale Effekte (IME) und AME
