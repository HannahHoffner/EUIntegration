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

# Umkodierung der Antwortkategorie 11 zu 0
clean_data$BJ_gruppiert[clean_data$BJ_gruppiert == 11] <- 0

# Überprüfung der neuen Variable
table(clean_data$BJ_gruppiert)

#UV: Bildung, AV: Einstellung
## Laden des Pakets für Datenvisualisierung
library(ggplot2)

## Erstellung der Kreuztabelle mit Absolutwerten
cross_table <- table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert)
cross_table

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert), margin = 2) * 100
cross_table2

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_table2_df <- as.data.frame(cross_table)

# Plot der Verteilung
ggplot(data = cross_table2_df, aes(x = Var2, y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bildungsjahre", y = "Prozentualer Anteil", fill = "EinstellungDichotom") +
  scale_fill_manual(values = c("1" = "blue", "0" = "red")) +
  theme_minimal()




#Geschlecht auf 0 & 1

#BJrecodiert: 10 still studying 
# 11 no full time education 
# 97 refusal 
# 98 dk

unique(df_1$BJ_recoded)



#### Deskriptive Statistik ####

plot(clean_data$BJ_recoded, clean_data$Zustimmung_EU)


