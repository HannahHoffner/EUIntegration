library(haven)
library(psych)
library(ggplot2)



#### Deskriptive Statistik ####

##Datensatz einlesen 

DatenMEA <- read_sav("DatenMEA.sav")

Daten <- read_sav("DatenProbe.sav")


####Univariate Untersuchung ####

#### Individualebene ####


#### Abhängig Variable: Zustimmung ####

#Dichtom

Zustimmung2 <- table(DatenMEA$EinstellungDichotom)
Zustimmung2

#0: 12.083 Ablehnung, weder noch
#1: 12.299 Zustimmung

WSK2 <- prop.table(table(DatenMEA$EinstellungDichotom))
WSK2

#Drei-gliedrig

Zustimmung3 <- table(DatenMEA$Einstellung3)
Zustimmung3

#0: 12.299 Zustimmung
#1: 4.216  weder noch
#2: 7.867  Ablehnung

WSK3 <- prop.table(table(DatenMEA$Einstellung3))
WSK3

##Vergleich mit Benefit

#Spearmann

weighted_spearman_cor <- cor.test(
  DatensatzGesamt$Benefit_recoded,
  DatensatzGesamt$EinstellungDichotom,
  method = "spearman",
  alternative = "two.sided", 
  weights = DatensatzGesamt$Land_Gewichtung
)

print(weighted_spearman_cor)

#cronbachs alpha

alpha_result <- alpha(DatensatzGesamt[, c("Benefit_recoded", "EinstellungDichotom")], 
                      check.keys = TRUE)  # Prüfen, ob alle Werte ordinales Format haben
# Cronbachs Alpha ausgeben
print(alpha_result$total$raw_alpha)


#### Unabhängige Variable ####


##Bildungsjahre_gruppiert 

BJdeskriptiv <- table(DatenMEA$BJ_gruppiert)
BJdeskriptiv

#0: 4.817 (0-15 BJ)
#1: 10.224 (16-19 BJ)
#2: 7.245 (über 20 BJ)
#4: 2.096 (noch im Bildungssystem)

WSKBJ <- prop.table(table(DatenMEA$BJ_gruppiert))
WSKBJ

##Alter 

Alterdeskriptiv <- table(DatenMEA$Alter_gruppiert)
Alterdeskriptiv

#1: 2.960 (15-24 Jahre)
#2: 5.723 (25-39 Jahre)
#3: 6.367 (40-54 Jahre)
#4: 9.332 (55 Jahre oder älter)

WSKAlter <- prop.table(table(DatenMEA$Alter_gruppiert))
WSKAlter

##Geschlecht

Geschlechtdeskriptiv <- table(DatenMEA$Geschlecht_recoded)
Geschlechtdeskriptiv

#0: 13.047 female
#1: 11.335 male

WSKGeschlecht <- prop.table(table(DatenMEA$Geschlecht_recoded))
WSKGeschlecht

  
#### Länderebene ####

##BIP pro Kopf

##Dauer der Mitgliedschaft

Dauer <- table(DatenMEA$Mitgliedsdauer)
Dauer
  
  
  
  
#### Bivariate Untersuchung ####

#### BJ und Zustimmung #### 

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(Daten$EinstellungDichotom, Daten$BJ_gruppiert),margin = 2, Daten$Gewichtung_Land) * 100
cross_table2

#Gewichtete Kreuztabelle
weighted_table <- tapply(Daten$Gewichtung_Land, list(Daten$EinstellungDichotom, Daten$BJ_gruppiert), sum)
prop_table_weighted <- prop.table(weighted_table, margin = 2) * 100
prop_table_weighted

#Barplot
ggplot(data = Daten, 
       aes(x = as.factor(BJ_gruppiert),
           fill = as.factor(EinstellungDichotom.x),
      weights = Gewichtung_Land
       )
) +
  geom_bar(position = "fill") +
  labs(x = "Bildungsjahre kategorisiert",
       y = "Prozentualer Anteil",
       fill = "Einstellung zur EU"
  ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung")
  ) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Einstellung zur EU (in Prozent) nach kategorisierten Bildungsjahren")+
  scale_x_discrete(labels = c("0 - 15 Bildungsjahre", 
                            "16 - 19 Bildungsjahre", 
                            "über 20 Bildungsjahre", 
                            "noch im Bildungssystem")
                 )

#### Alter und Einstellung ####

#Gewichtete Kreuztabelle
wtable <- tapply(Daten$Gewichtung_Land, list(Daten$EinstellungDichotom, Daten$Alter_gruppiert), sum)
wtable2 <- prop.table(wtable, margin = 2) * 100
wtable2

#Barplot
ggplot(data = Daten, 
       aes(x = as.factor(Alter_gruppiert),
           fill = as.factor(EinstellungDichotom),
           weights = Gewichtung_Land
       )
) +
  geom_bar(position = "fill") +
  labs(x = "Alter kategorisiert",
       y = "Prozentualer Anteil",
       fill = "Einstellung zur EU"
  ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung")
  ) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Einstellung zur EU (in Prozent) nach kategorisiertem Alter")+
  scale_x_discrete(labels = c("15 - 24 Jahre", 
                              "25 - 39 Jahre", 
                              "40 - 54 Jahre", 
                              "55 Jahre oder älter")
  )

#### Geschlecht und Einstellung #### 

#Gewichtete Kreuztabelle
wtableG <- tapply(Daten$Gewichtung_Land, list(Daten$EinstellungDichotom, Daten$Geschlecht_recoded), sum)
wtableG1 <- prop.table(wtableG, margin = 2) * 100
wtableG1

#Barplot
ggplot(data = Daten, 
       aes(x = as.factor(Geschlecht_recoded),
           fill = as.factor(EinstellungDichotom),
           weights = Gewichtung_Land
       )
) +
  geom_bar(position = "fill") +
  labs(x = "Geschlecht",
       y = "Prozentualer Anteil",
       fill = "Einstellung zur EU"
  ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung")
  ) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Einstellung zur EU (in Prozent) nach kategorisiertem Geschlecht")+
  scale_x_discrete(labels = c("weiblich", 
                              "männlich")
                   )

#### GDP per capita und Einstellung ####

#LänderMW

mittelwerte <- aggregate(EinstellungDichotom ~ Entity, 
                         data = Daten,
                         weights = Gewichtung_Land,
                         FUN = mean)

#neue Variable

Daten <- merge(Daten, mittelwerte, by = "Entity", all.x = TRUE)
names(Daten)[names(Daten) == "EinstellungDichotom.y"] <- "MWEinstellung"

#Grafik

ggplot(Daten, 
       aes(x = GDPpcapita2009,
           y = MWEinstellung, 
           label = Entity
       )
) +
  geom_point(size = 4, color = "darkblue", alpha = 0.8) +
  geom_text(hjust = 0, vjust = 0, color = "black", fontface = "bold", size = 2.65)+
  labs(x = "BIP pro Kopf 2009 (in $)",
       y = "Einstellung zur EU (nach Ländermittelwerten)",
       title = "Einstellung zur EU nach BIP pro Kopf 2009")+
  theme_minimal()
#+
#theme(plot.margin = margin(1, 1, 2, 2, "cm"))

#### MitgliedsDauer und Einstellung ####

ggplot(Daten, 
       aes(x = Mitgliedsdauer,
           y = MWEinstellung, 
           label = Entity
       )
) +
  geom_point(size = 4, color = "darkblue", alpha = 0.8) +
  geom_text(hjust = 0, vjust = 0, color = "black", fontface = "bold", size = 2.65)+
  labs(x = "Mitgliedsdauer in Jahren",
       y = "Einstellung zur EU (nach Ländermittelwerten)",
       title = "Einstellung zur EU nach Dauer der Mitgliedschaft")+
  theme_minimal()



