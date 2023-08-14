library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)



#### Deskriptive Statistik ####

#### TO DO ####

#### AV: Zustimmung ####

p2 <- table(df_final$EinstellungDichotom)

p2

#0: Ablehnung 10.716
#1: Zustimmung 11518

#### UV: Bildungsjahre ####


#Überblick

prop.table(table(df_final$BJ_gruppiert))

prop.table(table(df_final$BJ_gruppiert))*100

#irgendwie alle bei 9 und ansonsten konstant in Prozent 

barplot(table(df_final$BJ_gruppiert), 
        col = "lightblue", 
        main = "Verteilung der Bildungsjahre",
        xlab = "Anzahl der Bildungsjahre (Kategorisiert)", 
        ylab = "Absolute Häufigkeiten"
)

##Legende 

legend("topleft",
       c("1: bis zu 14 Bildungsjahren",
           "2: 15 Bildungsjahre",
           "3: 16 Bildungsjahre",
           "4: 17 Bildungsjahre",
           "5: 18 Bildungsjahre",
           "6: 19 Bildungsjahre",
           "7: 20 Bildungsjahre",
           "8: 21 Bildungsjahre",
           "9: 22 Bildungsjahre und mehr",
           "10: still studying"),
       pch = 15, 
       cex = 0.4
)



#### Alter kategorisiert und Zustimmung ####

## Erstellung der Kreuztabelle mit Absolutwerten
cross_tableAlter <- table(df_final$EinstellungDichotom, df_final$AlterGruppiert)
cross_tableAlter

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_tableAlter2 <- prop.table(table(df_final$EinstellungDichotom, df_final$AlterGruppiert),margin = 2) * 100
cross_tableAlter2

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_tablealter2_df <- as.data.frame(cross_tableAlter2)

#Erläuterung Kategorien 

category_labels <- c("1: 19 - 24 Jahre", 
                     "2: 25 - 39 Jahre", 
                     "3: 40 - 54 Jahre", 
                     "4: 55 Jahre oder älter")

text_plot <- tableGrob(data.frame(category = category_labels), rows = NULL)


# Create a stacked bar plot with modified legend labels and y-axis encoding
plotA <- ggplot(data = df_final, 
       aes(x = as.factor(AlterGruppiert),
           fill = as.factor(EinstellungDichotom)
       )
) +
  geom_bar(position = "fill") +
  labs(x = "Lebensjahre kategorisiert",
       y = "Prozentualer Anteil",
       fill = "Einstellung zur EU"
  ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung")
  ) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Einstellung zur EU (in Prozent) nach kategorisierten Lebensjahren")

plotA

#Grafiken zusammenfügen

library(gridExtra)

plot_alter <- grid.arrange(plotA, text_plot, ncol = 2, widths = c(0.7, 0.25))

#Zustimmung sinkt mit Alter

#### Bildung (kategorisiert) und Zustimmung ####

#UV: Bildung, AV: Einstellung

## Erstellung der Kreuztabelle mit Absolutwerten
cross_table <- table(df_final$EinstellungDichotom, df_final$BJ_gruppiert)
cross_table

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert),margin = 2) * 100
cross_table2

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_table2_df <- as.data.frame(cross_table2)

library(ggplot2)
# Plot der Verteilung
ggplot(data = cross_table2_df, 
       aes(x = Var2, 
           y = Freq, 
           fill = as.factor(Var1)
       )
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Bildungsjahre kategorisiert",
       y = "Anteil in %", 
       fill = "Einstellung zur EU"
       ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung"
                               )
                    ) +
  theme_gray()

# Create a stacked bar plot with modified legend labels and y-axis encoding
plotB <- ggplot(data = df_final, 
       aes(x = as.factor(BJ_gruppiert),
           fill = as.factor(EinstellungDichotom)
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
  ggtitle("Einstellung zur EU (in Prozent) nach kategorisierten Bildungsjahren")


k1 <- c( "0: keine Vollzeitbildung",
         "1: bis zu 14 Bildungsjahren",
                     "2: 15 Bildungsjahre",
                     "3: 16 Bildungsjahre",
                     "4: 17 Bildungsjahre",
                     "5: 18 Bildungsjahre",
                     "6: 19 Bildungsjahre",
                     "7: 20 Bildungsjahre",
                     "8: 21 Bildungsjahre",
                     "9: 22 Bildungsjahre und mehr",
                     "10: noch im Bildungssystem")


text_plot2 <- tableGrob(data.frame(category = k1), rows = NULL)



plot_BJ <- grid.arrange(plotB, text_plot2, ncol = 2, widths = c(0.6, 0.25))

#Zustimmung steigt mit zunehmenden BJ

#### Geschlecht und Zustimmung  ####

#0 female 
#1 male 

## Erstellung der Kreuztabelle mit Absolutwerten
cross_tableGeschlecht <- table(df_final$EinstellungDichotom, df_final$Geschlecht)
cross_tableGeschlecht

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_tableGeschlecht <- prop.table(table(df_final$EinstellungDichotom, df_final$Geschlecht),margin = 2) * 100
cross_tableGeschlecht

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_tableGeschlecht<- as.data.frame(cross_tableGeschlecht)

#Erläuterung Kategorien 

k2 <- c("0: Frau", 
        "1: Mann")

k2 <- tableGrob(data.frame(category = k2), rows = NULL)


# Create a stacked bar plot with modified legend labels and y-axis encoding
plotC <- ggplot(data = df_final, 
                aes(x = as.factor(Geschlecht),
                    fill = as.factor(EinstellungDichotom)
                )
) +
  geom_bar(position = "fill") +
  labs(x = "Geschlecht (dichotom)",
       y = "Prozentualer Anteil",
       fill = "Einstellung zur EU"
  ) +
  scale_fill_manual(values = c("darkblue", "lightblue"), 
                    labels = c("Ablehnung", "Zustimmung")
  ) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Einstellung zur EU (in Prozent) nach Geschlecht")

plotC

#Grafiken zusammenfügen

library(gridExtra)

plot_geschlecht <- grid.arrange(plotC, k2, ncol = 2, widths = c(1, 0.25))

#Männer stimmen eher zu

#### GDP und Land ####

GDPLand <- unique(df_final[, c("Entity", "GDPpcapita2009")])

GDPLand


#GDPin GDP2 umbenennen
spaltennamen <- colnames(GDP2)

# Neuen Namen für die 2. Spalte festlegen
spaltennamen[2] <- "GDPpc"

# Spaltennamen im Datensatz aktualisieren
colnames(GDP2) <- spaltennamen

GDP2$GDPpc <- round(GDP2$GDPpc)


plot(GDP2$GDPpc, GDP2$Entity)

library(ggplot2)

ggplot(data = GDP2, aes(x = Entity, y = GDPpc)) +
  geom_point() +
  labs(x = "Entity", y = "GDP per Capita")

GDP2_sorted <- GDP2[order(GDP2$GDPpc), ]

ggplot(data = GDP2_sorted, aes(x = reorder(Entity, GDPpc), y = GDPpc)) +
  geom_point() +
  labs(x = "Entity", y = "GDP per Capita")

#### GDP und Zustimmung ####

#install.packages("ggrepel")


library(tidyverse)

df_gdp <- df_final


# Mittelwert der EinstellungDichotom für jedes Land berechnen
mittelwerte <- aggregate(EinstellungDichotom ~ Entity, 
                         data = df_gdp, 
                         FUN = mean)

# Neue Variable mit den Mittelwerten hinzufügen
df_gdp <- merge(df_gdp, mittelwerte, by = "Entity", all.x = TRUE)

# Umbenennen der neuen Variable
names(df_gdp)[names(df_gdp) == "EinstellungDichotom.y"] <- "MWEinstellung"

library(ggplot2)

ggplot(df_gdp, 
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


#braucht ewig
ggplot(df_gdp, 
       aes(x = GDPpcapita2009,
           y = MWEinstellung, 
           label = Entity
       )
) +
  geom_point(size = 4, color = "darkblue", alpha = 0.8) +
  geom_text_repel(
    segment.size = 0.2, force = 10,
    segment.color = "darkgray", segment.alpha = 0.6,
    hjust = 0, vjust = 0, color = "black", fontface = "bold")+
  labs(x = "BIP pro Kopf 2009 (in $)",
       y = "Einstellung zur EU (nach Mittelwerten)",
       title = "Einstellung zur EU nach BIP pro Kopf 2009")+
  theme_minimal()
