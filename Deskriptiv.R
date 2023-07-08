#### Deskriptive Statistik ####

#### TO DO ####

#Legenden

#### AV: Zustimmung ####

#Absolute Häufigkeiten

abs_freq <- table(df_final$EinstellungDichotom)

#0: 10.716
#1: 11518

#Prozentsätze

percentages <- round(100 * abs_freq / sum(abs_freq))


#barplot in Prozent

barplot(table(percentages), 
        col = "lightblue", 
        main = "Verteilung der Einstellungen zur EU",
        xlab = "Ablehnung (0) oder Zustimmung (1) zur EU", 
        ylab = "Prozentuale Anteil",
        names.arg = c("0", "1") #ohne Prozent angezeigt 48% zu 52%
)

legend("bottom",
       c("0: Ablehnung ",
         "1: Zustimmung"),
       pch = 15, 
       cex = 0.5
)

?legend

#### UV: Bildungsjahre ####

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
           "10: still studying",
           "11: no-fulltime education"),
       pch = 15, 
       cex = 0.4
)



#### Zustimmung Abhängigkeit kategorisiertem Alter ####

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

####Zustimmung in Abhängigkeit zur Bildung (kategorisiert) ####

#UV: Bildung, AV: Einstellung

## Erstellung der Kreuztabelle mit Absolutwerten
cross_table <- table(df_final$EinstellungDichotom, df_final$BJ_gruppiert)
cross_table

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert),margin = 2) * 100
cross_table2

# Umwandeln der Kreuztabelle2 in einen Datenframe
cross_table2_df <- as.data.frame(cross_table2)


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


k1 <- c("1: bis zu 14 Bildungsjahren",
                     "2: 15 Bildungsjahre",
                     "3: 16 Bildungsjahre",
                     "4: 17 Bildungsjahre",
                     "5: 18 Bildungsjahre",
                     "6: 19 Bildungsjahre",
                     "7: 20 Bildungsjahre",
                     "8: 21 Bildungsjahre",
                     "9: 22 Bildungsjahre und mehr",
                     "10: still studying")

text_plot2 <- tableGrob(data.frame(category = k1), rows = NULL)



plot_BJ <- grid.arrange(plotB, text_plot2, ncol = 2, widths = c(0.6, 0.25))

#Zustimmung steigt mit zunehmenden BJ

#### Zustimmung in Abhängigkeit zum Geschlecht ####

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

plot_geschlecht <- grid.arrange(plotC, k2, ncol = 2, widths = c(0.7, 0.25))

#Männer stimmen eher zu


