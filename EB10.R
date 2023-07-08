library(haven)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

#### To Do ####

#Interpretation??



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
               v556,
               v557
               ) 

#Umbenennen der Variablen

df_1 <- select(df_1,
               Land = v6, 
               LandGewichtung = v40,
               Zustimmung_EU = v206, 
               BJ_recoded = v554,
               Gender = v555,
               Alter = v556,
               AlterGruppiert = v557
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

colnames(df_final)[13] <- "egal"

df_final$egal <- NULL

colnames(df_final)[12] <- "GDPpcapita2009"

#### Deskriptive Statistik #####

#UV: Bildung, AV: Einstellung

## Erstellung der Kreuztabelle mit Absolutwerten
cross_table <- table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert)
cross_table

#Erstellung der Kreuztabelle2 mit Prozentwerten
cross_table2 <- prop.table(table(clean_data$EinstellungDichotom, clean_data$BJ_gruppiert), margin = 2) * 100
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
       fill = "Einstellung zur EU") +
  scale_fill_manual(values = c("darkblue", "lightblue"), labels = c("Ablehnung", "Zustimmung")) +
  theme_gray()

# Create a stacked bar plot with modified legend labels and y-axis encoding
ggplot(data = df_final, aes(x = as.factor(BJ_gruppiert), fill = as.factor(EinstellungDichotom))) +
  geom_bar(position = "fill") +
  labs(x = "Bildungsjahre kategorisiert", y = "Prozentualer Anteil", fill = "Einstellung zur EU") +
  scale_fill_manual(values = c("darkblue", "lightblue"), labels = c("Ablehnung", "Zustimmung")) +
  theme_gray() +
  scale_y_continuous(labels = scales::percent_format())



#### Mehrebenen Logit ####
#install.packages(Matrix)
library(lme4)



##Nullmodell mit allen Kontrollvariablen, ohne UV (Lengfeld) H
# Fit the null model
null_model <- glmer(EinstellungDichotom ~ Geschlecht + AlterGruppiert + GDPpcapita2009 + (1 | Entity), 
                    data = df_final, 
                    family = binomial(link = "logit"))

#gewichtetes NUllmodell
null_model_weighted <- glmer(EinstellungDichotom ~ Geschlecht + AlterGruppiert + GDPpcapita2009 + (1 | Entity), 
                    data = df_final, 
                    family = binomial(link = "logit"),
                    weights = LandGewichtung)

# View the summary of the null model
summary(null_model)
summary(null_model_weighted)


##Logistische Mehrebenenanalyse H
library(lme4)

# Fit the logistic multilevel model
fit <- glmer(EinstellungDichotom ~ BJ_gruppiert + Geschlecht + AlterGruppiert + GDPpcapita2009 + (1 | Entity), 
             data = df_final, 
             family = binomial(link = "logit"))

# View the summary of the model
summary(fit)



###Nach Warnung:
#Warning messages:
#1: Some predictor variables are on very different scales: consider rescaling 
#2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.0590804 (tol = 0.002, component 1)
#3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model is nearly unidentifiable: very large eigenvalue
#- Rescale variables?;Model is nearly unidentifiable: large eigenvalue ratio
#- Rescale variables?


# Center variables am Gruppenmittelwert
#install.packages("misty")
library(misty)

df_final$BJ_gruppiert_centered <- center(df_final$BJ_gruppiert, type="CWC", cluster=df_final$Entity)
#df_final$Geschlecht_scaled <- scale(df_final$Geschlecht)  Geschlecht nicht zentrieren weil eh dichotom!
df_final$AlterGruppiert_centered <- center(df_final$AlterGruppiert, type="CWC", cluster=df_final$Entity)
df_final$Alter_centered <- center(df_final$Alter, type="CWC", cluster=df_final$Entity)
#df_final$LandGewichtung_centered <- center(df_final$LandGewichtung, type="CWC")
#df_final$GDPpcapita2009_centered <- center(df_final$GDPpcapita2009, type="CWC", cluster=df_final$Entity)

# Fit the logistic multilevel model with centered variables
fit_centered <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered + Geschlecht + AlterGruppiert_centered + GDPpcapita2009 + (1 | Entity), 
                      data = df_final, 
                      family = binomial(link = "logit"))
#gewichtet
fit_centered_weighted <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered + Geschlecht + AlterGruppiert_centered + GDPpcapita2009 + (1 | Entity), 
                      data = df_final, 
                      family = binomial(link = "logit"),
                      weights = LandGewichtung)

# View the summary of the model with centered variables
summary(fit_centered)   #AIC 28747.4  BIC 28795.4
summary(fit_centered_weighted) # AIC 24187.4  BIC 24235.4

# Rescale variables - z-Transformation
df_final$BJ_gruppiert_scaled <- scale(df_final$BJ_gruppiert)
#df_final$Geschlecht_scaled <- scale(df_final$Geschlecht)  Geschlecht nicht zentrieren weil eh dichotom!
df_final$Alter_scaled <- scale(df_final$Alter)
df_final$AlterGruppiert_scaled <- scale(df_final$AlterGruppiert)
#df_final$LandGewichtung_scaled <- scale(df_final$LandGewichtung)
df_final$GDPpcapita2009_scaled <- scale(df_final$GDPpcapita2009)

# Fit the logistic multilevel model with rescaled variables
fit_rescaled <- glmer(EinstellungDichotom ~ BJ_gruppiert_scaled + Geschlecht + AlterGruppiert_scaled + GDPpcapita2009_scaled + (1 | Entity), 
                      data = df_final, 
                      family = binomial(link = "logit"))

# Fit the logistic multilevel model with rescaled variables gewichtet
fit_rescaled_weighted <- glmer(EinstellungDichotom ~ BJ_gruppiert_scaled + Geschlecht + AlterGruppiert_scaled + GDPpcapita2009_scaled + (1 | Entity), 
                      data = df_final, 
                      family = binomial(link = "logit"),
                      weights = LandGewichtung)

# View the summary of the model with rescaled variables
summary(fit_rescaled)          #AIC 28746.4  BIC 28794.5
summary(fit_rescaled_weighted) #AIC 24187.0  BIC 24235.0

#ICC-Koeffizient berechnen, um Cluster-Effekt zu testen --> ob Mehrebenenanalyse geeignet ist
#install.packages("sjstats")
#library(sjstats)
library(performance)

# Calculate the ICC
icc <- icc(fit)

# View the ICC
icc

# Calculate the ICC
icc_rescaled <- icc(fit_rescaled)

# View the ICC
icc_rescaled

# Calculate the ICC
icc_rescaled_weighted <- icc(fit_rescaled_weighted)

# View the ICC
icc_rescaled_weighted
###Output
#Adjusted ICC: 0.064
#Unadjusted ICC: 0.059      --> schlecht! müsste >0.1 sein, um MEA geeignet zu sein


###Output stargazer

library(modelsummary)
library(lmerTest)
library(MLMusingR)
data(engage) #in the MLMusingR package

#Koeffizienten-Map erzeugen um Variablennamen umzulabeln für sauberen Output

cm <- c('BJ_gruppiert_scaled'    = 'Bildungsjahre gruppiert',
        'Geschlecht_scaled'    = 'Geschlecht',
        'Alter_scaled' = 'Alter',
        'GDPpcapita2009_scaled' = 'GDP von 2009',
        '(Intercept)' = 'Constant')

#Modellzusammenfassung und speichern als docx
modelsummary(fit_rescaled, coef_rename = cm, stars = TRUE, output = "tabelle.docx",
             title = 'Multilevel Regression Model Ergebnisse Einstellungen zur EU.')
