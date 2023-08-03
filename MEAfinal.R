#Mehrebenenanalyse zum Einfluss des Bildungsniveaus auf Einstellung zur EU
##Daten: Eurobarometer 2010

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


#Daten einlesen
EuBa <- read_sav("DatensatzUKundGERmerge.sav") #Eurobarometer
gdp <- read_csv("gdp-per-capita-inflation--and-ppp-adjusted-world-bank-data-vs-penn-world-table-data.csv") #GDP

#Datensätze reinigen
##EB10
###Variablenauswahl und Umbenennung
EuBa_Vars <- select(EuBa,
                    Land = NATION,
                    Gewichtung_Land = v40,
                    Einstellung_EU = v206,
                    Bildungsjahre = v553,
                    Bildungsjahre_kategorisiert = v554,
                    Geschlecht = v555,
                    Alter = v556,
                    Alter_gruppiert = v557
)

###NAs löschen
EuBaDaten <- na.omit(EuBa_Vars)

### Geschlecht recoden
EuBaDaten$Geschlecht_recoded <- NA
EuBaDaten$Geschlecht_recoded [EuBaDaten$Geschlecht == 1] <- 1 #male
EuBaDaten$Geschlecht_recoded [EuBaDaten$Geschlecht == 2] <- 0 #female

#Einstellung zur EU recoden (dichotomisieren: good=1, bad&neither good nor bad=0, dk=löschen
EuBaDaten$EinstellungDichotom <- NA

## Umkodierung der Antwortkategorien und Zuordnung zu "EinstellungDichotom"
EuBaDaten$EinstellungDichotom[EuBaDaten$Einstellung_EU == 1] <- 1          #gut
EuBaDaten$EinstellungDichotom[EuBaDaten$Einstellung_EU %in% c(2, 3)] <- 0  #schlecht, weder noch
EuBaDaten <- EuBaDaten[!EuBaDaten$Einstellung_EU %in% c(4, 9), ]         # Löschen der Fälle mit den Antworten 4 (dont know) und 9(inap)


## Einstellung in 3 Kategorien 
#neue Variable
EuBaDaten$Einstellung3 <- NA
## Umkodierung und Zuordnung 
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 3] <- 1 #wedernoch
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 1] <- 0 #good
EuBaDaten$Einstellung3[EuBaDaten$Einstellung_EU == 2] <- 2 #bad
## Überprüfung der neuen Variable
table(EuBaDaten$Einstellung3)

##Bildung neu kodieren ##
#neue Variable um Bildung in primär, senkundär, tertiär und noch im 
#Bildungssystem zu Kategorisieren
EuBaDaten$BJ_gruppiert <- NA

# Löschen der Fälle mit den Antworten 97 und 98
EuBaDaten <- EuBaDaten[!(EuBaDaten$Bildungsjahre_kategorisiert %in% c(97, 98)), ]
# Umkodierung von 0-15 BJ zu 0
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 1] <- 0 #bis 14
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 2] <- 0 #15
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 11] <- 0 #nofulltime
# Umkodierung von 16-19 BJ zu 1
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert >= 3 & EuBaDaten$Bildungsjahre_kategorisiert <= 6] <- 1 
# Umkodierung über 20 BJ zu 2
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert >= 7 & EuBaDaten$Bildungsjahre_kategorisiert <= 9] <- 2 
#Umkodierung noch im Bildungssystem zu 4
EuBaDaten$BJ_gruppiert[EuBaDaten$Bildungsjahre_kategorisiert == 10] <- 4
# Überprüfung der neuen Variable
table(EuBaDaten$BJ_gruppiert)


#Bildung gruppiert neu kodiert: falsch, weil ordinal angenommen & noch in Studium als höchstes angenommen
EuBaDaten$Bildungsjahre_recoded <- EuBaDaten$Bildungsjahre_kategorisiert     # Umkodierung der Antwortkategorien
EuBaDaten <- EuBaDaten[!(EuBaDaten$Bildungsjahre_recoded %in% c(97, 98)), ]# Löschen der Fälle mit den Antworten 97 und 98
EuBaDaten$Bildungsjahre_recoded[EuBaDaten$Bildungsjahre_recoded == 11] <- 0  # Umkodierung der Antwortkategorie 11 (no full time) zu 0

###Ländernamen zum mergen
EuBaDaten$Entity <- NA
EuBaDaten$Entity[EuBaDaten$Land == 1] <- "France"
EuBaDaten$Entity[EuBaDaten$Land == 2] <- "Belgium"
EuBaDaten$Entity[EuBaDaten$Land == 3] <- "Netherlands"
EuBaDaten$Entity[EuBaDaten$Land == 4] <- "Germany"
EuBaDaten$Entity[EuBaDaten$Land == 5] <- "Italy"
EuBaDaten$Entity[EuBaDaten$Land == 6] <- "Luxembourg"
EuBaDaten$Entity[EuBaDaten$Land == 7] <- "Denmark"
EuBaDaten$Entity[EuBaDaten$Land == 8] <- "Ireland"
EuBaDaten$Entity[EuBaDaten$Land == 9] <- "United Kingdom"
EuBaDaten$Entity[EuBaDaten$Land == 11] <- "Greece"
EuBaDaten$Entity[EuBaDaten$Land == 12] <- "Spain"
EuBaDaten$Entity[EuBaDaten$Land == 13] <- "Portugal"
EuBaDaten$Entity[EuBaDaten$Land == 16] <- "Finland"
EuBaDaten$Entity[EuBaDaten$Land == 17] <- "Sweden"
EuBaDaten$Entity[EuBaDaten$Land == 18] <- "Austria"
EuBaDaten$Entity[EuBaDaten$Land == 19] <- "Cyprus"
EuBaDaten$Entity[EuBaDaten$Land == 20] <- "Czech Republic"
EuBaDaten$Entity[EuBaDaten$Land == 21] <- "Estonia"
EuBaDaten$Entity[EuBaDaten$Land == 22] <- "Hungary"
EuBaDaten$Entity[EuBaDaten$Land == 23] <- "Latvia"
EuBaDaten$Entity[EuBaDaten$Land == 24] <- "Lithuania"
EuBaDaten$Entity[EuBaDaten$Land == 25] <- "Malta"
EuBaDaten$Entity[EuBaDaten$Land == 26] <- "Poland"
EuBaDaten$Entity[EuBaDaten$Land == 27] <- "Slovakia"
EuBaDaten$Entity[EuBaDaten$Land == 28] <- "Slovenia"
EuBaDaten$Entity[EuBaDaten$Land == 29] <- "Bulgaria"
EuBaDaten$Entity[EuBaDaten$Land == 30] <- "Romania"
EuBaDaten$Entity[EuBaDaten$Land == 32] <- "Croatia"

##############################GDP logaritmiert in die MEA aufnehmen?????????? Quadriert plus normal?????
##GDP Datensatz   
GDP <- filter(gdp, Year == 2009)  #2009 rausfiltern 
#Länder filtern
eu_laender <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
                "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland",
                "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "United Kingdom")

GDP2 <- subset(GDP, Entity %in% eu_laender)

#Continent, Year, Population, Code löschen
GDP2$Continent <- NULL
GDP2$Year <- NULL
GDP2$Code <- NULL
colnames(GDP2)[4] <- "Population"
GDP2$Population <- NULL

#Finalen Datensatz erstellen
DatensatzGesamt <- merge(EuBaDaten, GDP2, by = "Entity")                     #Datensätze zusammenfügen
DatensatzGesamt$Bildungsjahre <- NULL                                        #Bildungsjahre Alter
DatensatzGesamt$Geschlecht <- NULL                                           #Geschlecht alte Kodierung
DatensatzGesamt$`GDP per capita (output, multiple price benchmarks)` <- NULL #GDP unnötig
colnames(DatensatzGesamt)[11] <- "GDPpcapita2009"                            #Umbenennung Spalte

# Neue quadrierte BIPVariable hinzufügen
DatensatzGesamt <- DatensatzGesamt %>% 
  mutate(bip_squared = GDPpcapita2009^2)  ##quadriertes BIP nicht signifikant, daher mit normalem weitergerechnet!


EUeintritt_table <- data.frame(
   Land = eu_laender,  # Ländernamen in der gleichen Reihenfolge wie in deinem Datensatz
   Mitgliedsdauer = c(1995, 1958, 2007, 2013, 2004, 2004, 1973, 2004, 1995, 1958, 1958, 1981, 2004, 1973, 1958, 2004, 2004, 1958, 2004, 1958, 2004, 1986, 2007, 2004, 2004, 1986, 1995, 1973))
 

DatensatzGesamt$Eintritt_in_EU <- EUeintritt_table$Mitgliedsdauer[match(DatensatzGesamt$Entity, EUeintritt_table$Land)]  
   
DatensatzGesamt$Mitgliedsdauer <- 2010 - DatensatzGesamt$Eintritt_in_EU
   
###Mehrebenenanalyse

#Preliminary phase: Preparing the data (centering variables)
#Step #1: Building an empty model, so as to assess the variation of the log-odds from one cluster to another
#Step #2: Building an intermediate model, so as to assess the variation of the lower-level effect(s) from one cluster to another
#Step #3: Building a final model, so as to test the hypothesis(/-es)

#Preliminary phase
###Daten zentrieren:
# Center variables am Gruppenmittelwert
DatensatzGesamt$BJ_gruppiert_centered <- center(DatensatzGesamt$Bildungsjahre_recoded, type="CWC", cluster=df_final$Entity)
#am Gesamtmittelwert:
DatensatzGesamt$BJ_gruppiert_centeredGrandMean <- center(DatensatzGesamt$Bildungsjahre_recoded, type = "CGM")
#df_final$Geschlecht_scaled <- scale(df_final$Geschlecht)                                                                ## Geschlecht nicht zentrieren weil eh dichotom!
#DatensatzGesamt$AlterGruppiert_centered <- center(DatensatzGesamt$Alter_gruppiert, type="CWC", cluster=df_final$Entity) ## Zentrierung sinvoll????
#DatensatzGesamt$Alter_centered <- center(DatensatzGesamt$Alter, type="CWC", cluster=df_final$Entity)                    ## wir arbeiten mit Alter gruppiert
#df_final$GDPpcapita2009_centered <- center(df_final$GDPpcapita2009, type="CWC", cluster=df_final$Entity)                ## unnötig da schon zentriert


#Step 1: Leermodell (model containing no predictors)
# calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).

####Leermodell mit weights-Gewichtung auf DatensatzGesamt Einstellung 2-geteilt
M02a <- glmer(EinstellungDichotom ~ ( 1 | Entity),
              data=DatensatzGesamt,
              family = "binomial",
              weights = Gewichtung_Land)
summary(M02a)

iccM02a <- M02a@theta[1]^2/ (M02a@theta[1]^2 + (3.14159^2/3))
iccM02a #Ergebnis: 0.054045
# bei gewichteten Ländern: 0.0409

icc_mit_PaketM02a <-icc(M02a) #Adjusted ICC: 0.054
icc_mit_PaketM02a 

####Leermodell ohne Gewichtung auf DatensatzGesamt Einstellung 2-geteilt
M02b <- glmer(EinstellungDichotom ~ ( 1 | Entity),
              data=DatensatzGesamt,
              family = "binomial",
)
summary(M02b)

iccM02b <- M02b@theta[1]^2/ (M02b@theta[1]^2 + (3.14159^2/3))
iccM02b #Ergebnis: 0.07482931

icc_mit_PaketM02b <-icc(M02b) #Adjusted ICC: 0.075
icc_mit_PaketM02b


####Leermodell mit weights-Gewichtung auf DatensatzGesamt Einstellung 3-geteilt
M03a <- lmer(Einstellung3 ~ 1 + ( 1 | Entity),
              data=DatensatzGesamt,
              weights = Gewichtung_Land)
summary(M03a)

#ICC
icc_mit_PaketM03a <-icc(M03a) #Adjusted ICC: 0.046
icc_mit_PaketM03a 


####Leermodell ohne Gewichtung auf DatensatzGesamt Einstellung 3-geteilt
M03b <- lmer(Einstellung3 ~ 1 + ( 1 | Entity),
              data=DatensatzGesamt)
summary(M03b)

#ICC
icc_mit_PaketM03b <-icc(M03b) #Adjusted ICC: 0.054
icc_mit_PaketM03b

#If you focus on the between-observation effect of the (level-1) variable, you can use the grand-mean centered variable ("gpa_gmc"). 
#If you focus on the within-cluster effect, use the cluster-mean centered variable ("gpa_cmc"). 
#We use the cluster-mean centered variable herein.

# Below are the commands to run the constrained intermediate model (CIM); 
# the model contains all level-1 variables, all level-2 variables well as all intra-level interactions).

CIM <- glmer(EinstellungDichotom ~ BJ_gruppiert_centeredGrandMean + Alter_gruppiert + Geschlecht_recoded + GDPpcapita2009 + (1 | Entity),
             data = DatensatzGesamt,
             family = "binomial",
             weights = Gewichtung_Land)
summary(CIM)

#mit gruppeninterner Zentrierung
CIM_cmc <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered + Alter_gruppiert + Geschlecht_recoded + GDPpcapita2009 + (1 | Entity),
                 data = DatensatzGesamt,
                 family = "binomial",
                 weights = Gewichtung_Land)
summary(CIM_cmc)

# augmented intermediate model (AIM); inkl random slope term von Bildung
#To estimate a random slope effect in lme4, you place the predictor for which you want a random slope before the |
AIM <- glmer(EinstellungDichotom ~ BJ_gruppiert_centeredGrandMean + Alter_gruppiert + Geschlecht_recoded + GDPpcapita2009 + (1 + BJ_gruppiert_centeredGrandMean || Entity),
             data = DatensatzGesamt,
             family = "binomial",
             weights = Gewichtung_Land)
summary(AIM)

#mit gruppeninterner Zentrierung
AIM_cmc <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered + Alter_gruppiert + Geschlecht_recoded + GDPpcapita2009 + (1 + BJ_gruppiert_centered || Entity),
                 data = DatensatzGesamt,
                 family = "binomial",
                 weights = Gewichtung_Land)
summary(AIM_cmc)

#Random Effects, Random Slopes mit allen Level1-Variablen
AIM_cmc2 <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered 
                  + Alter_gruppiert 
                  + Geschlecht_recoded 
                  + GDPpcapita2009 
                  + (1 + BJ_gruppiert_centered + Geschlecht_recoded + Alter_gruppiert|| Entity),
                  data = DatensatzGesamt,
                  family = "binomial",
                  weights = Gewichtung_Land)
summary(AIM_cmc2)  #Varianzen der Random Effects sehr gering! --> könnten auch weggelassen werden
anova (AIM_cmc, AIM_cmc2)
#Testen ob Miteinbeziehung von random slope der Bildung das Modell verbessert 
#likelihood-ratio test LR X(1)²,  Vergleich der deviance bei CIM und AIM
anova(CIM, AIM)
anova(CIM_cmc, AIM_cmc)

#final model (inklusive cross-level interaction) 
FM <- glmer(EinstellungDichotom ~ BJ_gruppiert_centeredGrandMean 
            + Alter_gruppiert 
            + Geschlecht_recoded 
            + GDPpcapita2009 
            + BJ_gruppiert_centeredGrandMean:GDPpcapita2009
            + (1 + BJ_gruppiert_centeredGrandMean || Entity),
            data = DatensatzGesamt,
            family = "binomial",
            weights = Gewichtung_Land)
summary(FM)

crosslevel <- glmer(EinstellungDichotom ~ BJ_gruppiert_centered
                    + Alter_gruppiert 
                    + Geschlecht_recoded 
                    + GDPpcapita2009
                    + BJ_gruppiert_centered:GDPpcapita2009
                    + (1 + BJ_gruppiert_centered + Geschlecht_recoded + Alter_gruppiert|| Entity),
                    data = DatensatzGesamt,
                    family = "binomial",
                    weights = Gewichtung_Land)
summary(crosslevel)

## Vergleich von Mehrebenen (glmer) zu nicht (glm)
GLM <-  glm(EinstellungDichotom ~ BJ_gruppiert_centered + Alter_gruppiert + Geschlecht_recoded + GDPpcapita2009 + BJ_gruppiert_centered:GDPpcapita2009, data = DatensatzGesamt, family = "binomial")
summary (GLM)

##########################################################
#Calculate Odds-Ratios
#OR <- exp(fixef(AIM_cmc))
#CI <- exp(confint(AIM_cmc,parm="beta_"))
#################################################
#data(DatensatzGesamt) 
s1 <- glmer(EinstellungDichotom ~ Geschlecht_recoded + Alter_gruppiert + GDPpcapita2009 + (1|Entity), family = binomial,
            data = DatensatzGesamt)
s2 <- update(s1, . ~ . + BJ_gruppiert_centered)
##########################################################

###Output: https://francish.net/mlmusingr/MLM_Appendix_A.pdf
cm <- c('BJ_gruppiert_centered'    = 'Bildungsjahre gruppiert',
        'Geschlecht_recoded'    = 'Geschlecht',
        'Alter_gruppiert' = 'Alter',
        'GDPpcapita2009' = 'GDP von 2009',
        '(Intercept)' = 'Constant',
        'SD (BJ_gruppiert_centered Entity)' = 'SD Bildungsjahre gruppiert, Land'
)
modelsummary(list(s1, s2, CIM_cmc, AIM_cmc, AIM_cmc2),
             coef_rename = cm,
             statistic = "({conf.low}, {conf.high})",
             exponentiate = TRUE, stars = TRUE,
             coef_omit = "Intercept",
             title = 'Multilevel Logistic Regression Model Results Predicting Suspensions Using Odds Ratios (ORs)',
             dep.var.labels = c("Einstellung gegenüber der EU")
)

modelsummary(M0)


################################
#Mit Skalierten, nach Land gewichteten, am Gruppenmittelwert zentrierten Werten:
# Rescale variables - z-Transformation
DatensatzGesamt$BJsc <- scale(DatensatzGesamt$BJ_gruppiert_centered)
DatensatzGesamt$Altsc <- scale(DatensatzGesamt$Alter_gruppiert)
DatensatzGesamt$GDPsc <- scale(DatensatzGesamt$GDPpcapita2009)

#Nullmodell(bleibt gleich)
M0 <- glmer(EinstellungDichotom ~ ( 1 | Entity),
            data=DatensatzGesamt,
            family = "binomial",
            weights = Gewichtung_Land)
summary(M0)
#ICC
iccM0 <-icc(M0)
iccM0

#Fixed Slope
fixedslope <- glmer(EinstellungDichotom ~ BJsc 
                    + Altsc 
                    + Geschlecht_recoded 
                    + GDPsc + (1 | Entity),
                    data = DatensatzGesamt,
                    family = "binomial",
                    weights = Gewichtung_Land)
summary(fixedslope)

#Random Slope für alle UV der Ebene 1
randomslope <- glmer(EinstellungDichotom ~ BJsc 
                     + Altsc 
                     + Geschlecht_recoded 
                     + GDPsc 
                     + (1 + BJsc + Geschlecht_recoded + Altsc|| Entity),
                     data = DatensatzGesamt,
                     family = "binomial",
                     weights = Gewichtung_Land)
summary(randomslope)

#Vergleich der Devianzen
anova(fixedslope, randomslope) #24180-24118=62, signifikant (Miteinbezug des Residualterms sinnvoll!)

#Plus Cross-Level-Interaction
crosslevelsc <- glmer(EinstellungDichotom ~ BJsc
                      + Altsc 
                      + Geschlecht_recoded 
                      + GDPsc
                      + BJsc:GDPsc
                      + (1 + BJsc + Geschlecht_recoded + Altsc|| Entity),
                      data = DatensatzGesamt,
                      family = "binomial",
                      weights = Gewichtung_Land)
summary(crosslevelsc)


###Output: https://francish.net/mlmusingr/MLM_Appendix_A.pdf
coefmap <- c('BJsc'    = 'Bildungsjahre gruppiert',
             'Geschlecht_recoded'    = 'Geschlecht',
             'Altsc' = 'Alter',
             'GDPsc' = 'GDP von 2009',
             '(Intercept)' = 'Constant',
             'SD (BJsc Entity)' = 'SD Bildungsjahre gruppiert, Land',
             'SD (Geschlecht_recoded Entity)' = 'SD Geschlecht, Land',
             'SD (Altsc Entity)' = 'SD Alter, Land',
             'BJsc:GDPsc' = 'Cross-Level-Interaktion Bildungsjahre, GDP'
)
modelsummary(list(fixedslope, randomslope, crosslevelsc),
             coef_rename = coefmap,
             statistic = "({conf.low}, {conf.high})",
             exponentiate = TRUE, stars = TRUE,
             coef_omit = "Intercept",
             title = 'Multilevel Logistic Regression Model Results Predicting Suspensions Using Odds Ratios (ORs)'
)
