library(psych)
library(haven)
DatenMEA <- read_sav("DatenMEA.sav")

#### Benefit und Zustimmung mit Gewichtung ####

weighted_spearman_cor <- cor.test(
  EuBaDaten$Benefit_recoded,
  EuBaDaten$EinstellungDichotom,
  method = "spearman",
  alternative = "two.sided", 
  weights = EuBaDaten$Land_Gewichtung
)

print(weighted_spearman_cor)

#cronbachs alpha

alpha_result <- alpha(EuBaDaten[, c("Benefit_recoded", "EinstellungDichotom")], 
                      check.keys = TRUE)  # Prüfen, ob alle Werte ordinales Format haben
# Cronbachs Alpha ausgeben
print(alpha_result$total$raw_alpha)


#### 3 gliedrig geht nicht


weighted_spearman_cor3 <- cor.test(
  EuBaDaten$Benefit_recoded,
  EuBaDaten$Einstellung3,
  method = "spearman",
  alternative = "two.sided", 
  weights = EuBaDaten$Land_Gewichtung
)

print(weighted_spearman_cor3)

