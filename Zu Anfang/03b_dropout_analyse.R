# ============================================
# MASTERARBEIT: Dropout-Analyse
# ============================================

library(tidyverse)
library(haven)
library(psych)

# Originaldaten laden
daten <- read_sav("data/COVID_study_GE_T1-T5.sav")

# ============================================
# DROPOUT DEFINIEREN
# ============================================

# Wer hat T1 vollständig ausgefüllt?
daten$has_T1 <- rowSums(!is.na(daten[, paste0("ad.", 1:8, ".t1")])) == 8

# Wer hat T2-T5 teilgenommen? (mind. 1 ADNM-Item)
daten$has_T2 <- rowSums(!is.na(daten[, paste0("ad.", 1:8, ".t2")])) > 0
daten$has_T3 <- rowSums(!is.na(daten[, paste0("ad.", 1:8, ".t3")])) > 0
daten$has_T4 <- rowSums(!is.na(daten[, paste0("ad.", 1:8, ".t4")])) > 0
daten$has_T5 <- rowSums(!is.na(daten[, paste0("ad.", 1:8, ".t5")])) > 0

# Dropout-Gruppen definieren
daten <- daten %>%
  filter(has_T1 == TRUE) %>%  # Nur Personen mit T1-Daten
  mutate(
    dropout_after_T1 = !has_T2,
    completer_T2 = has_T2,
    completer_T3 = has_T3,
    completer_T4 = has_T4,
    completer_T5 = has_T5
  )

cat("=== DROPOUT ÜBERSICHT ===\n\n")
cat("Personen mit T1:", nrow(daten), "\n")
cat("Davon bei T2:", sum(daten$has_T2), 
    "(", round(100*mean(daten$has_T2), 1), "%)\n")
cat("Davon bei T3:", sum(daten$has_T3), 
    "(", round(100*mean(daten$has_T3), 1), "%)\n")
cat("Davon bei T4:", sum(daten$has_T4), 
    "(", round(100*mean(daten$has_T4), 1), "%)\n")
cat("Davon bei T5:", sum(daten$has_T5), 
    "(", round(100*mean(daten$has_T5), 1), "%)\n\n")

# ============================================
# VERGLEICH: COMPLETER vs. DROPOUT
# ============================================

# Basierend auf T4 (da du bis T4 Stressoren hast)
# Vergleiche auf T1-Variablen

cat("=== VERGLEICH: T4-COMPLETER vs. DROPOUT ===\n\n")

# Soziodemografie (wenn vorhanden - passe Variablennamen an!)
# Beispiel - ersetze mit deinen echten Variablennamen:
# if("Alter" %in% names(daten)) {
#   cat("--- Alter ---\n")
#   print(describeBy(daten$Alter, daten$completer_T4, mat = TRUE))
#   print(t.test(Alter ~ completer_T4, data = daten))
# }

# Alter zu T1
if("s.age.t1" %in% names(daten)) {
  cat("--- Alter ---\n")
  print(describeBy(daten$s.age.t1, daten$completer_T4, mat = TRUE))
  print(t.test(s.age.t1 ~ completer_T4, data = daten))
}

# ADNM zu T1
cat("--- ADNM Total Score T1 ---\n")
if("adnm.t1" %in% names(daten)) {
  desc_dropout <- describeBy(daten$adnm.t1, 
                             daten$completer_T4, 
                             mat = TRUE, 
                             skew = FALSE)
  print(desc_dropout[, c("group1", "n", "mean", "sd")])
  
  test_result <- t.test(adnm.t1 ~ completer_T4, data = daten)
  cat("\nT-Test:\n")
  cat("t =", round(test_result$statistic, 2), 
      ", p =", round(test_result$p.value, 3), "\n")
  
  if(test_result$p.value < 0.05) {
    cat("⚠️ SIGNIFIKANTER Unterschied!\n")
  } else {
    cat("✓ KEIN signifikanter Unterschied (gut für MCAR-Annahme)\n")
  }
}

cat("\n--- Brief-COPE Total Score T1 ---\n")
if("bcope.t1" %in% names(daten)) {
  desc_cope <- describeBy(daten$bcope.t1, 
                          daten$completer_T4, 
                          mat = TRUE, 
                          skew = FALSE)
  print(desc_cope[, c("group1", "n", "mean", "sd")])
  
  test_cope <- t.test(bcope.t1 ~ completer_T4, data = daten)
  cat("\nT-Test:\n")
  cat("t =", round(test_cope$statistic, 2), 
      ", p =", round(test_cope$p.value, 3), "\n")
  
  if(test_cope$p.value < 0.05) {
    cat("⚠️ SIGNIFIKANTER Unterschied!\n")
  } else {
    cat("✓ KEIN signifikanter Unterschied\n")
  }
}

# Geschlecht (falls vorhanden - passe Variablenname an!)
# if("Geschlecht" %in% names(daten)) {
#   cat("\n--- Geschlecht ---\n")
#   tab <- table(daten$Geschlecht, daten$completer_T4)
#   print(tab)
#   chi_test <- chisq.test(tab)
#   cat("Chi-Quadrat Test: p =", round(chi_test$p.value, 3), "\n")
# }

#Geschlecht zu T1
if("s.gen.t1" %in% names(daten)) {
     cat("\n--- Geschlecht ---\n")
     tab <- table(daten$s.gen.t1, daten$completer_T4)
     print(tab)
     chi_test <- chisq.test(tab)
     cat("Chi-Quadrat Test: p =", round(chi_test$p.value, 3), "\n")
   }

# ============================================
# ZUSAMMENFASSUNG
# ============================================

cat("\n\n=== ZUSAMMENFASSUNG DROPOUT-ANALYSE ===\n")
cat("Wenn p-Werte > 0.05: Dropout wahrscheinlich MCAR (Missing Completely At Random)\n")
cat("Wenn p-Werte < 0.05: Systematischer Dropout → im Methodenteil erwähnen!\n")

# Ergebnisse speichern
dropout_summary <- data.frame(
  Zeitpunkt = c("T2", "T3", "T4", "T5"),
  N_Teilnehmer = c(sum(daten$has_T2), sum(daten$has_T3), 
                   sum(daten$has_T4), sum(daten$has_T5)),
  Prozent = round(100 * c(mean(daten$has_T2), mean(daten$has_T3),
                          mean(daten$has_T4), mean(daten$has_T5)), 1)
)

write.csv(dropout_summary, "results/dropout_summary.csv", row.names = FALSE)
cat("\n✓ Dropout-Tabelle gespeichert: results/dropout_summary.csv\n")