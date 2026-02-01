# ============================================
# MASTERARBEIT: Netzwerkanalyse ADNM-8
# Skript 03: Daten vorbereiten (ÜBERARBEITET)
# ============================================

library(tidyverse)
library(haven)

# Daten laden
daten <- read_sav("data/COVID_study_GE_T1-T5.sav")

# ============================================
# WICHTIG: SPSS-Labels entfernen!
# ============================================

daten <- zap_labels(daten)
cat("✓ SPSS-Labels entfernt\n\n")

# ============================================
# SCHRITT 1: Variablen definieren
# ============================================

# === ADNM-8 Variablen (8 Items) ===
adnm_vars_t1 <- paste0("ad.", 1:8, ".t1")
adnm_vars_t2 <- paste0("ad.", 1:8, ".t2")
adnm_vars_t3 <- paste0("ad.", 1:8, ".t3")
adnm_vars_t4 <- paste0("ad.", 1:8, ".t4")
adnm_vars_t5 <- paste0("ad.", 1:8, ".t5")

# === Brief-COPE Variablen (14 Strategien) ===
cope_items <- c("acc", "act", "behd", "den", "dist", "esupp",
                "hu", "isupp", "pl", "posr", "rel", "sbl", 
                "sub", "ven")

cope_vars_t1 <- paste0("bcope.", cope_items, ".t1")
cope_vars_t2 <- paste0("bcope.", cope_items, ".t2")
cope_vars_t3 <- paste0("bcope.", cope_items, ".t3")
cope_vars_t4 <- paste0("bcope.", cope_items, ".t4")
cope_vars_t5 <- paste0("bcope.", cope_items, ".t5")

# === Stressor Variablen (15 ausgewählte Items) ===
stressor_vars_t1 <- c(
  "bc.1.t1", "bc.2.t1", "bc.5.t1",
  "bh.1.t1", "bh.6.t1", "bh.8.t1",
  "bl.1.t1", "bl.7.t1",
  "bp.1.t1", "bp.3.t1",
  "bs.4.t1", "bs.5.t1",
  "bw.4.t1", "bw.5.t1", "bw.7.t1"
)

stressor_vars_t2 <- str_replace_all(stressor_vars_t1, ".t1", ".t2")
stressor_vars_t3 <- str_replace_all(stressor_vars_t1, ".t1", ".t3")
stressor_vars_t4 <- str_replace_all(stressor_vars_t1, ".t1", ".t4")
# T5: KEINE Stressor-Variablen!

# Alle Variablen pro Zeitpunkt kombinieren
all_vars_t1 <- c(adnm_vars_t1, cope_vars_t1, stressor_vars_t1)  # 37 Items
all_vars_t2 <- c(adnm_vars_t2, cope_vars_t2, stressor_vars_t2)  # 37 Items
all_vars_t3 <- c(adnm_vars_t3, cope_vars_t3, stressor_vars_t3)  # 37 Items
all_vars_t4 <- c(adnm_vars_t4, cope_vars_t4, stressor_vars_t4)  # 37 Items
all_vars_t5 <- c(adnm_vars_t5, cope_vars_t5)                    # 22 Items

# ============================================
# SCHRITT 2: Datensätze erstellen
# ============================================

cat("=== DATENSÄTZE ERSTELLEN ===\n\n")

daten_T1 <- daten %>% select(all_of(all_vars_t1))
daten_T2 <- daten %>% select(all_of(all_vars_t2))
daten_T3 <- daten %>% select(all_of(all_vars_t3))
daten_T4 <- daten %>% select(all_of(all_vars_t4))
daten_T5 <- daten %>% select(all_of(all_vars_t5))

# ============================================
# SCHRITT 3: Missing Data Analyse
# ============================================

cat("=== MISSING DATA ANALYSE ===\n\n")

analyze_missing <- function(data, timepoint) {
  cat("--- Zeitpunkt", timepoint, "---\n")
  
  missing_count <- colSums(is.na(data))
  missing_pct <- round(100 * missing_count / nrow(data), 1)
  
  if(any(missing_count > 0)) {
    missing_summary <- data.frame(
      Variable = names(missing_count)[missing_count > 0],
      N_Missing = missing_count[missing_count > 0],
      Percent = missing_pct[missing_count > 0]
    )
    print(missing_summary)
  } else {
    cat("Keine fehlenden Werte!\n")
  }
  
  complete_cases <- sum(complete.cases(data))
  complete_pct <- round(100 * complete_cases / nrow(data), 1)
  
  cat("\nVollständige Fälle:", complete_cases, 
      "(", complete_pct, "%)\n")
  cat("Unvollständige Fälle:", nrow(data) - complete_cases,
      "(", 100 - complete_pct, "%)\n\n")
  
  return(complete_cases)
}

n_complete_t1 <- analyze_missing(daten_T1, "T1")
n_complete_t2 <- analyze_missing(daten_T2, "T2")
n_complete_t3 <- analyze_missing(daten_T3, "T3")
n_complete_t4 <- analyze_missing(daten_T4, "T4")
n_complete_t5 <- analyze_missing(daten_T5, "T5")

# ============================================
# SCHRITT 4: Vollständige Datensätze
# ============================================

cat("=== VOLLSTÄNDIGE DATENSÄTZE ERSTELLEN ===\n\n")

daten_T1_complete <- na.omit(daten_T1)
daten_T2_complete <- na.omit(daten_T2)
daten_T3_complete <- na.omit(daten_T3)
daten_T4_complete <- na.omit(daten_T4)
daten_T5_complete <- na.omit(daten_T5)

cat("T1 complete (37 Items):", nrow(daten_T1_complete), "Fälle\n")
cat("T2 complete (37 Items):", nrow(daten_T2_complete), "Fälle\n")
cat("T3 complete (37 Items):", nrow(daten_T3_complete), "Fälle\n")
cat("T4 complete (37 Items):", nrow(daten_T4_complete), "Fälle\n")
cat("T5 complete (22 Items):", nrow(daten_T5_complete), "Fälle\n")

# Speichern
saveRDS(daten_T1_complete, "data/daten_T1_complete.rds")
saveRDS(daten_T2_complete, "data/daten_T2_complete.rds")
saveRDS(daten_T3_complete, "data/daten_T3_complete.rds")
saveRDS(daten_T4_complete, "data/daten_T4_complete.rds")
saveRDS(daten_T5_complete, "data/daten_T5_complete.rds")

# Variablenlisten speichern
saveRDS(list(
  adnm_t1 = adnm_vars_t1,
  cope_t1 = cope_vars_t1,
  stress_t1 = stressor_vars_t1,
  all_t1 = all_vars_t1,
  adnm_t2 = adnm_vars_t2,
  cope_t2 = cope_vars_t2,
  stress_t2 = stressor_vars_t2,
  all_t2 = all_vars_t2,
  adnm_t3 = adnm_vars_t3,
  cope_t3 = cope_vars_t3,
  stress_t3 = stressor_vars_t3,
  all_t3 = all_vars_t3,
  adnm_t4 = adnm_vars_t4,
  cope_t4 = cope_vars_t4,
  stress_t4 = stressor_vars_t4,
  all_t4 = all_vars_t4,
  adnm_t5 = adnm_vars_t5,
  cope_t5 = cope_vars_t5,
  all_t5 = all_vars_t5
), "data/variable_lists.rds")

cat("\n✓ Alle Datensätze gespeichert!\n")