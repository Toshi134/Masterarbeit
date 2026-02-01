# ============================================================================
# Skript: Deskriptive Statistik für Complete Cases über 5 Messzeitpunkte
# ============================================================================

# Pakete laden
library(haven)
library(tidyr)
library(psych)      # für describe()
library(summarytools) # alternativ für detailliertere deskriptive Statistiken
library(dplyr)

# Daten einlesen (Pfad anpassen!)
daten <- read_sav("data/COVID_study_GE_T1-T5.sav")

# ============================================================================
# Variablenübersicht für alle Messzeitpunkte
# ============================================================================

# Definition der Variablen für jeden Messzeitpunkt
variablen_t1 <- c("s.age.t1", "s.gen.t1", "s.ed.t1r", "s.inc.t1r", 
                  "c.con.t1r", "c.vir.t1r")

# ============================================================================
# Complete Cases für jeden Messzeitpunkt filtern
# ============================================================================

data_t1_complete_descriptive <- daten %>%
  dplyr::select(all_of(variablen_t1)) %>%
  na.omit()

# ============================================================================
# Anzahl Complete Cases pro Messzeitpunkt
# ============================================================================

cat("\n=== ANZAHL COMPLETE CASES PRO MESSZEITPUNKT ===\n")
cat("T1:", nrow(data_t1_complete_descriptive), "Complete Cases\n")

# ============================================================================
# Deskriptive Statistik für jeden Messzeitpunkt
# ============================================================================

# --- T1 ---
cat("\n\n========================================\n")
cat("DESKRIPTIVE STATISTIK - MESSZEITPUNKT T1\n")
cat("========================================\n")
cat("N =", nrow(data_t1_complete_descriptive), "\n\n")

# Kontinuierliche Variablen
cat("--- Alter ---\n")
print(summary(data_t1_complete_descriptive$s.age.t1))
cat("\nSD:", sd(data_t1_complete_descriptive$s.age.t1, na.rm = TRUE), "\n\n")

# Kategoriale Variablen - Häufigkeiten
cat("--- Geschlecht ---\n")
print(table(data_t1_complete_descriptive$s.gen.t1))
print(prop.table(table(data_t1_complete_descriptive$s.gen.t1)) * 100)

cat("\n--- Bildungsabschluss ---\n")
print(table(data_t1_complete_descriptive$s.ed.t1r))
print(prop.table(table(data_t1_complete_descriptive$s.ed.t1r)) * 100)

cat("\n--- Haushaltseinkommen ---\n")
print(table(data_t1_complete_descriptive$s.inc.t1r))
print(prop.table(table(data_t1_complete_descriptive$s.inc.t1r)) * 100)

cat("\n--- Physischer Kontakt ---\n")
print(table(data_t1_complete_descriptive$c.con.t1r))
print(prop.table(table(data_t1_complete_descriptive$c.con.t1r)) * 100)

cat("\n--- Virtueller Kontakt ---\n")
print(table(data_t1_complete_descriptive$c.vir.t1r))
print(prop.table(table(data_t1_complete_descriptive$c.vir.t1r)) * 100)

# ============================================================================
# Optional: Zusammenfassende Tabelle erstellen
# ============================================================================

# Funktion zur Erstellung einer Übersichtstabelle
create_summary_table <- function(data_list, timepoint_names) {
  summary_list <- list()
  
  for (i in seq_along(data_list)) {
    tp <- timepoint_names[i]
    df <- data_list[[i]]
    
    summary_list[[tp]] <- data.frame(
      Zeitpunkt = tp,
      N = nrow(df),
      Alter_M = mean(df[[1]], na.rm = TRUE),
      Alter_SD = sd(df[[1]], na.rm = TRUE)
    )
  }
  
  do.call(rbind, summary_list)
}

# Anwendung
summary_table <- create_summary_table(
  list(data_t1_complete_descriptive),
  c("T1")
)

cat("\n\n========================================\n")
cat("ÜBERSICHTSTABELLE ALLER MESSZEITPUNKTE\n")
cat("========================================\n")
print(summary_table)

# ============================================================================
# Optional: Export der Complete Cases Datensätze
# ============================================================================

write.csv(data_t1_complete_descriptive, "complete_cases_t1_descriptive.csv", row.names = FALSE)

cat("\n=== SKRIPT ERFOLGREICH ABGESCHLOSSEN ===\n")
