# ============================================
# MASTERARBEIT: Kanten-Analyse
# Skript 09: Spezifische Verbindungen analysieren
# ============================================

library(qgraph)
library(tidyverse)

# Netzwerk laden
network_T1 <- readRDS("results/network_T1.rds")
var_lists <- readRDS("data/variable_lists.rds")

# Kantengewichte-Matrix extrahieren
edge_matrix <- network_T1$graph

# ============================================
# FUNKTION: Kanten zwischen zwei Gruppen
# ============================================

get_edges_between_groups <- function(edge_matrix, group1_vars, group2_vars) {
  
  edges <- data.frame()
  
  for(var1 in group1_vars) {
    for(var2 in group2_vars) {
      
      # Kantengewicht holen
      if(var1 %in% rownames(edge_matrix) & var2 %in% colnames(edge_matrix)) {
        weight <- edge_matrix[var1, var2]
        
        # Nur nicht-null Kanten
        if(weight != 0) {
          edges <- rbind(edges, data.frame(
            From = var1,
            To = var2,
            Weight = weight,
            Abs_Weight = abs(weight),
            Direction = ifelse(weight > 0, "Positive", "Negative")
          ))
        }
      }
    }
  }
  
  return(edges)
}

# =================================================
# H2a: POSITIVE REFRAMING & ACCEPTANCE → STRESSOREN
# =================================================

cat("=== H1a: ADAPTIVE COPING → STRESSOREN (negativ erwartet) ===\n\n")

# Positive Reframing → Stressoren
posr_stressors <- get_edges_between_groups(
  edge_matrix, 
  "bcope.posr.t1", 
  var_lists$stress_t1
)

cat("--- POSITIVE REFRAMING → STRESSORS ---\n")
if(nrow(posr_stressors) > 0) {
  print(posr_stressors)
  cat("\nZusammenfassung:\n")
  cat("  Anzahl Verbindungen:", nrow(posr_stressors), "\n")
  cat("  Davon negativ:", sum(posr_stressors$Direction == "Negative"), "\n")
  cat("  Davon positiv:", sum(posr_stressors$Direction == "Positive"), "\n")
  cat("  Durchschnittliche Stärke:", round(mean(posr_stressors$Abs_Weight), 3), "\n")
  cat("  Durchschnittliches Gewicht (mit Vorzeichen):", round(mean(posr_stressors$Weight), 3), "\n")
} else {
  cat("  KEINE direkten Verbindungen!\n")
}

cat("\n")

# Acceptance → Stressoren
acc_stressors <- get_edges_between_groups(
  edge_matrix, 
  "bcope.acc.t1", 
  var_lists$stress_t1
)

cat("--- ACCEPTANCE → STRESSOREN ---\n")
if(nrow(acc_stressors) > 0) {
  print(acc_stressors)
  cat("\nZusammenfassung:\n")
  cat("  Anzahl Verbindungen:", nrow(acc_stressors), "\n")
  cat("  Davon negativ:", sum(acc_stressors$Direction == "Negative"), "\n")
  cat("  Davon positiv:", sum(acc_stressors$Direction == "Positive"), "\n")
  cat("  Durchschnittliche Stärke:", round(mean(acc_stressors$Abs_Weight), 3), "\n")
  cat("  Durchschnittliches Gewicht (mit Vorzeichen):", round(mean(acc_stressors$Weight), 3), "\n")
} else {
  cat("  KEINE direkten Verbindungen!\n")
}

cat("\n")

# ============================================
# VERGLEICH: ALLE COPING → STRESSOREN
# ============================================

cat("=== VERGLEICH: ALLE COPING-STRATEGIEN → STRESSOREN ===\n\n")

all_coping_stressors <- data.frame()

for(cope_var in var_lists$cope_t1) {
  
  edges <- get_edges_between_groups(edge_matrix, cope_var, var_lists$stress_t1)
  
  if(nrow(edges) > 0) {
    summary_row <- data.frame(
      Coping_Strategy = cope_var,
      N_Edges = nrow(edges),
      N_Negative = sum(edges$Direction == "Negative"),
      N_Positive = sum(edges$Direction == "Positive"),
      Mean_Weight = mean(edges$Weight),  # Mit Vorzeichen!
      Mean_Abs_Weight = mean(edges$Abs_Weight),
      Sum_Abs_Weight = sum(edges$Abs_Weight)  # = Strength zu Stressoren!
    )
    all_coping_stressors <- rbind(all_coping_stressors, summary_row)
  } else {
    summary_row <- data.frame(
      Coping_Strategy = cope_var,
      N_Edges = 0,
      N_Negative = 0,
      N_Positive = 0,
      Mean_Weight = 0,
      Mean_Abs_Weight = 0,
      Sum_Abs_Weight = 0
    )
    all_coping_stressors <- rbind(all_coping_stressors, summary_row)
  }
}

# Schöne Labels
all_coping_stressors <- all_coping_stressors %>%
  mutate(
    Label = case_when(
      Coping_Strategy == "bcope.posr.t1" ~ "Positive Reframing",
      Coping_Strategy == "bcope.acc.t1" ~ "Acceptance",
      Coping_Strategy == "bcope.behd.t1" ~ "Behavioral Disengagement",
      Coping_Strategy == "bcope.sub.t1" ~ "Substance Use",
      Coping_Strategy == "bcope.act.t1" ~ "Active Coping",
      Coping_Strategy == "bcope.den.t1" ~ "Denial",
      Coping_Strategy == "bcope.dist.t1" ~ "Self-Distraction",
      Coping_Strategy == "bcope.esupp.t1" ~ "Emotional Support",
      Coping_Strategy == "bcope.hu.t1" ~ "Humor",
      Coping_Strategy == "bcope.isupp.t1" ~ "Instrumental Support",
      Coping_Strategy == "bcope.pl.t1" ~ "Planning",
      Coping_Strategy == "bcope.rel.t1" ~ "Religion",
      Coping_Strategy == "bcope.sbl.t1" ~ "Self-Blame",
      Coping_Strategy == "bcope.ven.t1" ~ "Venting",
      TRUE ~ Coping_Strategy
    )
  ) %>%
  arrange(Mean_Weight)  # Sortiert: negativste zuerst

cat("ALLE COPING-STRATEGIEN → STRESSOREN\n")
cat("(sortiert nach durchschnittlichem Gewicht, negativ = protektiv)\n\n")

display_table <- all_coping_stressors %>%
  select(Label, N_Edges, N_Negative, N_Positive, Mean_Weight, Sum_Abs_Weight)

print(display_table, row.names = FALSE)

# ============================================
# H2a TEST: Sind Pos.Refr. & Acceptance am negativsten?
# ============================================

cat("\n\n--- HYPOTHESEN-TEST H2a ---\n")

# Ranking nach Mean_Weight (negativste = beste)
all_coping_stressors <- all_coping_stressors %>%
  arrange(Mean_Weight) %>%
  mutate(Rank_Negativity = row_number())

posr_rank <- all_coping_stressors %>% filter(Coping_Strategy == "bcope.posr.t1") %>% pull(Rank_Negativity)
acc_rank <- all_coping_stressors %>% filter(Coping_Strategy == "bcope.acc.t1") %>% pull(Rank_Negativity)

cat("\nRanking (nach negativem Mean Weight, 1 = am negativsten):\n")
cat("  Positive Reframing: Rang", posr_rank, "von 14\n")
cat("  Acceptance: Rang", acc_rank, "von 14\n")

if(posr_rank <= 3 | acc_rank <= 3) {
  cat("\n✓ Mindestens eine der beiden Strategien unter Top 3!\n")
} else {
  cat("\n⚠ Beide NICHT unter Top 3 der negativsten Verbindungen.\n")
}

# ============================================
# VISUALISIERUNG: Coping → STRESSOREN Verbindungen
# ============================================

cat("\n\nErstelle Visualisierung...\n")

# Barplot: Mean Weight pro Coping-Strategie
pdf("plots/coping_stress_connections.pdf", width = 16, height = 16)

par(mar = c(5, 12, 4, 2))

colors <- ifelse(all_coping_stressors$Mean_Weight < 0, "#2ECC71", "#E74C3C")

barplot(all_coping_stressors$Mean_Weight,
        names.arg = all_coping_stressors$Label,
        horiz = TRUE,
        las = 1,
        col = colors,
        main = "Average Edge Weight: Coping Strategies → Stressors",
        xlab = "Mean Edge Weight (negative = protective)",
        xlim = c(-0.03, 0.03),
        cex.names = 0.8)

abline(v = 0, lty = 2, lwd = 2)

legend("bottomright",
       legend = c("Negative (protective)", "Positive (risk)"),
       fill = c("#2ECC71", "#E74C3C"))

dev.off()

cat("✓ Gespeichert: plots/coping_stress_connections.pdf\n")

# ============================================
# TABELLE SPEICHERN
# ============================================

write.csv(all_coping_stressors, "results/coping_stress_edges.csv", row.names = FALSE)
cat("✓ Tabelle gespeichert: results/coping_stress_edges.csv\n")

cat("\n=== FERTIG ===\n")
