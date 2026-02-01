# ============================================
# MASTERARBEIT: Kanten-Analyse
# Skript 07: Spezifische Verbindungen analysieren
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

# ============================================
# H1a: POSITIVE REFRAMING & ACCEPTANCE → ADNM
# ============================================

cat("=== H1a: ADAPTIVE COPING → ADNM (negativ erwartet) ===\n\n")

# Positive Reframing → ADNM
posr_adnm <- get_edges_between_groups(
  edge_matrix, 
  "bcope.posr.t1", 
  var_lists$adnm_t1
)

cat("--- POSITIVE REFRAMING → ADNM ---\n")
if(nrow(posr_adnm) > 0) {
  print(posr_adnm)
  cat("\nZusammenfassung:\n")
  cat("  Anzahl Verbindungen:", nrow(posr_adnm), "\n")
  cat("  Davon negativ:", sum(posr_adnm$Direction == "Negative"), "\n")
  cat("  Davon positiv:", sum(posr_adnm$Direction == "Positive"), "\n")
  cat("  Durchschnittliche Stärke:", round(mean(posr_adnm$Abs_Weight), 3), "\n")
  cat("  Durchschnittliches Gewicht (mit Vorzeichen):", round(mean(posr_adnm$Weight), 3), "\n")
} else {
  cat("  KEINE direkten Verbindungen!\n")
}

cat("\n")

# Acceptance → ADNM
acc_adnm <- get_edges_between_groups(
  edge_matrix, 
  "bcope.acc.t1", 
  var_lists$adnm_t1
)

cat("--- ACCEPTANCE → ADNM ---\n")
if(nrow(acc_adnm) > 0) {
  print(acc_adnm)
  cat("\nZusammenfassung:\n")
  cat("  Anzahl Verbindungen:", nrow(acc_adnm), "\n")
  cat("  Davon negativ:", sum(acc_adnm$Direction == "Negative"), "\n")
  cat("  Davon positiv:", sum(acc_adnm$Direction == "Positive"), "\n")
  cat("  Durchschnittliche Stärke:", round(mean(acc_adnm$Abs_Weight), 3), "\n")
  cat("  Durchschnittliches Gewicht (mit Vorzeichen):", round(mean(acc_adnm$Weight), 3), "\n")
} else {
  cat("  KEINE direkten Verbindungen!\n")
}

cat("\n")

# ============================================
# H1b: BEHAVIORAL DISENGAGEMENT → ADNM (positiv erwartet)
# ============================================

cat("=== H1b: MALADAPTIVE COPING → ADNM (positiv erwartet) ===\n\n")

behd_adnm <- get_edges_between_groups(
  edge_matrix, 
  "bcope.behd.t1", 
  var_lists$adnm_t1
)

cat("--- BEHAVIORAL DISENGAGEMENT → ADNM ---\n")
if(nrow(behd_adnm) > 0) {
  print(behd_adnm)
  cat("\nZusammenfassung:\n")
  cat("  Anzahl Verbindungen:", nrow(behd_adnm), "\n")
  cat("  Davon positiv:", sum(behd_adnm$Direction == "Positive"), "\n")
  cat("  Davon negativ:", sum(behd_adnm$Direction == "Negative"), "\n")
  cat("  Durchschnittliche Stärke:", round(mean(behd_adnm$Abs_Weight), 3), "\n")
  cat("  Durchschnittliches Gewicht (mit Vorzeichen):", round(mean(behd_adnm$Weight), 3), "\n")
} else {
  cat("  KEINE direkten Verbindungen!\n")
}

cat("\n")

# ============================================
# VERGLEICH: ALLE COPING → ADNM
# ============================================

cat("=== VERGLEICH: ALLE COPING-STRATEGIEN → ADNM ===\n\n")

all_coping_adnm <- data.frame()

for(cope_var in var_lists$cope_t1) {
  
  edges <- get_edges_between_groups(edge_matrix, cope_var, var_lists$adnm_t1)
  
  if(nrow(edges) > 0) {
    summary_row <- data.frame(
      Coping_Strategy = cope_var,
      N_Edges = nrow(edges),
      N_Negative = sum(edges$Direction == "Negative"),
      N_Positive = sum(edges$Direction == "Positive"),
      Mean_Weight = mean(edges$Weight),  # Mit Vorzeichen!
      Mean_Abs_Weight = mean(edges$Abs_Weight),
      Sum_Abs_Weight = sum(edges$Abs_Weight)  # = Strength zu ADNM!
    )
    all_coping_adnm <- rbind(all_coping_adnm, summary_row)
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
    all_coping_adnm <- rbind(all_coping_adnm, summary_row)
  }
}

# Schöne Labels
all_coping_adnm <- all_coping_adnm %>%
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

cat("ALLE COPING-STRATEGIEN → ADNM\n")
cat("(sortiert nach durchschnittlichem Gewicht, negativ = protektiv)\n\n")

display_table <- all_coping_adnm %>%
  select(Label, N_Edges, N_Negative, N_Positive, Mean_Weight, Sum_Abs_Weight)

print(display_table, row.names = FALSE)

# ============================================
# H1a TEST: Sind Pos.Refr. & Acceptance am negativsten?
# ============================================

cat("\n\n--- HYPOTHESEN-TEST H1a ---\n")

# Ranking nach Mean_Weight (negativste = beste)
all_coping_adnm <- all_coping_adnm %>%
  arrange(Mean_Weight) %>%
  mutate(Rank_Negativity = row_number())

posr_rank <- all_coping_adnm %>% filter(Coping_Strategy == "bcope.posr.t1") %>% pull(Rank_Negativity)
acc_rank <- all_coping_adnm %>% filter(Coping_Strategy == "bcope.acc.t1") %>% pull(Rank_Negativity)

cat("\nRanking (nach negativem Mean Weight, 1 = am negativsten):\n")
cat("  Positive Reframing: Rang", posr_rank, "von 14\n")
cat("  Acceptance: Rang", acc_rank, "von 14\n")

if(posr_rank <= 3 | acc_rank <= 3) {
  cat("\n✓ Mindestens eine der beiden Strategien unter Top 3!\n")
} else {
  cat("\n⚠ Beide NICHT unter Top 3 der negativsten Verbindungen.\n")
}

# ============================================
# H1b TEST: Ist Behav.Dis. am positivsten?
# ============================================

cat("\n\n--- HYPOTHESEN-TEST H1b ---\n")

all_coping_adnm_pos <- all_coping_adnm %>%
  arrange(desc(Mean_Weight)) %>%
  mutate(Rank_Positivity = row_number())

behd_rank <- all_coping_adnm_pos %>% filter(Coping_Strategy == "bcope.behd.t1") %>% pull(Rank_Positivity)

cat("\nRanking (nach positivem Mean Weight, 1 = am positivsten):\n")
cat("  Behavioral Disengagement: Rang", behd_rank, "von 14\n")

if(behd_rank <= 3) {
  cat("✓ Unter Top 3 der positivsten Verbindungen!\n")
} else {
  cat("⚠ NICHT unter Top 3.\n")
}

# ============================================
# VISUALISIERUNG: Coping → ADNM Verbindungen
# ============================================

cat("\n\nErstelle Visualisierung...\n")

# Barplot: Mean Weight pro Coping-Strategie
pdf("plots/coping_adnm_connections.pdf", width = 10, height = 8)

par(mar = c(5, 12, 4, 2))

colors <- ifelse(all_coping_adnm$Mean_Weight < 0, "#2ECC71", "#E74C3C")

barplot(all_coping_adnm$Mean_Weight,
        names.arg = all_coping_adnm$Label,
        horiz = TRUE,
        las = 1,
        col = colors,
        main = "Average Edge Weight: Coping Strategies → ADNM-8 Symptoms",
        xlab = "Mean Edge Weight (negative = protective)",
        cex.names = 0.8)

abline(v = 0, lty = 2, lwd = 2)

legend("bottomright",
       legend = c("Negative (protective)", "Positive (risk)"),
       fill = c("#2ECC71", "#E74C3C"))

dev.off()

cat("✓ Gespeichert: plots/coping_adnm_connections.pdf\n")

# ============================================
# TABELLE SPEICHERN
# ============================================

write.csv(all_coping_adnm, "results/coping_adnm_edges.csv", row.names = FALSE)
cat("✓ Tabelle gespeichert: results/coping_adnm_edges.csv\n")

cat("\n=== FERTIG ===\n")
