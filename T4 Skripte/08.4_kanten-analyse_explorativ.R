# ============================================
#  Skript 08.4: Kanten-Analyse
#  STRESSOREN → ADNM ANALYSE
# ============================================

cat("\n\n")
cat("========================================\n")
cat("STRESSOREN → ADNM-8 ANALYSE\n")
cat("========================================\n\n")

# ============================================
# EXPLORATIV: Alle Stressoren → ADNM
# ============================================

cat("=== ALLE STRESSOREN → ADNM ===\n\n")

all_stress_adnm <- data.frame()

for(stress_var in var_lists$stress_t4) {
  
  edges <- get_edges_between_groups(edge_matrix, stress_var, var_lists$adnm_t4)
  
  if(nrow(edges) > 0) {
    summary_row <- data.frame(
      Stressor = stress_var,
      N_Edges = nrow(edges),
      N_Negative = sum(edges$Direction == "Negative"),
      N_Positive = sum(edges$Direction == "Positive"),
      Mean_Weight = mean(edges$Weight),
      Mean_Abs_Weight = mean(edges$Abs_Weight),
      Sum_Abs_Weight = sum(edges$Abs_Weight)
    )
    all_stress_adnm <- rbind(all_stress_adnm, summary_row)
  } else {
    summary_row <- data.frame(
      Stressor = stress_var,
      N_Edges = 0,
      N_Negative = 0,
      N_Positive = 0,
      Mean_Weight = 0,
      Mean_Abs_Weight = 0,
      Sum_Abs_Weight = 0
    )
    all_stress_adnm <- rbind(all_stress_adnm, summary_row)
  }
}

# Schöne Labels und Kategorien
all_stress_adnm <- all_stress_adnm %>%
  mutate(
    Label = case_when(
      Stressor == "bc.1.t4" ~ "Social Isolation",
      Stressor == "bc.2.t4" ~ "Restricted Contact Loved Ones",
      Stressor == "bc.5.t4" ~ "Unable to Visit Critical Ill",
      Stressor == "bh.1.t4" ~ "Fear of Infection",
      Stressor == "bh.6.t4" ~ "Fear Loved Ones Infected",
      Stressor == "bh.8.t4" ~ "Death of Loved One",
      Stressor == "bl.1.t4" ~ "Restricted Activity",
      Stressor == "bl.7.t4" ~ "Loss of Daily Structure",
      Stressor == "bp.1.t4" ~ "Uncertainty Pandemic",
      Stressor == "bp.3.t4" ~ "Poor Crisis Management",
      Stressor == "bs.4.t4" ~ "No Place of Retreat",
      Stressor == "bs.5.t4" ~ "Conflicts at Home",
      Stressor == "bw.4.t4" ~ "Income Loss",
      Stressor == "bw.5.t4" ~ "Job Loss",
      Stressor == "bw.7.t4" ~ "Increased Workload",
      TRUE ~ Stressor
    ),
    Category = case_when(
      str_detect(Stressor, "^bc\\.") ~ "Social Contact",
      str_detect(Stressor, "^bh\\.") ~ "Health",
      str_detect(Stressor, "^bl\\.") ~ "Public Life",
      str_detect(Stressor, "^bp\\.") ~ "Pandemic",
      str_detect(Stressor, "^bs\\.") ~ "Home",
      str_detect(Stressor, "^bw\\.") ~ "Work",
      TRUE ~ "Other"
    )
  ) %>%
  arrange(desc(Mean_Weight))  # Sortiert: positivste (= stärkste Risikofaktoren) zuerst

cat("ALLE STRESSOREN → ADNM\n")
cat("(sortiert nach durchschnittlichem Gewicht, positiv = Risikofaktor)\n\n")

display_stress_table <- all_stress_adnm %>%
  select(Label, Category, N_Edges, N_Positive, N_Negative, Mean_Weight, Sum_Abs_Weight)

print(display_stress_table, row.names = FALSE)

# ============================================
# EXPLORATIV: Welche Stressoren am stärksten mit ADNM?
# ============================================

cat("\n\n--- EXPLORATIV: STÄRKSTE STRESSOR-ADNM VERBINDUNGEN ---\n\n")

# Ranking nach Mean_Weight (positivste = stärkste Risikofaktoren)
all_stress_adnm <- all_stress_adnm %>%
  arrange(desc(Mean_Weight)) %>%
  mutate(Rank_Risk = row_number())

cat("TOP 5 STRESSOREN (nach positivem Mean Weight):\n\n")
top5_stress <- all_stress_adnm %>%
  head(5) %>%
  select(Rank_Risk, Label, Category, Mean_Weight, N_Edges)

print(top5_stress, row.names = FALSE)

# ============================================
# EXPLORATIV: Soziale Isolation
# ============================================

cat("\n\n--- EXPLORATIV: SOZIALE ISOLATION → ADNM ---\n\n")

social_iso_rank <- all_stress_adnm %>% 
  filter(Stressor == "bc.1.t4") %>% 
  pull(Rank_Risk)

social_iso_weight <- all_stress_adnm %>% 
  filter(Stressor == "bc.1.t4") %>% 
  pull(Mean_Weight)

cat("Social Isolation:\n")
cat("  Rang:", social_iso_rank, "von 15 Stressoren\n")
cat("  Mean Weight:", round(social_iso_weight, 3), "\n")

# Detaillierte Kanten
social_iso_edges <- get_edges_between_groups(
  edge_matrix, 
  "bc.1.t4", 
  var_lists$adnm_t4
)

if(nrow(social_iso_edges) > 0) {
  cat("\nSpezifische Verbindungen:\n")
  print(social_iso_edges)
  cat("\n  Durchschnitt:", round(mean(social_iso_edges$Weight), 3), "\n")
} else {
  cat("\n  KEINE direkten Verbindungen!\n")
}

# ============================================
# EXPLORATIV: Arbeitsbezogene Stressoren
# ============================================

cat("\n\n--- EXPLORATIV: ARBEITSBEZOGENE STRESSOREN → ADNM ---\n\n")

# Income Loss
income_rank <- all_stress_adnm %>% 
  filter(Stressor == "bw.4.t4") %>% 
  pull(Rank_Risk)

income_weight <- all_stress_adnm %>% 
  filter(Stressor == "bw.4.t4") %>% 
  pull(Mean_Weight)

cat("Income Loss:\n")
cat("  Rang:", income_rank, "von 15 Stressoren\n")
cat("  Mean Weight:", round(income_weight, 3), "\n")

# Job Loss
job_rank <- all_stress_adnm %>% 
  filter(Stressor == "bw.5.t4") %>% 
  pull(Rank_Risk)

job_weight <- all_stress_adnm %>% 
  filter(Stressor == "bw.5.t4") %>% 
  pull(Mean_Weight)

cat("\nJob Loss:\n")
cat("  Rang:", job_rank, "von 15 Stressoren\n")
cat("  Mean Weight:", round(job_weight, 3), "\n")

# Workload
work_rank <- all_stress_adnm %>% 
  filter(Stressor == "bw.7.t4") %>% 
  pull(Rank_Risk)

work_weight <- all_stress_adnm %>% 
  filter(Stressor == "bw.7.t4") %>% 
  pull(Mean_Weight)

cat("\nIncreased Workload:\n")
cat("  Rang:", work_rank, "von 15 Stressoren\n")
cat("  Mean Weight:", round(work_weight, 3), "\n")

# Vergleich
median_stress_weight <- median(all_stress_adnm$Mean_Weight)
cat("\nMedian aller Stressor-ADNM Verbindungen:", round(median_stress_weight, 3), "\n")

cat("\nVergleich zum Median:\n")
cat("  Income Loss:", ifelse(income_weight > median_stress_weight, "überdurchschnittlich", "unterdurchschnittlich"), "\n")
cat("  Job Loss:", ifelse(job_weight > median_stress_weight, "überdurchschnittlich", "unterdurchschnittlich"), "\n")
cat("  Workload:", ifelse(work_weight > median_stress_weight, "überdurchschnittlich", "unterdurchschnittlich"), "\n")

# ============================================
# VERGLEICH: Stressor-Kategorien
# ============================================

cat("\n\n--- VERGLEICH NACH STRESSOR-KATEGORIEN ---\n\n")

category_summary <- all_stress_adnm %>%
  group_by(Category) %>%
  summarise(
    N_Stressors = n(),
    Mean_Weight = mean(Mean_Weight),
    SD_Weight = sd(Mean_Weight),
    Min_Weight = min(Mean_Weight),      # ← JETZT DABEI!
    Max_Weight = max(Mean_Weight),
    Median_Weight = median(Mean_Weight), # Auch Median hinzufügen
    Total_Edges = sum(N_Edges)
  ) %>%
  arrange(desc(Mean_Weight))

print(category_summary, n = 6)

cat("\nInterpretation:\n")
cat("Höherer Mean_Weight = Stärkere Assoziation mit ADNM-Symptomen\n")
cat("Min/Max zeigen die Spannweite innerhalb jeder Kategorie\n")

# ============================================
# VISUALISIERUNG: Stressoren → ADNM
# ============================================

cat("\n\nErstelle Visualisierungen...\n")

# Barplot: Mean Weight pro Stressor
pdf("plots/stressors_adnm_connections.pdf", width = 10, height = 8)

par(mar = c(5, 12, 4, 2))

# Farben nach Kategorie
category_colors <- c(
  "Social Contact" = "#E74C3C",
  "Health" = "#9B59B6",
  "Public Life" = "#3498DB",
  "Pandemic" = "#F39C12",
  "Home" = "#1ABC9C",
  "Work" = "#E67E22"
)

stress_colors <- category_colors[all_stress_adnm$Category]

barplot(all_stress_adnm$Mean_Weight,
        names.arg = all_stress_adnm$Label,
        horiz = TRUE,
        las = 1,
        col = stress_colors,
        main = "Average Edge Weight: Stressors → ADNM-8 Symptoms",
        xlab = "Mean Edge Weight (positive = risk factor)",
        xlim = c(-0.01, 0.06),
        cex.names = 0.7)

abline(v = 0, lty = 2, lwd = 2)

legend("bottomright",
       legend = names(category_colors),
       fill = category_colors,
       cex = 0.7)

dev.off()

cat("✓ Gespeichert: plots/stressors_adnm_connections.pdf\n")

# Vergleich: Top Stressors vs Top Coping
png("plots/top_predictors_adnm.png", width = 1600, height = 1000, res = 120)

par(mfrow = c(1, 2), mar = c(5, 10, 4, 2))

# Links: Top 10 Coping (negativste)
top_coping <- all_coping_adnm %>%
  arrange(Mean_Weight) %>%
  head(10)

barplot(top_coping$Mean_Weight,
        names.arg = top_coping$Label,
        horiz = TRUE,
        las = 1,
        col = "#3498DB",
        main = "Top 10 Coping → ADNM\n(most protective)",
        xlab = "Mean Weight",
        cex.names = 0.7,
        xlim = c(min(top_coping$Mean_Weight) * 1.2, 0))

abline(v = 0, lty = 2)

# Rechts: Top 10 Stressors (positivste)
top_stress <- all_stress_adnm %>%
  arrange(desc(Mean_Weight)) %>%
  head(10)

barplot(top_stress$Mean_Weight,
        names.arg = top_stress$Label,
        horiz = TRUE,
        las = 1,
        col = "#E74C3C",
        main = "Top 10 Stressors → ADNM\n(strongest risk factors)",
        xlab = "Mean Weight",
        cex.names = 0.7,
        xlim = c(0, max(top_stress$Mean_Weight) * 1.2))

abline(v = 0, lty = 2)

dev.off()

cat("✓ Gespeichert: plots/top_predictors_adnm.png\n")

# ============================================
# TABELLEN SPEICHERN
# ============================================

# Stressor-Tabelle
stressor_export <- all_stress_adnm %>%
  select(Label, Category, N_Edges, N_Positive, N_Negative, Mean_Weight, Sum_Abs_Weight, Rank_Risk) %>%
  arrange(Rank_Risk)

write.csv(stressor_export, "results/stressors_adnm_edges.csv", row.names = FALSE)
cat("✓ Stressor-Tabelle: results/stressors_adnm_edges.csv\n")

# Kategorien-Zusammenfassung
write.csv(category_summary, "results/stressor_categories_summary.csv", row.names = FALSE)
cat("✓ Kategorien-Tabelle: results/stressor_categories_summary.csv\n")

cat("\n=== STRESSOR-ANALYSE ABGESCHLOSSEN ===\n")
