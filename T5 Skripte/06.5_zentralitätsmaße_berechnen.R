# ============================================
# MASTERARBEIT: Zentralitätsanalyse
# Skript 06.5: Zentralitätsmaße berechnen
# ANGEPASST: Explorativ statt Hypothesen für manche Variablen
# ============================================

library(qgraph)
library(bootnet)
library(tidyverse)

# Netzwerk laden
network_T5 <- readRDS("results/network_T5.rds")
var_lists <- readRDS("data/variable_lists.rds")

# ============================================
# ZENTRALITÄTSMASSE BERECHNEN
# ============================================

cat("=== ZENTRALITÄTSMASSE BERECHNEN ===\n\n")

# Zentralität extrahieren
cent_T5 <- centrality(network_T5)

# Als Dataframe - KORRIGIERT
cent_table <- data.frame(
  Node = names(cent_T5$OutDegree),  # names() statt rownames()
  Strength = as.numeric(cent_T5$OutDegree),
  Betweenness = as.numeric(cent_T5$Betweenness),
  Closeness = as.numeric(cent_T5$Closeness),
  stringsAsFactors = FALSE
)

# Sortieren nach Strength
cent_table <- cent_table %>% 
  arrange(desc(Strength))

# ============================================
# GRUPPEN ZUORDNEN
# ============================================

cent_table <- cent_table %>%
  mutate(
    Group = case_when(
      Node %in% var_lists$adnm_t5 ~ "ADNM-8",
      Node %in% var_lists$cope_t5 ~ "Coping",
      TRUE ~ "Unknown"
    )
  )

# Durchschnittliche Zentralität pro Gruppe
cat("DURCHSCHNITTLICHE STRENGTH PRO GRUPPE:\n\n")
group_means <- cent_table %>%
  group_by(Group) %>%
  summarise(
    N = n(),
    Mean_Strength = mean(Strength),
    SD_Strength = sd(Strength),
    Min_Strength = min(Strength),
    Max_Strength = max(Strength)
  )
print(group_means, n = 3)

cat("\n")

# ============================================
# HYPOTHESEN-RELEVANTE KNOTEN (H1a, H1b, H1c)
# ============================================

cat("=== HYPOTHESEN-RELEVANTE COPING-STRATEGIEN ===\n\n")

# Alle Coping-Strategien sortiert
coping_cent <- cent_table %>% 
  filter(Group == "Coping") %>%
  arrange(desc(Strength)) %>%
  mutate(Rank = row_number())

cat("ALLE COPING-STRATEGIEN (sortiert nach Strength):\n\n")
coping_display <- coping_cent %>%
  mutate(
    Label = case_when(
      Node == "bcope.posr.t5" ~ "Positive Reframing (H1a, H1c)",
      Node == "bcope.acc.t5" ~ "Acceptance (H1a)",
      Node == "bcope.behd.t5" ~ "Behavioral Disengagement (H1b)",
      Node == "bcope.sub.t5" ~ "Substance Use (explorativ)",
      Node == "bcope.act.t5" ~ "Active Coping",
      Node == "bcope.den.t5" ~ "Denial",
      Node == "bcope.dist.t5" ~ "Self-Distraction",
      Node == "bcope.esupp.t5" ~ "Emotional Support",
      Node == "bcope.hu.t5" ~ "Humor",
      Node == "bcope.isupp.t5" ~ "Instrumental Support",
      Node == "bcope.pl.t5" ~ "Planning",
      Node == "bcope.rel.t5" ~ "Religion",
      Node == "bcope.sbl.t5" ~ "Self-Blame",
      Node == "bcope.ven.t5" ~ "Venting",
      TRUE ~ Node
    )
  ) %>%
  select(Rank, Label, Strength)

print(head(coping_display, 14), row.names = FALSE)

cat("\n--- HYPOTHESEN-CHECK ---\n\n")

# H1c: Positive Reframing höchste Zentralität?
pos_refr_rank <- coping_cent %>% filter(Node == "bcope.posr.t5") %>% pull(Rank)
cat("H1c - Positive Reframing: Rang", pos_refr_rank, "von 14\n")
if(pos_refr_rank == 1) {
  cat("  ✓ Höchste Zentralität unter Coping-Strategien!\n")
} else {
  cat("  ⚠ NICHT höchste Zentralität (Rang", pos_refr_rank, ")\n")
}

# H1a: Akzeptanz
acc_rank <- coping_cent %>% filter(Node == "bcope.acc.t5") %>% pull(Rank)
cat("\nH1a - Acceptance: Rang", acc_rank, "von 14\n")

# H1b: Behavioral Disengagement
behd_rank <- coping_cent %>% filter(Node == "bcope.behd.t5") %>% pull(Rank)
cat("\nH1b - Behavioral Disengagement: Rang", behd_rank, "von 14\n")

cat("\n--- EXPLORATIV: SUBSTANZKONSUM ---\n\n")

# Substanzkonsum explorativ
sub_rank <- coping_cent %>% filter(Node == "bcope.sub.t5") %>% pull(Rank)
sub_strength <- coping_cent %>% filter(Node == "bcope.sub.t5") %>% pull(Strength)
cat("Substance Use: Rang", sub_rank, "von 14\n")
cat("Strength:", round(sub_strength, 3), "\n")

# Vergleich mit Median
median_coping_strength <- median(coping_cent$Strength)
if(sub_strength > median_coping_strength) {
  cat("  → Überdurchschnittliche Zentralität (> Median)\n")
} else {
  cat("  → Unterdurchschnittliche Zentralität (< Median)\n")
}

# ============================================
# ZENTRALITÄTS-PLOTS
# ============================================

cat("\n\nErstelle Zentralitäts-Plots...\n")

# Standard-Plot (alle drei Maße)
pdf("plots/centrality_T5_all.pdf", width = 12, height = 10)
centralityPlot(network_T5, 
               include = c("Strength", "Betweenness", "Closeness"),
               orderBy = "Strength",
               scale = "z-scores")
dev.off()
cat("✓ Gespeichert: plots/centrality_T5_all.pdf\n")

# Nur Strength
pdf("plots/centrality_T5_strength.pdf", width = 8, height = 12)
centralityPlot(network_T5, 
               include = "Strength",
               orderBy = "Strength",
               scale = "raw")
dev.off()
cat("✓ Gespeichert: plots/centrality_T5_strength.pdf\n")

# Strength mit Gruppen-Farben
png("plots/centrality_T5_strength_colored.png", 
    width = 1200, height = 2000, res = 150)

node_colors <- ifelse(cent_table$Group == "ADNM-8", "#E74C3C", "#3498DB")

order_strength <- order(cent_table$Strength, decreasing = TRUE)

par(mar = c(5, 10, 3, 2))
barplot(cent_table$Strength[order_strength],
        names.arg = cent_table$Node[order_strength],
        horiz = TRUE,
        las = 1,
        col = node_colors[order_strength],
        main = "Strength Centrality T5",
        xlab = "Strength",
        cex.names = 0.7)

legend("bottomright", 
       legend = c("ADNM-8", "Coping"),
       fill = c("#E74C3C", "#3498DB"),
       cex = 0.8)

dev.off()
cat("✓ Gespeichert: plots/centrality_T5_strength_colored.png\n")

# ============================================
# TABELLEN SPEICHERN
# ============================================

# Coping-Tabelle
coping_export <- coping_display %>%
  rename(
    Coping_Strategy = Label,
    Strength_Centrality = Strength
  )
write.csv(coping_export, "results/centrality_coping_T5.csv", row.names = FALSE)
cat("✓ Coping-Tabelle: results/centrality_coping_T5.csv\n")

# Stressor-Tabelle
stressor_export <- stressor_display %>%
  rename(
    Stressor = Label,
    Stressor_Category = Category,
    Strength_Centrality = Strength
  )
write.csv(stressor_export, "results/centrality_stressors_T5.csv", row.names = FALSE)
cat("✓ Stressor-Tabelle: results/centrality_stressors_T5.csv\n")

# Vollständige Tabelle
write.csv(cent_table, "results/centrality_T5_complete.csv", row.names = FALSE)
cat("✓ Vollständige Tabelle: results/centrality_T5_complete.csv\n")

cat("\n=== FERTIG ===\n")
cat("Öffne die CSV-Dateien und PDFs für deine Ergebnisse!\n")
