# ============================================
# MASTERARBEIT: Zentralitätsanalyse
# Skript 06: Zentralitätsmaße berechnen
# ANGEPASST: Explorativ statt Hypothesen für manche Variablen
# ============================================

library(qgraph)
library(bootnet)
library(tidyverse)

# Netzwerk laden
network_T1 <- readRDS("results/network_T1.rds")
var_lists <- readRDS("data/variable_lists.rds")

# ============================================
# ZENTRALITÄTSMASSE BERECHNEN
# ============================================

cat("=== ZENTRALITÄTSMASSE BERECHNEN ===\n\n")

# Zentralität extrahieren
cent_T1 <- centrality(network_T1)

# Als Dataframe - KORRIGIERT
cent_table <- data.frame(
  Node = names(cent_T1$OutDegree),  # names() statt rownames()
  Strength = as.numeric(cent_T1$OutDegree),
  Betweenness = as.numeric(cent_T1$Betweenness),
  Closeness = as.numeric(cent_T1$Closeness),
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
      Node %in% var_lists$adnm_t1 ~ "ADNM-8",
      Node %in% var_lists$cope_t1 ~ "Coping",
      Node %in% var_lists$stress_t1 ~ "Stressoren",
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
      Node == "bcope.posr.t1" ~ "Positive Reframing (H1a, H1c)",
      Node == "bcope.acc.t1" ~ "Acceptance (H1a)",
      Node == "bcope.behd.t1" ~ "Behavioral Disengagement (H1b)",
      Node == "bcope.sub.t1" ~ "Substance Use (explorativ)",
      Node == "bcope.act.t1" ~ "Active Coping",
      Node == "bcope.den.t1" ~ "Denial",
      Node == "bcope.dist.t1" ~ "Self-Distraction",
      Node == "bcope.esupp.t1" ~ "Emotional Support",
      Node == "bcope.hu.t1" ~ "Humor",
      Node == "bcope.isupp.t1" ~ "Instrumental Support",
      Node == "bcope.pl.t1" ~ "Planning",
      Node == "bcope.rel.t1" ~ "Religion",
      Node == "bcope.sbl.t1" ~ "Self-Blame",
      Node == "bcope.ven.t1" ~ "Venting",
      TRUE ~ Node
    )
  ) %>%
  select(Rank, Label, Strength)

print(head(coping_display, 14), row.names = FALSE)

cat("\n--- HYPOTHESEN-CHECK ---\n\n")

# H1c: Positive Reframing höchste Zentralität?
pos_refr_rank <- coping_cent %>% filter(Node == "bcope.posr.t1") %>% pull(Rank)
cat("H1c - Positive Reframing: Rang", pos_refr_rank, "von 14\n")
if(pos_refr_rank == 1) {
  cat("  ✓ Höchste Zentralität unter Coping-Strategien!\n")
} else {
  cat("  ⚠ NICHT höchste Zentralität (Rang", pos_refr_rank, ")\n")
}

# H1a: Akzeptanz
acc_rank <- coping_cent %>% filter(Node == "bcope.acc.t1") %>% pull(Rank)
cat("\nH1a - Acceptance: Rang", acc_rank, "von 14\n")

# H1b: Behavioral Disengagement
behd_rank <- coping_cent %>% filter(Node == "bcope.behd.t1") %>% pull(Rank)
cat("\nH1b - Behavioral Disengagement: Rang", behd_rank, "von 14\n")

cat("\n--- EXPLORATIV: SUBSTANZKONSUM ---\n\n")

# Substanzkonsum explorativ
sub_rank <- coping_cent %>% filter(Node == "bcope.sub.t1") %>% pull(Rank)
sub_strength <- coping_cent %>% filter(Node == "bcope.sub.t1") %>% pull(Strength)
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
# EXPLORATIV: STRESSOREN
# ============================================

cat("\n\n=== EXPLORATIVE STRESSOR-ANALYSE ===\n\n")

# Alle Stressoren sortiert
stressor_cent <- cent_table %>% 
  filter(Group == "Stressoren") %>%
  arrange(desc(Strength)) %>%
  mutate(Rank = row_number())

cat("ALLE STRESSOREN (sortiert nach Strength):\n\n")
stressor_display <- stressor_cent %>%
  mutate(
    Label = case_when(
      Node == "bc.1.t1" ~ "Social Isolation",
      Node == "bc.2.t1" ~ "Restricted Contact Loved Ones",
      Node == "bc.5.t1" ~ "Unable to Visit Critical Ill",
      Node == "bh.1.t1" ~ "Fear of Infection",
      Node == "bh.6.t1" ~ "Fear Loved Ones Infected",
      Node == "bh.8.t1" ~ "Death of Loved One",
      Node == "bl.1.t1" ~ "Restricted Activity",
      Node == "bl.7.t1" ~ "Loss of Daily Structure",
      Node == "bp.1.t1" ~ "Uncertainty Pandemic",
      Node == "bp.3.t1" ~ "Poor Crisis Management",
      Node == "bs.4.t1" ~ "No Place of Retreat",
      Node == "bs.5.t1" ~ "Conflicts at Home",
      Node == "bw.4.t1" ~ "Income Loss",
      Node == "bw.5.t1" ~ "Job Loss",
      Node == "bw.7.t1" ~ "Increased Workload",
      TRUE ~ Node
    ),
    Category = case_when(
      str_detect(Node, "^bc\\.") ~ "Social Contact",
      str_detect(Node, "^bh\\.") ~ "Health",
      str_detect(Node, "^bl\\.") ~ "Public Life",
      str_detect(Node, "^bp\\.") ~ "Pandemic",
      str_detect(Node, "^bs\\.") ~ "Home",
      str_detect(Node, "^bw\\.") ~ "Work",
      TRUE ~ "Other"
    )
  ) %>%
  select(Rank, Label, Category, Strength)

print(head(stressor_display, n = 15), row.names = FALSE)

cat("\n--- EXPLORATIV: WORK-RELATED STRESSOREN ---\n\n")

# Income Loss
income_rank <- stressor_cent %>% filter(Node == "bw.4.t1") %>% pull(Rank)
income_strength <- stressor_cent %>% filter(Node == "bw.4.t1") %>% pull(Strength)
cat("Income Loss: Rang", income_rank, "von 15 Stressoren\n")
cat("  Strength:", round(income_strength, 3), "\n")

# Job Loss
job_rank <- stressor_cent %>% filter(Node == "bw.5.t1") %>% pull(Rank)
job_strength <- stressor_cent %>% filter(Node == "bw.5.t1") %>% pull(Strength)
cat("\nJob Loss: Rang", job_rank, "von 15 Stressoren\n")
cat("  Strength:", round(job_strength, 3), "\n")

# Workload
work_rank <- stressor_cent %>% filter(Node == "bw.7.t1") %>% pull(Rank)
work_strength <- stressor_cent %>% filter(Node == "bw.7.t1") %>% pull(Strength)
cat("\nIncreased Workload: Rang", work_rank, "von 15 Stressoren\n")
cat("  Strength:", round(work_strength, 3), "\n")

cat("\n--- EXPLORATIV: SOZIALE ISOLATION ---\n\n")

# Soziale Isolation
social_rank <- stressor_cent %>% filter(Node == "bc.1.t1") %>% pull(Rank)
social_strength <- stressor_cent %>% filter(Node == "bc.1.t1") %>% pull(Strength)
cat("Social Isolation: Rang", social_rank, "von 15 Stressoren\n")
cat("  Strength:", round(social_strength, 3), "\n")

# Vergleich mit Median
median_stressor_strength <- median(stressor_cent$Strength)
cat("\nMedian aller Stressoren:", round(median_stressor_strength, 3), "\n")

cat("\nVergleich zum Median:\n")
cat("  Income Loss:", ifelse(income_strength > median_stressor_strength, "überdurchschnittlich", "unterdurchschnittlich"), "\n")
cat("  Job Loss:", ifelse(job_strength > median_stressor_strength, "überdurchschnittlich", "unterdurchschnittlich"), "\n")
cat("  Workload:", ifelse(work_strength > median_stressor_strength, "überdurchschnittlich", "unterdurchschnittlich"), "\n")
cat("  Social Isolation:", ifelse(social_strength > median_stressor_strength, "überdurchschnittlich", "unterdurchschnittlich"), "\n")

# ============================================
# ZENTRALITÄTS-PLOTS
# ============================================

cat("\n\nErstelle Zentralitäts-Plots...\n")

# Standard-Plot (alle drei Maße)
pdf("plots/centrality_T1_all.pdf", width = 12, height = 10)
centralityPlot(network_T1, 
               include = c("Strength", "Betweenness", "Closeness"),
               orderBy = "Strength",
               scale = "z-scores")
dev.off()
cat("✓ Gespeichert: plots/centrality_T1_all.pdf\n")

# Nur Strength
pdf("plots/centrality_T1_strength.pdf", width = 8, height = 12)
centralityPlot(network_T1, 
               include = "Strength",
               orderBy = "Strength",
               scale = "raw")
dev.off()
cat("✓ Gespeichert: plots/centrality_T1_strength.pdf\n")

# Strength mit Gruppen-Farben
png("plots/centrality_T1_strength_colored.png", 
    width = 1200, height = 2000, res = 150)

node_colors <- ifelse(cent_table$Group == "ADNM-8", "#E74C3C",
                      ifelse(cent_table$Group == "Coping", "#3498DB", "#2ECC71"))

order_strength <- order(cent_table$Strength, decreasing = TRUE)

par(mar = c(5, 10, 3, 2))
barplot(cent_table$Strength[order_strength],
        names.arg = cent_table$Node[order_strength],
        horiz = TRUE,
        las = 1,
        col = node_colors[order_strength],
        main = "Strength Centrality T1",
        xlab = "Strength",
        cex.names = 0.7)

legend("bottomright", 
       legend = c("ADNM-8", "Coping", "Stressoren"),
       fill = c("#E74C3C", "#3498DB", "#2ECC71"),
       cex = 0.8)

dev.off()
cat("✓ Gespeichert: plots/centrality_T1_strength_colored.png\n")

# ============================================
# TABELLEN SPEICHERN
# ============================================

# Coping-Tabelle
coping_export <- coping_display %>%
  rename(
    Coping_Strategy = Label,
    Strength_Centrality = Strength
  )
write.csv(coping_export, "results/centrality_coping_T1.csv", row.names = FALSE)
cat("✓ Coping-Tabelle: results/centrality_coping_T1.csv\n")

# Stressor-Tabelle
stressor_export <- stressor_display %>%
  rename(
    Stressor = Label,
    Stressor_Category = Category,
    Strength_Centrality = Strength
  )
write.csv(stressor_export, "results/centrality_stressors_T1.csv", row.names = FALSE)
cat("✓ Stressor-Tabelle: results/centrality_stressors_T1.csv\n")

# Vollständige Tabelle
write.csv(cent_table, "results/centrality_T1_complete.csv", row.names = FALSE)
cat("✓ Vollständige Tabelle: results/centrality_T1_complete.csv\n")

cat("\n=== FERTIG ===\n")
cat("Öffne die CSV-Dateien und PDFs für deine Ergebnisse!\n")
