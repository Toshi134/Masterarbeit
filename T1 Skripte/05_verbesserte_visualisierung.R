# ============================================
# MASTERARBEIT: Verbesserte Visualisierung
# Skript 05: Netzwerk mit Farben/Gruppen
# ============================================

library(qgraph)
library(tidyverse)

# Netzwerk laden
network_T1 <- readRDS("results/network_T1.rds")

# Variablenlisten laden
var_lists <- readRDS("data/variable_lists.rds")

# ============================================
# GRUPPEN DEFINIEREN
# ============================================

# Welche Variablen gehören zu welcher Gruppe?
node_names <- colnames(network_T1$graph)

# Gruppen-Zuordnung erstellen
groups <- list(
  "ADNM-8" = which(node_names %in% var_lists$adnm_t1),
  "Coping" = which(node_names %in% var_lists$cope_t1),
  "Stressoren" = which(node_names %in% var_lists$stress_t1)
)

# Check ob alle Knoten zugeordnet sind
cat("Gruppen-Zuordnung:\n")
cat("ADNM-8:", length(groups$`ADNM-8`), "Knoten\n")
cat("Coping:", length(groups$Coping), "Knoten\n")
cat("Stressoren:", length(groups$Stressoren), "Knoten\n")
cat("Total:", sum(lengths(groups)), "/ 37\n\n")

# ============================================
# SCHÖNERE KNOTENNAMEN
# ============================================

# Kürzel für bessere Lesbarkeit
nice_names <- node_names

# ADNM umbenennen
nice_names <- str_replace(nice_names, "ad\\.(\\d)\\.t1", "ADNM_\\1")

# Coping umbenennen (mit aussagekräftigen Namen)
coping_names <- c(
  "bcope.acc.t1" = "Acceptance",
  "bcope.act.t1" = "Active Coping",
  "bcope.behd.t1" = "Behavioral Diseng.",
  "bcope.den.t1" = "Denial",
  "bcope.dist.t1" = "Self-Distraction",
  "bcope.esupp.t1" = "Emotional Support",
  "bcope.hu.t1" = "Humor",
  "bcope.isupp.t1" = "Instrumental Supp.",
  "bcope.pl.t1" = "Planning",
  "bcope.posr.t1" = "Positive Reframing",
  "bcope.rel.t1" = "Religion",
  "bcope.sbl.t1" = "Self-Blame",
  "bcope.sub.t1" = "Substance Use",
  "bcope.ven.t1" = "Venting"
)

for(i in seq_along(coping_names)) {
  nice_names <- str_replace(nice_names, names(coping_names)[i], coping_names[i])
}

# Stressoren umbenennen
stressor_names <- c(
  "bc.1.t1" = "Social Isolation",
  "bc.2.t1" = "Contact Loved Ones",
  "bc.5.t1" = "Visit Critical Ill",
  "bh.1.t1" = "Fear Infection",
  "bh.6.t1" = "Fear Loved Ones",
  "bh.8.t1" = "Death Loved One",
  "bl.1.t1" = "Restricted Activity",
  "bl.7.t1" = "Loss Structure",
  "bp.1.t1" = "Uncertainty",
  "bp.3.t1" = "Crisis Management",
  "bs.4.t1" = "No Retreat",
  "bs.5.t1" = "Conflicts Home",
  "bw.4.t1" = "Income Loss",
  "bw.5.t1" = "Job Loss",
  "bw.7.t1" = "Workload"
)

for(i in seq_along(stressor_names)) {
  nice_names <- str_replace(nice_names, names(stressor_names)[i], stressor_names[i])
}

# ============================================
# VISUALISIERUNG MIT FARBEN
# ============================================

cat("Erstelle farbige Visualisierung...\n")

# Farben definieren
group_colors <- c(
  "#ffffff",  # für ADNM (Symptome)
  "#c7c7c7",  # für Coping
  "#8f8f8f"   # für Stressoren
)

# Große, hochauflösende Visualisierung
pdf("plots/network_T1_colored.pdf", width = 16, height = 16)

plot(network_T1,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T1: ADNM-8, Coping & Stressoren (N = 2306)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PDF gespeichert: plots/network_T1_colored.pdf\n")

# Auch als PNG
png("plots/network_T1_colored.png", width = 3000, height = 3000, res = 200)

plot(network_T1,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T1: ADNM-8, Coping & Stressoren (N = 2306)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PNG gespeichert: plots/network_T1_colored.png\n")

# ============================================
# ALTERNATIVE: Kompaktere Version (für Paper)
# ============================================

# Mit kürzeren Labels
short_names <- nice_names
short_names <- str_replace(short_names, "Behavioral Diseng\\.", "Behav.Dis")
short_names <- str_replace(short_names, "Self-Distraction", "Self-Dist")
short_names <- str_replace(short_names, "Emotional Support", "Emo.Supp")
short_names <- str_replace(short_names, "Instrumental Supp\\.", "Instr.Supp")
short_names <- str_replace(short_names, "Positive Reframing", "Pos.Refr")
short_names <- str_replace(short_names, "Substance Use", "Subst.Use")
short_names <- str_replace(short_names, "Restricted Activity", "Restric.Act")
short_names <- str_replace(short_names, "Crisis Management", "Crisis.Manage")
short_names <- str_replace(short_names, "Active Coping", "Act.Cope")
short_names <- str_replace(short_names, "Fear Loved Ones", "FOI.L.O.")
short_names <- str_replace(short_names, "Fear Infection", "FOI.Self")
short_names <- str_replace(short_names, "Contact Loved Ones", "Contact.L.O.")
short_names <- str_replace(short_names, "Social Isolation", "Soc.Iso")
short_names <- str_replace(short_names, "Loss Structure", "Loss.Struc")
short_names <- str_replace(short_names, "Death Loved One", "Death.L.O.")
short_names <- str_replace(short_names, "Visit Critical Ill", "Visit Crit.Ill")
short_names <- str_replace(short_names, "Conflicts Home", "Confl.Home")

pdf("plots/network_T1_compact.pdf", width = 15, height = 15)

plot(network_T1,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = short_names,
     label.cex = 0.8,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 1.0,
     title = "Network T1 (N = 2306)",
     borders = TRUE,
     vsize = 6,
     mar = c(2, 2, 2, 2))

dev.off()

cat("✓ Kompakte Version gespeichert: plots/network_T1_compact.pdf\n")

cat("\n=== FERTIG ===\n")
cat("Öffne die PDFs/PNGs um die farbigen Netzwerke anzusehen!\n")
