# ============================================
# MASTERARBEIT: Verbesserte Visualisierung
# Skript 05: Netzwerk mit Farben/Gruppen
# ============================================

library(qgraph)
library(tidyverse)

# Netzwerk laden
network_T3 <- readRDS("results/network_T3.rds")

# Variablenlisten laden
var_lists <- readRDS("data/variable_lists.rds")

# ============================================
# GRUPPEN DEFINIEREN
# ============================================

# Welche Variablen gehören zu welcher Gruppe?
node_names <- colnames(network_T3$graph)

# Gruppen-Zuordnung erstellen
groups <- list(
  "ADNM-8" = which(node_names %in% var_lists$adnm_t3),
  "Coping" = which(node_names %in% var_lists$cope_t3),
  "Stressoren" = which(node_names %in% var_lists$stress_t3)
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
nice_names <- str_replace(nice_names, "ad\\.(\\d)\\.t3", "ADNM_\\1")

# Coping umbenennen (mit aussagekräftigen Namen)
coping_names <- c(
  "bcope.acc.t3" = "Acceptance",
  "bcope.act.t3" = "Active Coping",
  "bcope.behd.t3" = "Behavioral Diseng.",
  "bcope.den.t3" = "Denial",
  "bcope.dist.t3" = "Self-Distraction",
  "bcope.esupp.t3" = "Emotional Support",
  "bcope.hu.t3" = "Humor",
  "bcope.isupp.t3" = "Instrumental Supp.",
  "bcope.pl.t3" = "Planning",
  "bcope.posr.t3" = "Positive Reframing",
  "bcope.rel.t3" = "Religion",
  "bcope.sbl.t3" = "Self-Blame",
  "bcope.sub.t3" = "Substance Use",
  "bcope.ven.t3" = "Venting"
)

for(i in seq_along(coping_names)) {
  nice_names <- str_replace(nice_names, names(coping_names)[i], coping_names[i])
}

# Stressoren umbenennen
stressor_names <- c(
  "bc.1.t3" = "Social Isolation",
  "bc.2.t3" = "Contact Loved Ones",
  "bc.5.t3" = "Visit Critical Ill",
  "bh.1.t3" = "Fear Infection",
  "bh.6.t3" = "Fear Loved Ones",
  "bh.8.t3" = "Death Loved One",
  "bl.1.t3" = "Restricted Activity",
  "bl.7.t3" = "Loss Structure",
  "bp.1.t3" = "Uncertainty",
  "bp.3.t3" = "Crisis Management",
  "bs.4.t3" = "No Retreat",
  "bs.5.t3" = "Conflicts Home",
  "bw.4.t3" = "Income Loss",
  "bw.5.t3" = "Job Loss",
  "bw.7.t3" = "Workload"
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
pdf("plots/network_T3_colored.pdf", width = 16, height = 16)

plot(network_T3,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T3: ADNM-8, Coping & Stressoren (N = 958)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PDF gespeichert: plots/network_T3_colored.pdf\n")

# Auch als PNG
png("plots/network_T3_colored.png", width = 3000, height = 3000, res = 200)

plot(network_T3,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T3: ADNM-8, Coping & Stressoren (N = 958)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PNG gespeichert: plots/network_T3_colored.png\n")

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

pdf("plots/network_T3_compact.pdf", width = 15, height = 15)

plot(network_T3,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = short_names,
     label.cex = 0.8,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 1.0,
     title = "Network T3 (N = 958)",
     borders = TRUE,
     vsize = 6,
     mar = c(2, 2, 2, 2))

dev.off()

cat("✓ Kompakte Version gespeichert: plots/network_T3_compact.pdf\n")

cat("\n=== FERTIG ===\n")
cat("Öffne die PDFs/PNGs um die farbigen Netzwerke anzusehen!\n")
