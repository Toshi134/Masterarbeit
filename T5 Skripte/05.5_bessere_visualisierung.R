# ============================================
# MASTERARBEIT: Verbesserte Visualisierung
# Skript 05.5: Netzwerk mit Farben/Gruppen
# ============================================

library(qgraph)
library(tidyverse)

# Netzwerk laden
network_T5 <- readRDS("results/network_T5.rds")

# Variablenlisten laden
var_lists <- readRDS("data/variable_lists.rds")

# ============================================
# GRUPPEN DEFINIEREN
# ============================================

# Welche Variablen gehören zu welcher Gruppe?
node_names <- colnames(network_T5$graph)

# Gruppen-Zuordnung erstellen
groups <- list(
  "ADNM-8" = which(node_names %in% var_lists$adnm_t5),
  "Coping" = which(node_names %in% var_lists$cope_t5)
)

# Check ob alle Knoten zugeordnet sind
cat("Gruppen-Zuordnung:\n")
cat("ADNM-8:", length(groups$`ADNM-8`), "Knoten\n")
cat("Coping:", length(groups$Coping), "Knoten\n")
cat("Total:", sum(lengths(groups)), "/ 22\n\n")

# ============================================
# SCHÖNERE KNOTENNAMEN
# ============================================

# Kürzel für bessere Lesbarkeit
nice_names <- node_names

# ADNM umbenennen
nice_names <- str_replace(nice_names, "ad\\.(\\d)\\.t5", "ADNM_\\1")

# Coping umbenennen (mit aussagekräftigen Namen)
coping_names <- c(
  "bcope.acc.t5" = "Acceptance",
  "bcope.act.t5" = "Active Coping",
  "bcope.behd.t5" = "Behavioral Diseng.",
  "bcope.den.t5" = "Denial",
  "bcope.dist.t5" = "Self-Distraction",
  "bcope.esupp.t5" = "Emotional Support",
  "bcope.hu.t5" = "Humor",
  "bcope.isupp.t5" = "Instrumental Supp.",
  "bcope.pl.t5" = "Planning",
  "bcope.posr.t5" = "Positive Reframing",
  "bcope.rel.t5" = "Religion",
  "bcope.sbl.t5" = "Self-Blame",
  "bcope.sub.t5" = "Substance Use",
  "bcope.ven.t5" = "Venting"
)

for(i in seq_along(coping_names)) {
  nice_names <- str_replace(nice_names, names(coping_names)[i], coping_names[i])
}

# ============================================
# VISUALISIERUNG MIT FARBEN
# ============================================

cat("Erstelle farbige Visualisierung...\n")

# Farben definieren
group_colors <- c(
  "#ffffff",  # für ADNM (Symptome)
  "#c7c7c7"  # für Coping
)

# Große, hochauflösende Visualisierung
pdf("plots/network_T5_colored.pdf", width = 16, height = 16)

plot(network_T5,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T5: ADNM-8 & Coping (N = 691)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PDF gespeichert: plots/network_T5_colored.pdf\n")

# Auch als PNG
png("plots/network_T5_colored.png", width = 3000, height = 3000, res = 200)

plot(network_T5,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = nice_names,
     label.cex = 1.0,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 0.6,
     nodeNames = nice_names,
     title = "Netzwerk T5: ADNM-8 % Coping (N = 691)",
     borders = TRUE,
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PNG gespeichert: plots/network_T5_colored.png\n")

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
short_names <- str_replace(short_names, "Active Coping", "Act.Cope")

pdf("plots/network_T5_compact.pdf", width = 15, height = 15)

plot(network_T5,
     layout = "spring",
     groups = groups,
     color = group_colors,
     labels = short_names,
     label.cex = 0.8,
     label.scale = FALSE,
     legend = TRUE,
     legend.cex = 1.0,
     title = "Network T5 (N = 691)",
     borders = TRUE,
     vsize = 6,
     mar = c(2, 2, 2, 2))

dev.off()

cat("✓ Kompakte Version gespeichert: plots/network_T5_compact.pdf\n")

cat("\n=== FERTIG ===\n")
cat("Öffne die PDFs/PNGs um die farbigen Netzwerke anzusehen!\n")
