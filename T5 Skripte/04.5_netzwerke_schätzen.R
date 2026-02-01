# ============================================
# MASTERARBEIT: Netzwerkanalyse ADNM-8
# Skript 04.5: Netzwerke schätzen
# ============================================

library(qgraph)
library(bootnet)
library(tidyverse)

# ============================================
# DATEN LADEN
# ============================================

daten_T5_complete <- readRDS("data/daten_T5_complete.rds")

cat("=== NETZWERK T5 SCHÄTZEN ===\n\n")
cat("Sample size:", nrow(daten_T5_complete), "\n")
cat("Anzahl Variablen:", ncol(daten_T5_complete), "\n\n")

# ============================================
# NETZWERK SCHÄTZEN
# ============================================

cat("Schätze Netzwerk mit EBICglasso...\n")
cat("(Dies kann 1-2 Minuten dauern)\n\n")

# Zeitmessung
start_time <- Sys.time()

network_T5 <- estimateNetwork(
  daten_T5_complete,
  default = "EBICglasso",      # LASSO mit EBIC model selection
  corMethod = "cor_auto",      # Automatische Korrelationsmethode
  tuning = 0.5                 # Gamma-Parameter (Standard)
)

end_time <- Sys.time()
cat("✓ Netzwerk geschätzt in", round(difftime(end_time, start_time, units = "secs"), 1), "Sekunden\n\n")

# ============================================
# NETZWERK-STATISTIKEN
# ============================================

cat("=== NETZWERK-EIGENSCHAFTEN ===\n\n")

# Anzahl Knoten
n_nodes <- ncol(network_T5$graph)
cat("Anzahl Knoten:", n_nodes, "\n")

# Anzahl möglicher Kanten
n_possible_edges <- (n_nodes * (n_nodes - 1)) / 2
cat("Mögliche Kanten:", n_possible_edges, "\n")

# Anzahl nicht-null Kanten
edge_matrix <- network_T5$graph[upper.tri(network_T5$graph)]
n_nonzero_edges <- sum(edge_matrix != 0)
cat("Nicht-null Kanten:", n_nonzero_edges, "\n")

# Sparsity (Anteil Null-Kanten)
sparsity <- round(100 * mean(edge_matrix == 0), 1)
cat("Sparsity:", sparsity, "%\n")

# Kantengewichte: Durchschnitt und Range
nonzero_edges <- edge_matrix[edge_matrix != 0]
cat("\nKantengewichte (nur nicht-null):\n")
cat("  Min:", round(min(abs(nonzero_edges)), 3), "\n")
cat("  Max:", round(max(abs(nonzero_edges)), 3), "\n")
cat("  Mean:", round(mean(abs(nonzero_edges)), 3), "\n")
cat("  Median:", round(median(abs(nonzero_edges)), 3), "\n")

# Positive vs. negative Kanten
n_positive <- sum(nonzero_edges > 0)
n_negative <- sum(nonzero_edges < 0)
cat("\nPositive Kanten:", n_positive, "(", round(100*n_positive/n_nonzero_edges, 1), "%)\n")
cat("Negative Kanten:", n_negative, "(", round(100*n_negative/n_nonzero_edges, 1), "%)\n")

# ============================================
# NETZWERK SPEICHERN
# ============================================

saveRDS(network_T5, "results/network_T5.rds")
cat("\n✓ Netzwerk gespeichert: results/network_T5.rds\n")

# ============================================
# EINFACHE VISUALISIERUNG
# ============================================

cat("\n=== VISUALISIERUNG ERSTELLEN ===\n\n")

# Variablennamen kürzen für bessere Lesbarkeit
node_names <- colnames(daten_T5_complete)

# PDF erstellen
pdf("plots/network_T5_simple.pdf", width = 14, height = 14)

plot(network_T5, 
     layout = "spring",
     labels = node_names,
     label.cex = 0.8,           # Schriftgröße Labels
     label.scale = FALSE,
     edge.labels = FALSE,       # Keine Kantengewichte anzeigen (zu voll)
     title = "Netzwerk T5 (N = 2306)",
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ Einfache Visualisierung gespeichert: plots/network_T5_simple.pdf\n")

# Auch als PNG für schnelle Vorschau
png("plots/network_T5_simple.png", width = 2000, height = 2000, res = 150)

plot(network_T5, 
     layout = "spring",
     labels = node_names,
     label.cex = 0.8,
     label.scale = FALSE,
     edge.labels = FALSE,
     title = "Netzwerk T5 (N = 2306)",
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PNG-Version gespeichert: plots/network_T5_simple.png\n")

# ============================================
# ZUSAMMENFASSUNG
# ============================================

cat("\n\n=== ZUSAMMENFASSUNG ===\n")
cat("✓ Netzwerk für T5 erfolgreich geschätzt\n")
cat("✓", n_nonzero_edges, "von", n_possible_edges, "möglichen Kanten sind nicht-null\n")
cat("✓ Sparsity:", sparsity, "%\n")
cat("✓ Dateien gespeichert in results/ und plots/\n\n")

cat("NÄCHSTE SCHRITTE:\n")
cat("1. Öffne plots/network_T5_simple.png um das Netzwerk anzusehen\n")
cat("2. Dann: Schönere Visualisierung mit Gruppen/Farben erstellen\n")
cat("3. Dann: Zentralitätsmaße berechnen\n")
