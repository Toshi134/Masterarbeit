# ============================================
# MASTERARBEIT: Netzwerkanalyse ADNM-8
# Skript 04: Netzwerke schätzen
# ============================================

library(qgraph)
library(bootnet)
library(tidyverse)

# ============================================
# DATEN LADEN
# ============================================

daten_T2_complete <- readRDS("data/daten_T2_complete.rds")

cat("=== NETZWERK T2 SCHÄTZEN ===\n\n")
cat("Sample size:", nrow(daten_T2_complete), "\n")
cat("Anzahl Variablen:", ncol(daten_T2_complete), "\n\n")

# ============================================
# NETZWERK SCHÄTZEN
# ============================================

cat("Schätze Netzwerk mit EBICglasso...\n")
cat("(Dies kann 1-2 Minuten dauern)\n\n")

# Zeitmessung
start_time <- Sys.time()

network_T2 <- estimateNetwork(
  daten_T2_complete,
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
n_nodes <- ncol(network_T2$graph)
cat("Anzahl Knoten:", n_nodes, "\n")

# Anzahl möglicher Kanten
n_possible_edges <- (n_nodes * (n_nodes - 1)) / 2
cat("Mögliche Kanten:", n_possible_edges, "\n")

# Anzahl nicht-null Kanten
edge_matrix <- network_T2$graph[upper.tri(network_T2$graph)]
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
# VERGLEICH MIT T1
# ============================================

cat("\n\n=== VERGLEICH MIT T1 ===\n\n")

# Lade T1 Netzwerk
if(file.exists("results/network_T1.rds")) {
  network_T1 <- readRDS("results/network_T1.rds")
  
  # T1 Statistiken
  edge_matrix_t1 <- network_T1$graph[upper.tri(network_T1$graph)]
  n_nonzero_t1 <- sum(edge_matrix_t1 != 0)
  sparsity_t1 <- round(100 * mean(edge_matrix_t1 == 0), 1)
  
  # Vergleichstabelle
  comparison <- data.frame(
    Metric = c("Sample Size", "Nicht-null Kanten", "Sparsity (%)", 
               "Mean Edge Weight", "Max Edge Weight"),
    T1 = c(
      2306,
      n_nonzero_t1,
      sparsity_t1,
      round(mean(abs(edge_matrix_t1[edge_matrix_t1 != 0])), 3),
      round(max(abs(edge_matrix_t1[edge_matrix_t1 != 0])), 3)
    ),
    T2 = c(
      nrow(daten_T2_complete),
      n_nonzero_edges,
      sparsity,
      round(mean(abs(nonzero_edges)), 3),
      round(max(abs(nonzero_edges)), 3)
    )
  )
  
  comparison$Difference <- comparison$T2 - comparison$T1
  
  print(comparison, row.names = FALSE)
  
} else {
  cat("T1 Netzwerk nicht gefunden - kein Vergleich möglich\n")
}

# ============================================
# NETZWERK SPEICHERN
# ============================================

saveRDS(network_T2, "results/network_T2.rds")
cat("\n✓ Netzwerk gespeichert: results/network_T2.rds\n")

# ============================================
# EINFACHE VISUALISIERUNG
# ============================================

cat("\n=== VISUALISIERUNG ERSTELLEN ===\n\n")

# Variablennamen kürzen für bessere Lesbarkeit
node_names <- colnames(daten_T2_complete)

# PDF erstellen
pdf("plots/network_T2_simple.pdf", width = 14, height = 14)

plot(network_T2, 
     layout = "spring",
     labels = node_names,
     label.cex = 0.8,           # Schriftgröße Labels
     label.scale = FALSE,
     edge.labels = FALSE,       # Keine Kantengewichte anzeigen (zu voll)
     title = "Netzwerk T2 (N = 2306)",
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ Einfache Visualisierung gespeichert: plots/network_T2_simple.pdf\n")

# Auch als PNG für schnelle Vorschau
png("plots/network_T2_simple.png", width = 2000, height = 2000, res = 150)

plot(network_T2, 
     layout = "spring",
     labels = node_names,
     label.cex = 0.8,
     label.scale = FALSE,
     edge.labels = FALSE,
     title = "Netzwerk T2 (N = 2306)",
     mar = c(3, 3, 3, 3))

dev.off()

cat("✓ PNG-Version gespeichert: plots/network_T2_simple.png\n")

# ============================================
# ZUSAMMENFASSUNG
# ============================================

cat("\n\n=== ZUSAMMENFASSUNG ===\n")
cat("✓ Netzwerk für T2 erfolgreich geschätzt\n")
cat("✓", n_nonzero_edges, "von", n_possible_edges, "möglichen Kanten sind nicht-null\n")
cat("✓ Sparsity:", sparsity, "%\n")
cat("✓ Dateien gespeichert in results/ und plots/\n\n")

cat("=== FERTIG ===\n")
