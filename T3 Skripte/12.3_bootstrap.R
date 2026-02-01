# =============================================================================
# Bootstrap-Analyse für Netzwerk Messzeitpunkt 1
# Masterarbeit: ADNM-8 Symptome, Stressoren und Coping während der Pandemie
# =============================================================================

# Benötigte Pakete laden
library(bootnet)
library(qgraph)
library(igraph)
library(dplyr)

# Optional: Parallelisierung für schnellere Berechnung
library(parallel)
library(foreach)
library(doParallel)

# =============================================================================
# 1. DATEN LADEN UND VORBEREITEN
# =============================================================================

# Daten für Messzeitpunkt 1 einlesen (Pfad anpassen!)
daten_T3_complete <- readRDS("data/daten_T3_complete.rds")

# Spaltennamen überprüfen
colnames(daten_T3_complete)

# =============================================================================
# 2. NETZWERK LADEN (bereits geschätzt)
# =============================================================================

# Lade dein bereits geschätztes Netzwerk
network_T3 <- readRDS("results/network_T3.rds")

# =============================================================================
# 3. BOOTSTRAP FÜR KANTENSTABILITÄT (Edge Weights)
# =============================================================================

cat("\n=== Starte Nonparametric Bootstrap für Kantengewichte ===\n")
cat("Dies kann einige Minuten dauern...\n\n")

# Nonparametric Bootstrap (empfohlen: mindestens 1000, ideal: 2500)
set.seed(4140)  # Für Reproduzierbarkeit
boot_edges_t3 <- bootnet(network_T3,
                         nBoots = 2500,  # Anzahl Bootstrap-Samples
                         type = "nonparametric",
                         nCores = detectCores() - 1,  # Parallelisierung
                         statistics = c("edge", "strength", "closeness", 
                                        "betweenness", "expectedInfluence"))

# Ergebnisse speichern
saveRDS(boot_edges_t3, "boot_edges_t3.rds")

# Edge Weight Stability plotten
pdf("bootstrap_edges_t3.pdf", width = 12, height = 8)
plot(boot_edges_t3, 
     labels = TRUE, 
     order = "sample",
     plot = "interval")
dev.off()

# Differenztests zwischen Kanten
pdf("bootstrap_edge_differences_t3.pdf", width = 14, height = 10)
plot(boot_edges_t3, 
     plot = "difference",
     onlyNonZero = TRUE,  # Nur signifikante Kanten zeigen
     order = "sample")
dev.off()

# =============================================================================
# 4. CASE-DROPPING BOOTSTRAP FÜR ZENTRALITÄTSSTABILITÄT
# =============================================================================

cat("\n=== Starte Case-Dropping Bootstrap für Zentralitätsmaße ===\n")
cat("Dies kann länger dauern...\n\n")

# Case-dropping Bootstrap
set.seed(4140)
boot_case_t3 <- bootnet(network_T3,
                        nBoots = 2500,
                        type = "case",
                        nCores = detectCores() - 1,
                        statistics = c("strength", "closeness", 
                                       "betweenness", "expectedInfluence"),
                        caseMin = 0.05,  # Minimum 5% der Stichprobe behalten
                        caseMax = 0.95)  # Maximum 95% droppen

# Ergebnisse speichern
saveRDS(boot_case_t3, "boot_case_t3.rds")

# Stability Plot erstellen
pdf("case_dropping_stability_t3.pdf", width = 12, height = 8)
plot(boot_case_t3, 
     statistics = c("strength", "closeness", "betweenness", "expectedInfluence"))
dev.off()

# CS-Koeffizient berechnen (Correlation Stability Coefficient)
# Sollte idealerweise > 0.5 sein, mindestens > 0.25
cs_results_t3 <- corStability(boot_case_t3)
print("CS-Koeffizienten für Zentralitätsmaße:")
print(cs_results_t3)

# CS-Koeffizienten in Datei speichern
sink("cs_coefficients_t3.txt")
cat("=== Correlation Stability Coefficients (CS) ===\n")
cat("Messzeitpunkt 1\n\n")
print(cs_results_t3)
cat("\nInterpretation:\n")
cat("CS > 0.5: Sehr stabile Zentralitätsmaße\n")
cat("CS = 0.25-0.5: Akzeptable Stabilität\n")
cat("CS < 0.25: Geringe Stabilität, vorsichtige Interpretation nötig\n")
sink()

# =============================================================================
# 5. DIFFERENZTESTS FÜR ZENTRALITÄTSMASSE
# =============================================================================

cat("\n=== Erstelle Differenztests für Zentralitätsmaße ===\n")

# Differenztests für Zentralitätsmaße (mit nonparametric bootstrap)
pdf("centrality_differences_strength_t3.pdf", width = 14, height = 10)
plot(boot_edges_t3,  # ← Nutze boot_edges_t3 statt boot_case_t3!
     plot = "difference",
     statistics = "strength",
     order = "sample")
dev.off()

# =============================================================================
# 6. BOOTSTRAP FÜR NETZWERKSTRUKTUR (z.B. Clustering, Dichte)
# =============================================================================

cat("\n=== Berechne globale Netzwerkmaße mit Bootstrap ===\n")

# Funktion für globale Netzwerkmaße
compute_network_metrics <- function(data) {
  # Netzwerk schätzen
  net <- estimateNetwork(data, 
                         default = "EBICglasso",
                         corMethod = "cor_auto",
                         tuning = 0.5)
  
  # Gewichtsmatrix extrahieren
  weight_matrix <- getWmat(net)
  
  # In igraph-Objekt konvertieren
  g <- graph_from_adjacency_matrix(abs(weight_matrix), 
                                   mode = "undirected", 
                                   weighted = TRUE)
  
  # Globale Maße berechnen
  metrics <- c(
    density = edge_density(g),
    clustering = transitivity(g, type = "global"),
    avg_path_length = tryCatch(mean_distance(g), error = function(e) NA),
    modularity = tryCatch(modularity(cluster_walktrap(g)), error = function(e) NA),
    assortativity = tryCatch(assortativity_degree(g), error = function(e) NA)
  )
  
  return(metrics)
}

# Bootstrap für Netzwerkstruktur
set.seed(4140)
n_boots <- 1000
n <- nrow(daten_T3_complete)

# Initialisiere Ergebnismatrix
structural_boots <- matrix(NA, nrow = n_boots, ncol = 5)
colnames(structural_boots) <- c("density", "clustering", "avg_path_length", 
                                "modularity", "assortativity")

cat("Führe", n_boots, "Bootstrap-Iterationen für Netzwerkstruktur durch...\n")

# Bootstrap durchführen
for(i in 1:n_boots) {
  if(i %% 100 == 0) cat("Bootstrap", i, "von", n_boots, "\n")
  
  # Ziehe Bootstrap-Sample
  boot_indices <- sample(1:n, n, replace = TRUE)
  boot_data <- daten_T3_complete[boot_indices, ]
  
  # Berechne Metriken
  structural_boots[i, ] <- compute_network_metrics(boot_data)
}

# Originale Metriken berechnen
original_metrics <- compute_network_metrics(daten_T3_complete)

# Ergebnisse zusammenfassen
structural_summary <- data.frame(
  Metric = names(original_metrics),
  Original = original_metrics,
  Mean_Boot = colMeans(structural_boots, na.rm = TRUE),
  SD_Boot = apply(structural_boots, 2, sd, na.rm = TRUE),
  CI_Lower = apply(structural_boots, 2, quantile, 0.025, na.rm = TRUE),
  CI_Upper = apply(structural_boots, 2, quantile, 0.975, na.rm = TRUE)
)

print("Bootstrapped Netzwerkstruktur-Maße:")
print(structural_summary)

# Speichern
write.csv(structural_summary, "structural_metrics_bootstrap_t3.csv", row.names = FALSE)
saveRDS(structural_boots, "structural_boots_t3.rds")

# Visualisierung der Bootstrap-Verteilungen
pdf("structural_metrics_distributions_t3.pdf", width = 12, height = 8)
par(mfrow = c(2, 3))
for(i in 1:ncol(structural_boots)) {
  hist(structural_boots[, i], 
       main = colnames(structural_boots)[i],
       xlab = "Wert",
       col = "lightblue",
       border = "white")
  abline(v = original_metrics[i], col = "red", lwd = 2)
  abline(v = quantile(structural_boots[, i], c(0.025, 0.975), na.rm = TRUE), 
         col = "blue", lty = 2, lwd = 2)
}
dev.off()

# =============================================================================
# 7. ZUSAMMENFASSUNG UND BERICHT
# =============================================================================

cat("\n=== Erstelle Zusammenfassungsbericht ===\n")

sink("bootstrap_summary_t3.txt")
cat("=============================================================================\n")
cat("BOOTSTRAP-ANALYSE MESSZEITPUNKT 1\n")
cat("=============================================================================\n\n")

cat("Stichprobengröße:", nrow(daten_T3_complete), "\n")
cat("Anzahl Variablen:", ncol(daten_T3_complete), "\n")
cat("Bootstrap-Iterationen (Kanten):", boot_edges_t3$nBoots, "\n")
cat("Bootstrap-Iterationen (Case-Dropping):", boot_case_t3$nBoots, "\n")
cat("Bootstrap-Iterationen (Struktur):", n_boots, "\n\n")

cat("-----------------------------------------------------------------------------\n")
cat("1. KANTENSTABILITÄT\n")
cat("-----------------------------------------------------------------------------\n")
cat("Siehe Plots: bootstrap_edges_t3.pdf und bootstrap_edge_differences_t3.pdf\n\n")

cat("-----------------------------------------------------------------------------\n")
cat("2. ZENTRALITÄTSSTABILITÄT (CS-Koeffizienten)\n")
cat("-----------------------------------------------------------------------------\n")
print(cs_results_t3)
cat("\n")

cat("-----------------------------------------------------------------------------\n")
cat("3. NETZWERKSTRUKTUR\n")
cat("-----------------------------------------------------------------------------\n")
print(structural_summary)
cat("\n")

cat("=============================================================================\n")
cat("INTERPRETATION FÜR MASTERARBEIT\n")
cat("=============================================================================\n\n")

cat("Edge Weights:\n")
cat("- Bootstrapped Konfidenzintervalle zeigen die Präzision der Kantenschätzungen\n")
cat("- Schmale Intervalle = stabile Kanten\n")
cat("- Differenztests zeigen, welche Kanten signifikant unterschiedlich sind\n\n")

cat("Zentralitätsmaße:\n")
cat("- CS > 0.5: Sehr interpretierbar\n")
cat("- CS 0.25-0.5: Mit Vorsicht interpretieren\n")
cat("- CS < 0.25: Nicht stabil genug für Interpretation\n\n")

cat("Netzwerkstruktur:\n")
cat("- 95% Konfidenzintervalle für globale Maße\n")
cat("- Prüfe, ob CIs die erwarteten Werte einschließen\n\n")

sink()

cat("\n=== Bootstrap-Analyse abgeschlossen! ===\n")
cat("\nErstelte Dateien:\n")
cat("- boot_edges_t3.rds (Kantenstabilität)\n")
cat("- boot_case_t3.rds (Case-Dropping)\n")
cat("- structural_boots_t3.rds (Strukturelle Maße)\n")
cat("- Diverse PDF-Plots\n")
cat("- bootstrap_summary_t3.txt (Gesamtbericht)\n")
cat("- cs_coefficients_t3.txt (CS-Koeffizienten)\n")
cat("- structural_metrics_bootstrap_t3.csv (Strukturmaße)\n")
