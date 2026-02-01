# ============================================
# MASTERARBEIT: Personen-basierte Netzwerke
# Skript 11: Symptom-Netzwerke nach Coping-Profil (H4b)
# ============================================

library(qgraph)
library(bootnet)
library(tidyverse)
library(NetworkComparisonTest)
library(igraph)
library(cluster)

# Daten laden
daten_T1_complete <- readRDS("data/daten_T1_complete.rds")
var_lists <- readRDS("data/variable_lists.rds")

cat("=== H4b: SYMPTOM-NETZWERKE NACH COPING-PROFIL ===\n\n")

# ============================================
# WICHTIG: Daten-Typen korrigieren
# ============================================

cat("--- DATEN BEREINIGEN ---\n\n")

# Alle Variablen zu numerisch konvertieren (für sicheres Arbeiten)
daten_T1_complete <- daten_T1_complete %>%
  mutate(across(everything(), ~ {
    if(is.factor(.x) | inherits(.x, "haven_labelled")) {
      as.numeric(as.character(.x))
    } else {
      as.numeric(.x)
    }
  }))

cat("✓ Alle Variablen in numerisches Format konvertiert\n\n")

# Prüfen
cat("Datentypen nach Konversion:\n")
cat("  ADNM-Variablen:", class(daten_T1_complete[[var_lists$adnm_t1[1]]]), "\n")
cat("  Coping-Variablen:", class(daten_T1_complete[[var_lists$cope_t1[1]]]), "\n\n")

# ============================================
# LABELS DEFINIEREN (für spätere Verwendung)
# ============================================

adnm_labels <- paste0("ADNM_", 1:8)
coping_labels <- c("Acceptance", "Active Coping", "Behavioral Disengagement",
                   "Denial", "Self-Distraction", "Emotional Support",
                   "Humor", "Instrumental Support", "Planning",
                   "Positive Reframing", "Religion", "Self-Blame",
                   "Substance Use", "Venting")

cat("✓ Labels definiert\n\n")

# ============================================
# SCHRITT 1: Coping-Profile erstellen
# ============================================

cat("--- SCHRITT 1: COPING-PROFILE DEFINIEREN ---\n\n")

# Coping-Daten extrahieren
coping_data <- daten_T1_complete[, var_lists$cope_t1]

# Sicherstellen dass alles numerisch ist
coping_data <- as.data.frame(lapply(coping_data, as.numeric))

# Definiere adaptive vs. maladaptive Strategien
adaptive_strategies <- c(
  "bcope.posr.t1",   # Positive Reframing
  "bcope.acc.t1",    # Acceptance
  "bcope.act.t1",    # Active Coping
  "bcope.pl.t1",     # Planning
  "bcope.esupp.t1",  # Emotional Support
  "bcope.isupp.t1",  # Instrumental Support
  "bcope.rel.t1",    # Religion
  "bcope.hu.t1"      # Humor
)

maladaptive_strategies <- c(
  "bcope.behd.t1",   # Behavioral Disengagement
  "bcope.den.t1",    # Denial
  "bcope.sub.t1",    # Substance Use
  "bcope.sbl.t1",    # Self-Blame
  "bcope.ven.t1",    # Venting
  "bcope.dist.t1"    # Self-Distraction
)

# Prüfe ob alle Strategien existieren
cat("Prüfe Coping-Strategien:\n")
cat("  Adaptive Strategien gefunden:", 
    sum(adaptive_strategies %in% names(daten_T1_complete)), "von", 
    length(adaptive_strategies), "\n")
cat("  Maladaptive Strategien gefunden:", 
    sum(maladaptive_strategies %in% names(daten_T1_complete)), "von", 
    length(maladaptive_strategies), "\n\n")

# Berechne Summenscores
daten_T1_complete$adaptive_score <- rowMeans(
  daten_T1_complete[, adaptive_strategies], 
  na.rm = TRUE
)

daten_T1_complete$maladaptive_score <- rowMeans(
  daten_T1_complete[, maladaptive_strategies], 
  na.rm = TRUE
)

# Prüfe ob Scores berechnet wurden
cat("Adaptive Score - Range:", 
    round(min(daten_T1_complete$adaptive_score, na.rm = TRUE), 2), "bis",
    round(max(daten_T1_complete$adaptive_score, na.rm = TRUE), 2), "\n")
cat("Maladaptive Score - Range:", 
    round(min(daten_T1_complete$maladaptive_score, na.rm = TRUE), 2), "bis",
    round(max(daten_T1_complete$maladaptive_score, na.rm = TRUE), 2), "\n\n")

# Ratio: Adaptiv vs. Maladaptiv
daten_T1_complete$coping_ratio <- 
  daten_T1_complete$adaptive_score / 
  (daten_T1_complete$maladaptive_score + 0.01)

# Entferne Inf/NaN Werte
daten_T1_complete$coping_ratio[is.infinite(daten_T1_complete$coping_ratio)] <- NA
daten_T1_complete$coping_ratio[is.nan(daten_T1_complete$coping_ratio)] <- NA

# Entferne Personen mit fehlenden Ratio-Werten
daten_T1_complete <- daten_T1_complete %>%
  filter(!is.na(coping_ratio))

cat("Nach Bereinigung: n =", nrow(daten_T1_complete), "\n\n")

# Visualisierung der Verteilung
pdf("plots/coping_ratio_distribution.pdf", width = 10, height = 6)

hist(daten_T1_complete$coping_ratio,
     breaks = 50,
     main = "Distribution of Coping Ratio (Adaptive/Maladaptive)",
     xlab = "Coping Ratio (higher = more adaptive)",
     col = "#3498DB",
     border = "white")

abline(v = median(daten_T1_complete$coping_ratio, na.rm = TRUE), 
       col = "#E74C3C", lwd = 2, lty = 2)

legend("topright", 
       legend = c(paste("Median =", 
                        round(median(daten_T1_complete$coping_ratio, na.rm = TRUE), 2))),
       col = "#E74C3C", lty = 2, lwd = 2)

dev.off()

cat("✓ Verteilung gespeichert: plots/coping_ratio_distribution.pdf\n\n")

# ============================================
# SCHRITT 2: Gruppen definieren
# ============================================

cat("--- SCHRITT 2: GRUPPEN BILDEN ---\n\n")

# Methode 1: Median-Split
median_ratio <- median(daten_T1_complete$coping_ratio)

daten_T1_complete$coping_group_median <- ifelse(
  daten_T1_complete$coping_ratio > median_ratio,
  "Adaptive",
  "Maladaptive"
)

cat("Methode 1 - Median-Split:\n")
cat("  Adaptive Gruppe: n =", sum(daten_T1_complete$coping_group_median == "Adaptive"), "\n")
cat("  Maladaptive Gruppe: n =", sum(daten_T1_complete$coping_group_median == "Maladaptive"), "\n\n")

# Methode 2: Tertile (extremere Gruppen)
tertiles <- quantile(daten_T1_complete$coping_ratio, probs = c(1/3, 2/3))

daten_T1_complete$coping_group_tertile <- case_when(
  daten_T1_complete$coping_ratio <= tertiles[1] ~ "Maladaptive",
  daten_T1_complete$coping_ratio >= tertiles[2] ~ "Adaptive",
  TRUE ~ "Mixed"
)

cat("Methode 2 - Tertile (extreme Gruppen):\n")
cat("  Adaptive Gruppe: n =", sum(daten_T1_complete$coping_group_tertile == "Adaptive"), "\n")
cat("  Mixed Gruppe: n =", sum(daten_T1_complete$coping_group_tertile == "Mixed"), "\n")
cat("  Maladaptive Gruppe: n =", sum(daten_T1_complete$coping_group_tertile == "Maladaptive"), "\n\n")

# Methode 3: K-Means Clustering auf Coping-Profilen
cat("Methode 3 - K-Means Clustering:\n")

set.seed(123)
kmeans_result <- kmeans(coping_data, centers = 2, nstart = 25)

daten_T1_complete$coping_group_kmeans <- ifelse(
  kmeans_result$cluster == which.max(
    tapply(daten_T1_complete$adaptive_score, kmeans_result$cluster, mean)
  ),
  "Adaptive",
  "Maladaptive"
)

cat("  Adaptive Gruppe: n =", sum(daten_T1_complete$coping_group_kmeans == "Adaptive"), "\n")
cat("  Maladaptive Gruppe: n =", sum(daten_T1_complete$coping_group_kmeans == "Maladaptive"), "\n\n")

# Wähle Hauptmethode (Tertile für stärkeren Kontrast)
cat("Hauptanalyse: Tertile-Methode (stärkster Kontrast)\n\n")

# ============================================
# SCHRITT 3: Symptom-Netzwerke pro Gruppe
# ============================================

cat("--- SCHRITT 3: SYMPTOM-NETZWERKE SCHÄTZEN ---\n\n")

# Nur ADNM-Daten
adnm_data <- daten_T1_complete[, var_lists$adnm_t1]

# Sicherstellen dass alles numerisch ist
adnm_data <- as.data.frame(lapply(adnm_data, as.numeric))

cat("ADNM Daten-Check:\n")
cat("  Dimensionen:", nrow(adnm_data), "×", ncol(adnm_data), "\n")
cat("  Datentyp:", class(adnm_data[[1]]), "\n")
cat("  Beispiel-Werte:", paste(head(adnm_data[[1]], 5), collapse = ", "), "\n\n")

# Gruppen-Daten (Tertile)
adaptive_group <- daten_T1_complete %>%
  filter(coping_group_tertile == "Adaptive")

maladaptive_group <- daten_T1_complete %>%
  filter(coping_group_tertile == "Maladaptive")

cat("Gruppen-Größen:\n")
cat("  Adaptive:", nrow(adaptive_group), "\n")
cat("  Maladaptive:", nrow(maladaptive_group), "\n\n")

# ADNM-Daten pro Gruppe (WICHTIG: Als numerische Matrix!)
adnm_adaptive <- as.data.frame(lapply(
  adaptive_group[, var_lists$adnm_t1], 
  as.numeric
))

adnm_maladaptive <- as.data.frame(lapply(
  maladaptive_group[, var_lists$adnm_t1],
  as.numeric
))

# Finale Checks
cat("Finale Daten-Checks:\n")
cat("  Adaptive - Klasse:", class(adnm_adaptive), "\n")
cat("  Adaptive - Dimensionen:", nrow(adnm_adaptive), "×", ncol(adnm_adaptive), "\n")
cat("  Maladaptive - Klasse:", class(adnm_maladaptive), "\n")
cat("  Maladaptive - Dimensionen:", nrow(adnm_maladaptive), "×", ncol(adnm_maladaptive), "\n\n")

# Prüfe auf fehlende Werte
cat("Fehlende Werte:\n")
cat("  Adaptive:", sum(is.na(adnm_adaptive)), "\n")
cat("  Maladaptive:", sum(is.na(adnm_maladaptive)), "\n\n")

cat("Schätze Netzwerke...\n")

# Netzwerk 1: Adaptive Gruppe
network_adaptive <- estimateNetwork(
  adnm_adaptive,
  default = "EBICglasso",
  corMethod = "cor_auto"
)

cat("✓ Adaptives Netzwerk: n =", nrow(adnm_adaptive), "\n")

# Netzwerk 2: Maladaptive Gruppe
network_maladaptive <- estimateNetwork(
  adnm_maladaptive,
  default = "EBICglasso",
  corMethod = "cor_auto"
)

cat("✓ Maladaptives Netzwerk: n =", nrow(adnm_maladaptive), "\n\n")

# Netzwerke speichern
saveRDS(network_adaptive, "results/network_adaptive_group.rds")
saveRDS(network_maladaptive, "results/network_maladaptive_group.rds")

cat("✓ Netzwerke gespeichert\n\n")

# ============================================
# SCHRITT 4: Netzwerk-Eigenschaften vergleichen
# ============================================

cat("--- SCHRITT 4: NETZWERK-EIGENSCHAFTEN ---\n\n")

# Funktion: Netzwerk-Metriken berechnen
calculate_network_metrics <- function(network, label) {
  
  edge_matrix <- network$graph
  edges <- edge_matrix[upper.tri(edge_matrix)]
  edges_nonzero <- edges[edges != 0]
  
  # Globale Eigenschaften
  n_nodes <- ncol(edge_matrix)
  n_possible_edges <- (n_nodes * (n_nodes - 1)) / 2
  n_nonzero_edges <- length(edges_nonzero)
  sparsity <- mean(edges == 0)
  
  # Stärke-Maße
  global_strength <- sum(abs(edges_nonzero))
  mean_edge_weight <- mean(abs(edges_nonzero))
  max_edge_weight <- max(abs(edges_nonzero))
  
  # Dichte (Average Edge Weight über alle möglichen Kanten)
  density <- global_strength / n_possible_edges
  
  # Zentralität
  cent <- centrality(network)
  mean_strength <- mean(cent$OutDegree)
  sd_strength <- sd(cent$OutDegree)
  
  # Clustering-Koeffizient (über igraph)
  library(igraph)
  g <- graph_from_adjacency_matrix(abs(edge_matrix), 
                                   mode = "undirected", 
                                   weighted = TRUE, 
                                   diag = FALSE)
  clustering_coef <- transitivity(g, type = "weighted")
  
  metrics <- data.frame(
    Group = label,
    N_Nodes = n_nodes,
    N_Edges = n_nonzero_edges,
    Sparsity = round(sparsity, 3),
    Global_Strength = round(global_strength, 3),
    Mean_Edge_Weight = round(mean_edge_weight, 3),
    Max_Edge_Weight = round(max_edge_weight, 3),
    Network_Density = round(density, 3),
    Mean_Node_Strength = round(mean_strength, 3),
    SD_Node_Strength = round(sd_strength, 3),
    Clustering_Coefficient = round(clustering_coef, 3)
  )
  
  return(metrics)
}

# Metriken berechnen
metrics_adaptive <- calculate_network_metrics(network_adaptive, "Adaptive")
metrics_maladaptive <- calculate_network_metrics(network_maladaptive, "Maladaptive")

# Kombinieren
network_comparison <- rbind(metrics_adaptive, metrics_maladaptive)

cat("NETZWERK-EIGENSCHAFTEN:\n\n")
print(t(network_comparison), quote = FALSE)

cat("\n")

# H4b-relevante Interpretation
cat("--- H4b INTERPRETATION ---\n\n")

# Global Strength als Skalar extrahieren (sauber & robust)
gs_adaptive    <- as.numeric(metrics_adaptive$Global_Strength[1])
gs_maladaptive <- as.numeric(metrics_maladaptive$Global_Strength[1])

if (gs_adaptive < gs_maladaptive) {
  cat("✓ HYPOTHESE UNTERSTÜTZT!\n")
  cat("  Adaptive Gruppe hat SCHWÄCHERES Symptom-Netzwerk\n")
  cat("  Global Strength: Adaptive (", gs_adaptive,
      ") < Maladaptive (", gs_maladaptive, ")\n\n")
} else {
  cat("✗ HYPOTHESE NICHT UNTERSTÜTZT\n")
  cat("  Adaptive Gruppe hat NICHT schwächeres Symptom-Netzwerk\n")
  cat("  Global Strength: Adaptive (", gs_adaptive,
      ") ≥ Maladaptive (", gs_maladaptive, ")\n\n")
}

# Weitere Metriken
cat("Weitere Befunde:\n")
cat("  Network Density: Adaptive =", metrics_adaptive$Network_Density,
    ", Maladaptive =", metrics_maladaptive$Network_Density, "\n")
cat("  Mean Edge Weight: Adaptive =", metrics_adaptive$Mean_Edge_Weight,
    ", Maladaptive =", metrics_maladaptive$Mean_Edge_Weight, "\n")
cat("  Sparsity: Adaptive =", metrics_adaptive$Sparsity,
    ", Maladaptive =", metrics_maladaptive$Sparsity, "\n\n")

# ============================================
# SCHRITT 5: Statistische Tests
# ============================================

cat("--- SCHRITT 5: STATISTISCHE TESTS ---\n\n")

cat("Network Comparison Test (NCT)...\n")
cat("(Dies kann 10-15 Minuten dauern!)\n\n")

# Prüfe ob Paket geladen ist
if(!require(NetworkComparisonTest)) {
  install.packages("NetworkComparisonTest")
  library(NetworkComparisonTest)
}

# NCT durchführen
nct_result <- NCT(
  network_adaptive$graph,
  network_maladaptive$graph,
  it = 1000,
  binary.data = FALSE,
  paired = FALSE,
  weighted = TRUE,
  test.edges = TRUE,
  edges = "all",
  test.centrality = TRUE,
  centrality = c("strength"),
  progressbar = TRUE
)

cat("✓ NCT abgeschlossen\n\n")

# NCT speichern
saveRDS(nct_result, "results/nct_coping_groups.rds")
cat("✓ NCT-Objekt gespeichert\n\n")

# === ROBUSTE ERGEBNIS-EXTRAKTION ===

cat("NCT ERGEBNISSE:\n\n")

# 1. Global Strength Invariance Test
cat("1. GLOBAL STRENGTH VERGLEICH:\n")

# Verschiedene mögliche Strukturen abfangen
if(!is.null(nct_result$glstrinv.real)) {
  
  cat("   Beobachteter Unterschied:", round(nct_result$glstrinv.real, 3), "\n")
  cat("   p-Wert:", round(nct_result$glstrinv.pval, 3), "\n")
  
  if(nct_result$glstrinv.pval < 0.05) {
    cat("   → SIGNIFIKANTER Unterschied! ***\n\n")
  } else if(nct_result$glstrinv.pval < 0.10) {
    cat("   → Marginal signifikant (p < .10)\n\n")
  } else {
    cat("   → Kein signifikanter Unterschied\n\n")
  }
  
} else {
  cat("   Global Strength Test nicht verfügbar\n")
  cat("   Berechne manuell...\n")
  
  # Manuelle Berechnung
  gs_adaptive <- sum(abs(network_adaptive$graph[upper.tri(network_adaptive$graph)]))
  gs_maladaptive <- sum(abs(network_maladaptive$graph[upper.tri(network_maladaptive$graph)]))
  
  cat("   Global Strength Adaptive:", round(gs_adaptive, 3), "\n")
  cat("   Global Strength Maladaptive:", round(gs_maladaptive, 3), "\n")
  cat("   Differenz:", round(gs_maladaptive - gs_adaptive, 3), "\n\n")
}

# 2. Network Structure Invariance Test
cat("2. NETZWERK-STRUKTUR INVARIANZ:\n")

if(!is.null(nct_result$nwinv.real)) {
  
  cat("   Test-Statistik:", round(nct_result$nwinv.real, 3), "\n")
  cat("   p-Wert:", round(nct_result$nwinv.pval, 3), "\n")
  
  if(nct_result$nwinv.pval < 0.05) {
    cat("   → Netzwerke sind SIGNIFIKANT UNTERSCHIEDLICH\n\n")
  } else {
    cat("   → Netzwerke sind strukturell ähnlich\n\n")
  }
  
} else {
  cat("   Struktur-Invarianz-Test nicht verfügbar\n\n")
}

# 3. Edge-Level Differences
cat("3. KANTEN-UNTERSCHIEDE:\n")

if(!is.null(nct_result$einv.pvals)) {
  
  # Zähle signifikante Kanten
  sig_edges_matrix <- nct_result$einv.pvals < 0.05
  sig_edges_matrix[is.na(sig_edges_matrix)] <- FALSE
  n_sig_edges <- sum(sig_edges_matrix[upper.tri(sig_edges_matrix)])
  
  cat("   Anzahl signifikant unterschiedlicher Kanten:", n_sig_edges, "\n")
  
  if(n_sig_edges > 0) {
    
    # Finde welche Kanten
    sig_indices <- which(sig_edges_matrix & upper.tri(sig_edges_matrix), arr.ind = TRUE)
    
    if(nrow(sig_indices) <= 10) {
      cat("\n   Signifikant unterschiedliche Kanten:\n")
      for(i in 1:nrow(sig_indices)) {
        row <- sig_indices[i, 1]
        col <- sig_indices[i, 2]
        
        # Kantengewichte
        edge_adaptive <- network_adaptive$graph[row, col]
        edge_maladaptive <- network_maladaptive$graph[row, col]
        
        cat("     ", adnm_labels[row], "--", adnm_labels[col], "\n")
        cat("        Adaptive:", round(edge_adaptive, 3), 
            ", Maladaptive:", round(edge_maladaptive, 3),
            ", p =", round(nct_result$einv.pvals[row, col], 3), "\n")
      }
    } else {
      cat("   (Zu viele zum einzeln auflisten - siehe Visualisierung)\n")
    }
  } else {
    cat("   (Keine signifikant unterschiedlichen Kanten)\n")
  }
  cat("\n")
  
} else {
  cat("   Kanten-Test nicht verfügbar\n\n")
}

# 4. Centrality Differences
cat("4. ZENTRALITÄTS-UNTERSCHIEDE:\n")

if(!is.null(nct_result$diffcen.pval)) {
  
  sig_nodes <- which(nct_result$diffcen.pval < 0.05)
  n_sig_nodes <- length(sig_nodes)
  
  cat("   Anzahl Knoten mit signifikant unterschiedlicher Strength:", n_sig_nodes, "\n")
  
  if(n_sig_nodes > 0) {
    cat("\n   Signifikant unterschiedliche Knoten:\n")
    
    cent_adaptive <- centrality(network_adaptive)
    cent_maladaptive <- centrality(network_maladaptive)
    
    for(node in sig_nodes) {
      cat("     ", adnm_labels[node], "\n")
      cat("        Adaptive:", round(cent_adaptive$OutDegree[node], 3),
          ", Maladaptive:", round(cent_maladaptive$OutDegree[node], 3),
          ", p =", round(nct_result$diffcen.pval[node], 3), "\n")
    }
  } else {
    cat("   (Keine signifikant unterschiedlichen Knoten)\n")
  }
  cat("\n")
  
} else {
  cat("   Zentralitäts-Test nicht verfügbar\n\n")
}

# === ZUSAMMENFASSUNG FÜR H4b ===

cat("\n=== H4b INTERPRETATION ===\n\n")

# Berechne Global Strength manuell
gs_adaptive <- sum(abs(network_adaptive$graph[upper.tri(network_adaptive$graph)]))
gs_maladaptive <- sum(abs(network_maladaptive$graph[upper.tri(network_maladaptive$graph)]))

cat("Global Strength:\n")
cat("  Adaptive Gruppe:", round(gs_adaptive, 3), "\n")
cat("  Maladaptive Gruppe:", round(gs_maladaptive, 3), "\n")
cat("  Differenz:", round(gs_maladaptive - gs_adaptive, 3), "\n")

# Extrahiere p-Wert sicher
if(!is.null(nct_result$glstrinv.pval)) {
  
  # Extrahiere als einzelnen Wert
  p_val <- as.numeric(nct_result$glstrinv.pval)[1]
  
  cat("  p-Wert:", round(p_val, 3), "\n\n")
  
  # Erstelle einzelne logische Werte (WICHTIG: [1] am Ende!)
  adaptive_is_weaker <- as.logical(gs_adaptive < gs_maladaptive)[1]
  is_significant <- as.logical(p_val < 0.05)[1]
  is_marginal <- as.logical(p_val < 0.10)[1]
  
  # Debugging-Output
  cat("DEBUG:\n")
  cat("  adaptive_is_weaker:", adaptive_is_weaker, "(class:", class(adaptive_is_weaker), ")\n")
  cat("  is_significant:", is_significant, "(class:", class(is_significant), ")\n")
  cat("  is_marginal:", is_marginal, "(class:", class(is_marginal), ")\n\n")
  
  # Entscheidungslogik mit expliziten Checks
  if(isTRUE(adaptive_is_weaker) && isTRUE(is_significant)) {
    cat("✓✓ H4b VOLL UNTERSTÜTZT!\n")
    cat("   Adaptive Gruppe hat signifikant schwächeres Symptom-Netzwerk\n\n")
    
  } else if(isTRUE(adaptive_is_weaker) && isTRUE(is_marginal)) {
    cat("✓ H4b TEILWEISE UNTERSTÜTZT\n")
    cat("   Adaptive Gruppe hat schwächeres Netzwerk (marginal signifikant, p < .10)\n\n")
    
  } else if(isTRUE(adaptive_is_weaker)) {
    cat("~ H4b TREND\n")
    cat("   Adaptive Gruppe hat schwächeres Netzwerk (nicht signifikant)\n\n")
    
  } else {
    cat("✗ H4b NICHT UNTERSTÜTZT\n")
    cat("   Adaptive Gruppe hat NICHT schwächeres Netzwerk\n\n")
  }
  
} else {
  cat("  p-Wert: nicht verfügbar\n\n")
  
  adaptive_is_weaker <- as.logical(gs_adaptive < gs_maladaptive)[1]
  
  if(isTRUE(adaptive_is_weaker)) {
    cat("~ H4b DESKRIPTIV UNTERSTÜTZT\n")
    cat("   Adaptive Gruppe hat schwächeres Netzwerk (Signifikanz nicht testbar)\n\n")
  } else {
    cat("✗ H4b NICHT UNTERSTÜTZT\n")
    cat("   Adaptive Gruppe hat NICHT schwächeres Netzwerk\n\n")
  }
}

cat("=== NCT ABGESCHLOSSEN ===\n\n")

# ============================================
# SCHRITT 6: Visualisierungen
# ============================================

cat("--- SCHRITT 6: VISUALISIERUNGEN ---\n\n")

# Labels für ADNM-Items
adnm_labels <- paste0("ADNM_", 1:8)

# Vergleichs-Plot: Beide Netzwerke nebeneinander
pdf("plots/symptom_networks_comparison.pdf", width = 16, height = 8)

par(mfrow = c(1, 2))

# Adaptive Gruppe
plot(network_adaptive,
     layout = "spring",
     labels = adnm_labels,
     title = paste0("Adaptive Coping Group (n = ", nrow(adnm_adaptive), ")"),
     color = "#3498DB",
     vsize = 10,
     label.cex = 1.2,
     maximum = 0.5)  # Gleiche Skala für Vergleichbarkeit

# Maladaptive Gruppe
plot(network_maladaptive,
     layout = "spring",
     labels = adnm_labels,
     title = paste0("Maladaptive Coping Group (n = ", nrow(adnm_maladaptive), ")"),
     color = "#E74C3C",
     vsize = 10,
     label.cex = 1.2,
     maximum = 0.5)

dev.off()

cat("✓ Netzwerk-Vergleich: plots/symptom_networks_comparison.pdf\n")

# Zentralitäts-Vergleich
pdf("plots/centrality_comparison_groups.pdf", width = 12, height = 8)

cent_adaptive <- centrality(network_adaptive)
cent_maladaptive <- centrality(network_maladaptive)

par(mfrow = c(1, 2))

# Strength Vergleich
barplot(cent_adaptive$OutDegree,
        names.arg = adnm_labels,
        main = "Strength - Adaptive Group",
        ylab = "Strength",
        col = "#3498DB",
        las = 2)

barplot(cent_maladaptive$OutDegree,
        names.arg = adnm_labels,
        main = "Strength - Maladaptive Group",
        ylab = "Strength",
        col = "#E74C3C",
        las = 2)

dev.off()

cat("✓ Zentralitäts-Vergleich: plots/centrality_comparison_groups.pdf\n")

# Edge-Weight-Verteilungen
pdf("plots/edge_weight_distributions.pdf", width = 10, height = 6)

edges_adaptive <- network_adaptive$graph[upper.tri(network_adaptive$graph)]
edges_adaptive_nonzero <- abs(edges_adaptive[edges_adaptive != 0])

edges_maladaptive <- network_maladaptive$graph[upper.tri(network_maladaptive$graph)]
edges_maladaptive_nonzero <- abs(edges_maladaptive[edges_maladaptive != 0])

# Histogramme
par(mfrow = c(1, 2))

hist(edges_adaptive_nonzero,
     breaks = 20,
     main = "Edge Weights - Adaptive",
     xlab = "Absolute Edge Weight",
     col = "#3498DB",
     border = "white")

hist(edges_maladaptive_nonzero,
     breaks = 20,
     main = "Edge Weights - Maladaptive",
     xlab = "Absolute Edge Weight",
     col = "#E74C3C",
     border = "white")

dev.off()

cat("✓ Edge-Verteilungen: plots/edge_weight_distributions.pdf\n")

# ============================================
# SCHRITT 7: Ergebnisse zusammenfassen
# ============================================

cat("\n--- SCHRITT 7: ZUSAMMENFASSUNG ---\n\n")

# Tabelle erstellen
summary_table <- network_comparison %>%
  mutate(
    Difference = case_when(
      Group == "Adaptive" ~ "-",
      TRUE ~ paste0(
        round(Global_Strength - metrics_adaptive$Global_Strength, 3),
        " (", 
        round(100 * (Global_Strength - metrics_adaptive$Global_Strength) / 
                metrics_adaptive$Global_Strength, 1),
        "%)"
      )
    )
  )

write.csv(network_comparison, "results/network_comparison_coping_groups.csv", row.names = FALSE)
cat("✓ Vergleichs-Tabelle: results/network_comparison_coping_groups.csv\n")

# Gruppen-Charakteristika
group_characteristics <- daten_T1_complete %>%
  filter(coping_group_tertile %in% c("Adaptive", "Maladaptive")) %>%
  group_by(coping_group_tertile) %>%
  summarise(
    N = n(),
    Mean_Adaptive_Score = mean(adaptive_score, na.rm = TRUE),
    Mean_Maladaptive_Score = mean(maladaptive_score, na.rm = TRUE),
    Mean_Coping_Ratio = mean(coping_ratio, na.rm = TRUE),
    Mean_ADNM_Total = mean(rowMeans(across(all_of(var_lists$adnm_t1))), na.rm = TRUE)
  )

print(group_characteristics)

write.csv(group_characteristics, "results/coping_group_characteristics.csv", row.names = FALSE)
cat("✓ Gruppen-Charakteristika: results/coping_group_characteristics.csv\n")

cat("\n=== H4b ZUSAMMENFASSUNG ===\n\n")

cat("Anzahl Personen:\n")
cat("  Adaptive Coping: n =", nrow(adnm_adaptive), "\n")
cat("  Maladaptive Coping: n =", nrow(adnm_maladaptive), "\n\n")

# Sichere Extraktion aus Dataframes
if(exists("metrics_adaptive") && exists("metrics_maladaptive")) {
  gs_adaptive_final <- as.numeric(metrics_adaptive$Global_Strength)[1]
  gs_maladaptive_final <- as.numeric(metrics_maladaptive$Global_Strength)[1]
} else {
  # Fallback: Direkt aus Netzwerken berechnen
  gs_adaptive_final <- sum(abs(network_adaptive$graph[upper.tri(network_adaptive$graph)]))
  gs_maladaptive_final <- sum(abs(network_maladaptive$graph[upper.tri(network_maladaptive$graph)]))
}

cat("Symptom-Netzwerk Global Strength:\n")
cat("  Adaptive:", round(gs_adaptive_final, 3), "\n")
cat("  Maladaptive:", round(gs_maladaptive_final, 3), "\n")
cat("  Differenz:", round(gs_maladaptive_final - gs_adaptive_final, 3), "\n\n")

# p-Wert
if(exists("nct_result") && !is.null(nct_result$glstrinv.pval)) {
  p_val_final <- as.numeric(nct_result$glstrinv.pval)[1]
  cat("NCT p-Wert:", round(p_val_final, 3), "\n\n")
  
  # Einzelne Vergleiche mit isTRUE()
  adaptive_weaker <- as.logical(gs_adaptive_final < gs_maladaptive_final)[1]
  significant <- as.logical(p_val_final < 0.05)[1]
  marginal <- as.logical(p_val_final < 0.10)[1]
  
  if(isTRUE(adaptive_weaker) && isTRUE(significant)) {
    cat("✓✓ H4b VOLL UNTERSTÜTZT!\n")
    cat("   Adaptive Gruppe hat signifikant schwächeres Symptom-Netzwerk\n")
    
  } else if(isTRUE(adaptive_weaker) && isTRUE(marginal)) {
    cat("✓ H4b TEILWEISE UNTERSTÜTZT\n")
    cat("   Adaptive Gruppe hat schwächeres Netzwerk (marginal signifikant)\n")
    
  } else if(isTRUE(adaptive_weaker)) {
    cat("~ H4b TREND\n")
    cat("   Adaptive Gruppe hat schwächeres Netzwerk (nicht signifikant)\n")
    
  } else {
    cat("✗ H4b NICHT UNTERSTÜTZT\n")
  }
  
} else {
  cat("NCT p-Wert: nicht verfügbar\n\n")
  
  adaptive_weaker <- as.logical(gs_adaptive_final < gs_maladaptive_final)[1]
  
  if(isTRUE(adaptive_weaker)) {
    cat("~ H4b DESKRIPTIV UNTERSTÜTZT\n")
  } else {
    cat("✗ H4b NICHT UNTERSTÜTZT\n")
  }
}

cat("\n=== FERTIG ===\n")
