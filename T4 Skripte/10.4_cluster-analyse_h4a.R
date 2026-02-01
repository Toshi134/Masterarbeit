# ============================================
# MASTERARBEIT: Coping-Cluster-Analyse
# Skript 10.4: Funktionale Coping-Cluster (H4a)
# ============================================

library(qgraph)
library(igraph)
library(tidyverse)
library(cluster)
library(factoextra)
library(corrplot)

# Netzwerk und Daten laden
network_T4 <- readRDS("results/network_T4.rds")
var_lists <- readRDS("data/variable_lists.rds")
daten_T4_complete <- readRDS("data/daten_T4_complete.rds")

cat("=== COPING-CLUSTER-ANALYSE (H4a) ===\n\n")

# ============================================
# METHODE 1: Community Detection im Netzwerk
# ============================================

cat("--- METHODE 1: COMMUNITY DETECTION ---\n\n")

# Nur Coping-Strategien aus dem Netzwerk extrahieren
edge_matrix <- network_T4$graph

# Indices der Coping-Knoten
cope_indices <- which(colnames(edge_matrix) %in% var_lists$cope_t4)

# Sub-Netzwerk: Nur Coping-Strategien
coping_network_matrix <- edge_matrix[cope_indices, cope_indices]

# Konvertiere zu igraph-Objekt
# Absolutwerte nehmen (igraph braucht positive Gewichte)
coping_abs_matrix <- abs(coping_network_matrix)

# Erstelle gewichtetes igraph-Objekt
g_coping <- graph_from_adjacency_matrix(
  coping_abs_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Community Detection Algorithmen

# 1. Walktrap (empfohlen für psychologische Netzwerke)
cat("1. Walktrap Community Detection:\n")
communities_walktrap <- cluster_walktrap(g_coping, weights = E(g_coping)$weight)

cat("   Anzahl Communities:", length(communities_walktrap), "\n")
cat("   Modularity:", round(modularity(communities_walktrap), 3), "\n\n")

# Zuordnung anzeigen
walktrap_membership <- data.frame(
  Strategy = colnames(coping_network_matrix),
  Cluster = communities_walktrap$membership
) %>%
  mutate(
    Label = case_when(
      Strategy == "bcope.acc.t4" ~ "Acceptance",
      Strategy == "bcope.act.t4" ~ "Active Coping",
      Strategy == "bcope.behd.t4" ~ "Behavioral Disengagement",
      Strategy == "bcope.den.t4" ~ "Denial",
      Strategy == "bcope.dist.t4" ~ "Self-Distraction",
      Strategy == "bcope.esupp.t4" ~ "Emotional Support",
      Strategy == "bcope.hu.t4" ~ "Humor",
      Strategy == "bcope.isupp.t4" ~ "Instrumental Support",
      Strategy == "bcope.pl.t4" ~ "Planning",
      Strategy == "bcope.posr.t4" ~ "Positive Reframing",
      Strategy == "bcope.rel.t4" ~ "Religion",
      Strategy == "bcope.sbl.t4" ~ "Self-Blame",
      Strategy == "bcope.sub.t4" ~ "Substance Use",
      Strategy == "bcope.ven.t4" ~ "Venting",
      TRUE ~ Strategy
    )
  ) %>%
  arrange(Cluster, Label)

cat("   Cluster-Zuordnung:\n\n")
for(i in unique(walktrap_membership$Cluster)) {
  cat("   Cluster", i, ":\n")
  members <- walktrap_membership %>% filter(Cluster == i) %>% pull(Label)
  cat("     ", paste(members, collapse = ", "), "\n\n")
}

# 2. Louvain (alternative Methode)
cat("2. Louvain Community Detection:\n")
communities_louvain <- cluster_louvain(g_coping, weights = E(g_coping)$weight)

cat("   Anzahl Communities:", length(communities_louvain), "\n")
cat("   Modularity:", round(modularity(communities_louvain), 3), "\n\n")

# 3. Fast-Greedy
cat("3. Fast-Greedy Community Detection:\n")
communities_fastgreedy <- cluster_fast_greedy(g_coping, weights = E(g_coping)$weight)

cat("   Anzahl Communities:", length(communities_fastgreedy), "\n")
cat("   Modularity:", round(modularity(communities_fastgreedy), 3), "\n\n")

# Vergleich der Methoden
cat("Vergleich der Modularity-Werte:\n")
cat("  Walktrap:", round(modularity(communities_walktrap), 3), "\n")
cat("  Louvain:", round(modularity(communities_louvain), 3), "\n")
cat("  Fast-Greedy:", round(modularity(communities_fastgreedy), 3), "\n")
cat("\n  → Höhere Modularity = bessere Cluster-Trennung\n\n")

# Beste Methode wählen (höchste Modularity)
modularities <- c(
  walktrap = modularity(communities_walktrap),
  louvain = modularity(communities_louvain),
  fastgreedy = modularity(communities_fastgreedy)
)

best_method <- names(which.max(modularities))
cat("Beste Methode:", best_method, "\n\n")

# Verwende beste Methode für weitere Analysen
communities_final <- switch(best_method,
                            walktrap = communities_walktrap,
                            louvain = communities_louvain,
                            fastgreedy = communities_fastgreedy
)

# ============================================
# METHODE 2: Hierarchisches Clustering
# ============================================

cat("\n--- METHODE 2: HIERARCHISCHES CLUSTERING ---\n\n")

# Distanzmatrix aus Coping-Netzwerk
# Je stärker die Verbindung, desto geringer die Distanz
dist_matrix <- 1 - coping_abs_matrix
diag(dist_matrix) <- 0
dist_matrix[dist_matrix < 0] <- 0  # Negative Distanzen auf 0

# Hierarchisches Clustering
hc <- hclust(as.dist(dist_matrix), method = "ward.D2")

# Dendrogramm plotten
pdf("plots/coping_dendrogram.pdf", width = 12, height = 8)

plot(hc, 
     labels = walktrap_membership$Label,
     main = "Hierarchical Clustering of Coping Strategies",
     xlab = "",
     sub = "",
     cex = 0.8)

# Cluster einzeichnen (z.B. 3 Cluster)
rect.hclust(hc, k = 3, border = c("#E74C3C", "#3498DB", "#2ECC71"))

dev.off()

cat("✓ Dendrogramm gespeichert: plots/coping_dendrogram.pdf\n\n")

# Optimale Cluster-Anzahl finden
cat("Bestimme optimale Cluster-Anzahl...\n")

# Silhouette-Methode
fviz_nbclust(coping_abs_matrix, 
             FUN = hcut, 
             method = "silhouette",
             k.max = 8) -> sil_plot

pdf("plots/optimal_clusters_silhouette.pdf", width = 10, height = 6)
print(sil_plot)
dev.off()

cat("✓ Silhouette-Plot: plots/optimal_clusters_silhouette.pdf\n\n")

# ============================================
# METHODE 3: Korrelationsbasierte Cluster
# ============================================

cat("\n--- METHODE 3: KORRELATIONS-CLUSTER ---\n\n")

# Korrelationen zwischen Coping-Strategien (auf Itemebene)
coping_data <- daten_T4_complete[, var_lists$cope_t4]

cor_matrix <- cor(coping_data, use = "pairwise.complete.obs")

cat("Korrelationsmatrix erstellt.\n")

# Visualisierung
pdf("plots/coping_correlation_matrix.pdf", width = 10, height = 10)

corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",
         addrect = 3,  # 3 Cluster einzeichnen
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.7,
         col = colorRampPalette(c("#E74C3C", "white", "#3498DB"))(200))

dev.off()

cat("✓ Korrelationsmatrix: plots/coping_correlation_matrix.pdf\n\n")

# ============================================
# CLUSTER-CHARAKTERISIERUNG
# ============================================

cat("\n--- CLUSTER-CHARAKTERISIERUNG ---\n\n")

# Verwende Community Detection Ergebnisse
cluster_assignment <- data.frame(
  Strategy = colnames(coping_network_matrix),
  Cluster = communities_final$membership
)

# Merge mit Labels
cluster_assignment <- cluster_assignment %>%
  left_join(walktrap_membership %>% select(Strategy, Label), by = "Strategy")

# Für jedes Cluster: Charakteristika analysieren

cat("CLUSTER-PROFILE:\n\n")

for(i in unique(cluster_assignment$Cluster)) {
  
  cat("=== CLUSTER", i, "===\n")
  
  # Mitglieder
  members <- cluster_assignment %>% filter(Cluster == i)
  cat("Mitglieder (", nrow(members), "):\n")
  cat("  ", paste(members$Label, collapse = ", "), "\n\n")
  
  # Durchschnittliche interne Verbindungen
  member_vars <- members$Strategy
  member_indices <- which(colnames(coping_network_matrix) %in% member_vars)
  
  if(length(member_indices) > 1) {
    internal_edges <- coping_network_matrix[member_indices, member_indices]
    internal_edges <- internal_edges[upper.tri(internal_edges)]
    internal_edges_nonzero <- internal_edges[internal_edges != 0]
    
    if(length(internal_edges_nonzero) > 0) {
      cat("Interne Kohäsion:\n")
      cat("  Durchschnittliche Kantenstärke:", round(mean(abs(internal_edges_nonzero)), 3), "\n")
      cat("  Anzahl Verbindungen:", length(internal_edges_nonzero), "\n\n")
    }
  }
  
  # Verbindungen zu ADNM
  adnm_connections <- data.frame()
  for(strat in member_vars) {
    for(adnm_var in var_lists$adnm_t4) {
      weight <- edge_matrix[strat, adnm_var]
      if(weight != 0) {
        adnm_connections <- rbind(adnm_connections, data.frame(
          Strategy = strat,
          ADNM = adnm_var,
          Weight = weight
        ))
      }
    }
  }
  
  if(nrow(adnm_connections) > 0) {
    cat("Verbindung zu ADNM-Symptomen:\n")
    cat("  Durchschnittliches Gewicht:", round(mean(adnm_connections$Weight), 3), "\n")
    cat("  Anzahl Verbindungen:", nrow(adnm_connections), "\n")
    
    if(mean(adnm_connections$Weight) < 0) {
      cat("  → ADAPTIVES Cluster (negativ mit Symptomen)\n\n")
    } else if(mean(adnm_connections$Weight) > 0) {
      cat("  → MALADAPTIVES Cluster (positiv mit Symptomen)\n\n")
    } else {
      cat("  → NEUTRALES Cluster\n\n")
    }
  } else {
    cat("Keine direkten Verbindungen zu ADNM-Symptomen\n\n")
  }
}

# ============================================
# VISUALISIERUNG: Cluster im Netzwerk
# ============================================

cat("\n--- VISUALISIERUNG ---\n\n")

# Coping-Sub-Netzwerk mit Cluster-Farben
pdf("plots/coping_network_clusters.pdf", width = 12, height = 12)

# Farben für Cluster
cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")
node_colors <- cluster_colors[cluster_assignment$Cluster]

qgraph(coping_network_matrix,
       layout = "spring",
       labels = cluster_assignment$Label,
       groups = as.list(split(1:nrow(cluster_assignment), cluster_assignment$Cluster)),
       color = cluster_colors[1:length(unique(cluster_assignment$Cluster))],
       legend = TRUE,
       legend.cex = 0.5,
       vsize = 8,
       title = paste("Coping Strategy Clusters (", best_method, ")", sep = ""),
       edge.labels = FALSE,
       label.cex = 1.2)

dev.off()

cat("✓ Cluster-Netzwerk: plots/coping_network_clusters.pdf\n")

# ============================================
# STATISTISCHE TESTS
# ============================================

cat("\n\n--- STATISTISCHE VALIDIERUNG ---\n\n")

# Silhouette-Koeffizient für Community Detection
silhouette_scores <- silhouette(communities_final$membership, as.dist(dist_matrix))

cat("Silhouette-Analyse:\n")
cat("  Durchschnittlicher Silhouette-Koeffizient:", 
    round(mean(silhouette_scores[, 3]), 3), "\n")
cat("  Interpretation:\n")
cat("    > 0.7: Starke Cluster-Struktur\n")
cat("    0.5-0.7: Moderate Cluster-Struktur\n")
cat("    0.25-0.5: Schwache Cluster-Struktur\n")
cat("    < 0.25: Keine substantielle Struktur\n\n")

# Silhouette-Plot
pdf("plots/silhouette_plot.pdf", width = 10, height = 8)
plot(silhouette_scores, 
     col = cluster_colors[1:length(unique(cluster_assignment$Cluster))],
     border = NA,
     main = "Silhouette Plot - Coping Clusters")
dev.off()

cat("✓ Silhouette-Plot: plots/silhouette_plot.pdf\n")

# ============================================
# ERGEBNISSE SPEICHERN
# ============================================

cat("\n\n--- ERGEBNISSE SPEICHERN ---\n\n")

# Cluster-Zuordnung
cluster_results <- cluster_assignment %>%
  arrange(Cluster, Label)

write.csv(cluster_results, "results/coping_clusters.csv", row.names = FALSE)
cat("✓ Cluster-Zuordnung: results/coping_clusters.csv\n")

# Cluster-Statistiken
cluster_stats <- data.frame()

for(i in unique(cluster_assignment$Cluster)) {
  members <- cluster_assignment %>% filter(Cluster == i)
  member_vars <- members$Strategy
  
  # ADNM-Verbindungen
  adnm_connections <- data.frame()
  for(strat in member_vars) {
    for(adnm_var in var_lists$adnm_t4) {
      weight <- edge_matrix[strat, adnm_var]
      if(weight != 0) {
        adnm_connections <- rbind(adnm_connections, data.frame(Weight = weight))
      }
    }
  }
  
  cluster_stats <- rbind(cluster_stats, data.frame(
    Cluster = i,
    N_Strategies = nrow(members),
    Mean_ADNM_Weight = ifelse(nrow(adnm_connections) > 0, 
                              mean(adnm_connections$Weight), NA),
    N_ADNM_Connections = nrow(adnm_connections),
    Type = ifelse(nrow(adnm_connections) > 0,
                  ifelse(mean(adnm_connections$Weight) < 0, "Adaptive", "Maladaptive"),
                  "Unknown")
  ))
}

print(cluster_stats)

write.csv(cluster_stats, "results/cluster_statistics.csv", row.names = FALSE)
cat("✓ Cluster-Statistiken: results/cluster_statistics.csv\n")

# ============================================
# ZUSAMMENFASSUNG
# ============================================

cat("\n\n=== ZUSAMMENFASSUNG H4a ===\n\n")

cat("Anzahl identifizierter Cluster:", length(unique(cluster_assignment$Cluster)), "\n")
cat("Methode mit bester Modularity:", best_method, "\n")
cat("Modularity:", round(max(modularities), 3), "\n")
cat("Silhouette-Koeffizient:", round(mean(silhouette_scores[, 3]), 3), "\n\n")

cat("Cluster-Typen:\n")
print(table(cluster_stats$Type))

cat("\n✓ Alle Ergebnisse in results/ und plots/ gespeichert\n")
cat("\n=== FERTIG ===\n")
