library(dplyr)

# Bootstrap-Kanten laden
boot_edges_t3 <- readRDS("results/boot_edges_t3.rds")
edge_ci <- summary(boot_edges_t3, statistics = "edge")

# --------------------------------------------------
# Funktion: Stressor ↔ ADNM-8 extrahieren
# --------------------------------------------------
extract_stressor_adnm <- function(stressor_var, label) {
  edge_ci %>%
    filter(
      (node1 == stressor_var & grepl("^ad\\.[1-8]\\.t3$", node2)) |
        (node2 == stressor_var & grepl("^ad\\.[1-8]\\.t3$", node1))
    ) %>%
    mutate(Stressor = label)
}

# --------------------------------------------------
# Stressor-Variablen
# --------------------------------------------------
bc1_adnm <- extract_stressor_adnm("bc.1.t3", "bc.1")
bc2_adnm <- extract_stressor_adnm("bc.2.t3", "bc.2")
bc5_adnm <- extract_stressor_adnm("bc.5.t3", "bc.5")

bh1_adnm <- extract_stressor_adnm("bh.1.t3", "bh.1")
bh6_adnm <- extract_stressor_adnm("bh.6.t3", "bh.6")
bh8_adnm <- extract_stressor_adnm("bh.8.t3", "bh.8")

bl1_adnm <- extract_stressor_adnm("bl.1.t3", "bl.1")
bl7_adnm <- extract_stressor_adnm("bl.7.t3", "bl.7")

bp1_adnm <- extract_stressor_adnm("bp.1.t3", "bp.1")
bp3_adnm <- extract_stressor_adnm("bp.3.t3", "bp.3")

bs4_adnm <- extract_stressor_adnm("bs.4.t3", "bs.4")
bs5_adnm <- extract_stressor_adnm("bs.5.t3", "bs.5")

bw4_adnm <- extract_stressor_adnm("bw.4.t3", "bw.4")
bw5_adnm <- extract_stressor_adnm("bw.5.t3", "bw.5")
bw7_adnm <- extract_stressor_adnm("bw.7.t3", "bw.7")

# --------------------------------------------------
# Prüfen: Anzahl der extrahierten Kanten pro Stressor
# --------------------------------------------------
sapply(list(
  bc1_adnm, bc2_adnm, bc5_adnm,
  bh1_adnm, bh6_adnm, bh8_adnm,
  bl1_adnm, bl7_adnm,
  bp1_adnm, bp3_adnm,
  bs4_adnm, bs5_adnm,
  bw4_adnm, bw5_adnm, bw7_adnm
), nrow)

# --------------------------------------------------
# Zusammenfassen: Mittelwerte & CIs
# --------------------------------------------------
stressor_summary <- bind_rows(
  bc1_adnm, bc2_adnm, bc5_adnm,
  bh1_adnm, bh6_adnm, bh8_adnm,
  bl1_adnm, bl7_adnm,
  bp1_adnm, bp3_adnm,
  bs4_adnm, bs5_adnm,
  bw4_adnm, bw5_adnm, bw7_adnm
) %>%
  group_by(Stressor) %>%
  summarise(
    mean_edge = mean(mean),
    ci_lower  = mean(q2.5),
    ci_upper  = mean(q97.5),
    .groups = "drop"
  )

stressor_summary

# --------------------------------------------------
# CSV speichern (alle Einzelkanten)
# --------------------------------------------------
write.csv(
  bind_rows(
    bc1_adnm, bc2_adnm, bc5_adnm,
    bh1_adnm, bh6_adnm, bh8_adnm,
    bl1_adnm, bl7_adnm,
    bp1_adnm, bp3_adnm,
    bs4_adnm, bs5_adnm,
    bw4_adnm, bw5_adnm, bw7_adnm
  ),
  "stressor_adnm_edge_cis_t3.csv",
  row.names = FALSE
)
