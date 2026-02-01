library(dplyr)

boot_edges_t2 <- readRDS("results/boot_edges_t2.rds")
edge_ci <- summary(boot_edges_t2, statistics = "edge")

# Alle Stressoren explizit als Vektor
stressors <- c(
  "bc.1.t2", "bc.2.t2", "bc.5.t2", "bh.1.t2", "bh.6.t2", "bh.8.t2",
  "bl.1.t2", "bl.7.t2", "bp.1.t2", "bp.3.t2", "bs.4.t2", "bs.5.t2",
  "bw.4.t2", "bw.5.t2", "bw.7.t2"
)

extract_coping_stressoror <- function(coping_var, label) {
  edge_ci %>%
    filter(
      (node1 == coping_var & node2 %in% stressors) |
        (node2 == coping_var & node1 %in% stressors)
    ) %>%
    mutate(Coping = label)
}

# ursprüngliche Coping-Variablen
humor_stress <- extract_coping_stressor("bcope.hu.t2", "Humor")
posr_stress  <- extract_coping_stressor("bcope.posr.t2", "Positive Reframing")
acc_stress   <- extract_coping_stressor("bcope.acc.t2", "Acceptance")

# neue Coping-Variablen
esupp_stress <- extract_coping_stressor("bcope.esupp.t2", "Emotional Support")
act_stress   <- extract_coping_stressor("bcope.act.t2", "Active Coping")
ven_stress   <- extract_coping_stressor("bcope.ven.t2", "Venting")
rel_stress   <- extract_coping_stressor("bcope.rel.t2", "Religion")
dist_stress  <- extract_coping_stressor("bcope.dist.t2", "Self Distraction")
isupp_stress <- extract_coping_stressor("bcope.isupp.t2", "Instrumental Support")
behd_stress <- extract_coping_stressor("bcope.behd.t2", "Behavioral Disengagement")
pl_stress   <- extract_coping_stressor("bcope.pl.t2", "Planning")
sub_stress   <- extract_coping_stressor("bcope.sub.t2", "Substance Use")
den_stress   <- extract_coping_stressor("bcope.den.t2", "Denial")
sbl_stress  <- extract_coping_stressor("bcope.sbl.t2", "Self Blame")

# Prüfen, wie viele Kanten pro Coping-Mechanismus extrahiert wurden
sapply(list(
  humor_stress, posr_stress, acc_stress,
  esupp_stress, act_stress, ven_stress, rel_stress, dist_stress,
  isupp_stress, behd_stress, pl_stress, sub_stress, den_stress, sbl_stress
), nrow)

# alle Daten zusammenführen und zusammenfassen
coping_summary <- bind_rows(
  humor_stress, posr_stress, acc_stress,
  esupp_stress, act_stress, ven_stress, rel_stress, dist_stress,
  isupp_stress, behd_stress, pl_stress, sub_stress, den_stress, sbl_stress
) %>%
  group_by(Coping) %>%
  summarise(
    mean_edge = mean(mean),
    ci_lower  = mean(q2.5),
    ci_upper  = mean(q97.5),
    .groups = "drop"
  )

coping_summary

# CSV speichern
write.csv(
  bind_rows(
    humor_stress, posr_stress, acc_stress,
    esupp_stress, act_stress, ven_stress, rel_stress, dist_stress,
    isupp_stress, behd_stress, pl_stress, sub_stress, den_stress, sbl_stress
  ),
  "coping_adnm_edge_cis_t2.csv",
  row.names = FALSE
)
