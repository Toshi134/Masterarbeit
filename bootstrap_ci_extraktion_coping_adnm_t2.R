library(dplyr)

boot_edges_t2 <- readRDS("results/boot_edges_t2.rds")
edge_ci <- summary(boot_edges_t2, statistics = "edge")

extract_coping_adnm <- function(coping_var, label) {
  edge_ci %>%
    filter(
      (node1 == coping_var & grepl("^ad\\.[1-8]\\.t2$", node2)) |
        (node2 == coping_var & grepl("^ad\\.[1-8]\\.t2$", node1))
    ) %>%
    mutate(Coping = label)
}

# ursprüngliche Coping-Variablen
humor_adnm <- extract_coping_adnm("bcope.hu.t2", "Humor")
posr_adnm  <- extract_coping_adnm("bcope.posr.t2", "Positive Reframing")
acc_adnm   <- extract_coping_adnm("bcope.acc.t2", "Acceptance")

# neue Coping-Variablen
esupp_adnm <- extract_coping_adnm("bcope.esupp.t2", "Emotional Support")
act_adnm   <- extract_coping_adnm("bcope.act.t2", "Active Coping")
ven_adnm   <- extract_coping_adnm("bcope.ven.t2", "Venting")
rel_adnm   <- extract_coping_adnm("bcope.rel.t2", "Religion")
dist_adnm  <- extract_coping_adnm("bcope.dist.t2", "Self Distraction")
isupp_adnm <- extract_coping_adnm("bcope.isupp.t2", "Instrumental Support")
behd_adnm  <- extract_coping_adnm("bcope.behd.t2", "Behavioral Disengagement")
pl_adnm    <- extract_coping_adnm("bcope.pl.t2", "Planning")
sub_adnm   <- extract_coping_adnm("bcope.sub.t2", "Substance Use")
den_adnm   <- extract_coping_adnm("bcope.den.t2", "Denial")
sbl_adnm   <- extract_coping_adnm("bcope.sbl.t2", "Self Blame")

# Prüfen, wie viele Kanten pro Coping-Mechanismus extrahiert wurden
sapply(list(
  humor_adnm, posr_adnm, acc_adnm,
  esupp_adnm, act_adnm, ven_adnm, rel_adnm, dist_adnm,
  isupp_adnm, behd_adnm, pl_adnm, sub_adnm, den_adnm, sbl_adnm
), nrow)

# alle Daten zusammenführen und zusammenfassen
coping_summary <- bind_rows(
  humor_adnm, posr_adnm, acc_adnm,
  esupp_adnm, act_adnm, ven_adnm, rel_adnm, dist_adnm,
  isupp_adnm, behd_adnm, pl_adnm, sub_adnm, den_adnm, sbl_adnm
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
    humor_adnm, posr_adnm, acc_adnm,
    esupp_adnm, act_adnm, ven_adnm, rel_adnm, dist_adnm,
    isupp_adnm, behd_adnm, pl_adnm, sub_adnm, den_adnm, sbl_adnm
  ),
  "coping_adnm_edge_cis_t2.csv",
  row.names = FALSE
)
