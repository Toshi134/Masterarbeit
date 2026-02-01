# ============================================
# Skript 02: Daten einlesen
# ============================================

# SPSS-Datei einlesen
# WICHTIG: Ersetze "deine_datei.sav" mit dem echten Dateinamen!
daten <- read_sav("data/COVID_study_GE_T1-T5.sav")

# Ersten Überblick verschaffen
dim(daten)        # Wie viele Zeilen und Spalten?
names(daten)      # Alle Variablennamen
head(daten)       # Erste 6 Zeilen ansehen
str(daten)        # Struktur der Daten

# Spezifisch: Variablen für T1 anzeigen
names(daten)[grep(".t1", names(daten))]  # Alle T1-Variablen