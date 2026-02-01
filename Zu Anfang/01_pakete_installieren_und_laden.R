# ============================================
# MASTERARBEIT: Netzwerkanalyse ADNM-8
# Skript 01: Pakete installieren und laden
# ============================================

# Pakete installieren (nur einmal nötig!)
install.packages("haven")        # SPSS-Dateien einlesen
install.packages("tidyverse")    # Datenmanipulation
install.packages("qgraph")       # Netzwerk-Visualisierung
install.packages("bootnet")      # Bootstrap für Netzwerke
install.packages("NetworkComparisonTest")  # Netzwerke vergleichen
install.packages("psych")        # Deskriptive Statistiken
install.packages("mice")         # Falls Imputation nötig

# Pakete laden
library(haven)
library(tidyverse)
library(qgraph)
library(bootnet)
library(NetworkComparisonTest)
library(psych)

# Arbeitsverzeichnis checken
getwd()  # Sollte dein Projektordner sein