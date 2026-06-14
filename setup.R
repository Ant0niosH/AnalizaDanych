# setup.R — instalacja pakietów potrzebnych w projekcie
# Uruchom raz po sklonowaniu repo:  source("setup.R")

pkgs <- c(
  "tidyverse",          # wczytywanie i obróbka danych
  "sf",                 # dane przestrzenne (wektorowe)
  "sfnetworks",         # grafy przestrzenne
  "tidygraph",          # grafy w stylu tidyverse
  "igraph",             # metryki sieciowe, wykrywanie społeczności
  "rnaturalearth",      # granice państw / mapy podkładowe
  "rnaturalearthdata",
  "tidygeocoder",       # geokodowanie nazw klubów (OSM/Nominatim)
  "edgebundle",         # edge bundling dla czytelnych flow maps
  "leaflet",            # mapy interaktywne
  "scales",             # formatowanie osi/legend
  "knitr"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install)
}

invisible(lapply(pkgs, library, character.only = TRUE))
message("Gotowe — wszystkie pakiety wczytane.")
