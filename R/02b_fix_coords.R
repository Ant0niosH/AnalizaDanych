# R/02b_fix_coords.R
# Reczne korekty wspolrzednych klubow, ktorych Nominatim nie znalazl
# (skrocone nazwy + znaki diakrytyczne). Uruchom PO 02_geocode.R:
#   source("R/02b_fix_coords.R")
# Zrodlo wspolrzednych: OpenStreetMap / Wikipedia (lokalizacje stadionow).

library(tidyverse)

cache_path  <- "data/processed/geocode_cache.csv"
output_path <- "data/processed/clubs_geocoded.csv"
clubs <- read_csv("data/processed/clubs_to_geocode.csv", show_col_types = FALSE)

fixes <- tribble(
  ~club_id, ~lat,     ~lon,
  985,      53.4631, -2.2913,   # Man Utd        - Old Trafford
  703,      52.9400, -1.1328,   # Nott'm Forest  - The City Ground
  350,      53.3703, -1.4709,   # Sheff Utd      - Bramall Lane
  1160,     43.8156,  4.3589,   # Nimes          - Stade des Costieres
  1162,     49.1790, -0.3927,   # SM Caen        - Stade Michel d'Ornano
  10,       52.0314,  8.5167,   # Arm. Bielefeld - SchucoArena
  2687,     36.5028, -6.2733,   # Cadiz CF       - Nuevo Mirandilla
  993,      37.8590, -4.7596,   # Cordoba CF     - Nuevo Arcangel
  897,      43.3692, -8.4119,   # Dep. La Coruna - Riazor
  1531,     38.2667, -0.6650,   # Elche CF       - Martinez Valero
  1084,     36.7400, -4.4258,   # Malaga CF      - La Rosaleda
  5358,     42.1361, -0.4089,   # SD Huesca      - El Alcoraz
  3302,     36.8400, -2.4350,   # UD Almeria     - Power Horse Stadium
  1210,     43.5436, 10.3203,   # Livorno        - A. Picchi
  281,      53.4831, -2.2004    # Man City       - Etihad Stadium
)

cached <- read_csv(cache_path, show_col_types = FALSE)

cached <- cached %>%
  rows_update(
    fixes %>% mutate(geo_query = "reczna korekta",
                     lat_check = lat, lon_check = lon,
                     dist_km = 0, flag = FALSE),
    by = "club_id", unmatched = "ignore"
  )

write_csv(cached, cache_path)

# regeneruj finalny zbior
clubs_geocoded <- clubs %>%
  left_join(cached %>% select(club_id, lat, lon, geo_query, dist_km, flag),
            by = "club_id")
write_csv(clubs_geocoded, output_path)

cat("Naniesiono korekty       :", nrow(fixes), "\n")
cat("Klubow bez wspolrzednych :", sum(is.na(clubs_geocoded$lat)), "\n")
cat("Nadal flagowane (>40 km) :", sum(clubs_geocoded$flag, na.rm = TRUE), "\n")
