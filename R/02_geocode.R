# R/02_geocode.R  (wersja 2: lepsze zapytanie + kontrola krzyzowa)
# Krok 4: geokodowanie klubow (OpenStreetMap / Nominatim)
# Wejscie : data/processed/clubs_to_geocode.csv
# Wynik   : data/processed/clubs_geocoded.csv
# Cache   : data/processed/geocode_cache.csv
#
# UWAGA: jesli masz stary cache z wersji 1, usun go raz przed uruchomieniem:
#   file.remove("data/processed/geocode_cache.csv")

library(tidyverse)
library(tidygeocoder)

clubs <- read_csv("data/processed/clubs_to_geocode.csv", show_col_types = FALSE)

cache_path  <- "data/processed/geocode_cache.csv"
output_path <- "data/processed/clubs_geocoded.csv"

# odleglosc haversine w km (do kontroli krzyzowej)
hav_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371; d <- pi / 180
  dlat <- (lat2 - lat1) * d; dlon <- (lon2 - lon1) * d
  a <- sin(dlat / 2)^2 + cos(lat1 * d) * cos(lat2 * d) * sin(dlon / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# ---- wczytaj cache (jesli ma poprawny schemat) ---------------------------
cache_cols <- c("club_id", "lat", "lon", "geo_query",
                "lat_check", "lon_check", "dist_km", "flag")
if (file.exists(cache_path) &&
    all(cache_cols %in% names(read_csv(cache_path, n_max = 0, show_col_types = FALSE)))) {
  cached <- read_csv(cache_path, show_col_types = FALSE)
} else {
  cached <- tibble(club_id = numeric(), lat = numeric(), lon = numeric(),
                   geo_query = character(), lat_check = numeric(),
                   lon_check = numeric(), dist_km = numeric(), flag = logical())
}

done_ok <- cached %>% filter(!is.na(lat), !is.na(lon)) %>% pull(club_id)
to_do   <- clubs %>% filter(!club_id %in% done_ok)
cat("Do zgeokodowania:", nrow(to_do), "z", nrow(clubs), "klubow\n")
cat("(2 zapytania na klub -> okolo", round(nrow(to_do) * 2 / 60), "min)\n")

if (nrow(to_do) > 0) {

  # ZAPYTANIE 1 (glowne): stadion + klub/miasto + kraj
  res <- to_do %>%
    mutate(query = if_else(!is.na(stadium_name) & stadium_name != "",
                           paste(stadium_name, club_name, coalesce(country, ""), sep = ", "),
                           paste(club_name, coalesce(country, ""), sep = ", "))) %>%
    geocode(address = query, method = "osm", lat = lat, long = lon, min_time = 1)

  # fallback gdy glowne nie znalazlo: sam klub + kraj
  miss <- res %>% filter(is.na(lat) | is.na(lon))
  if (nrow(miss) > 0) {
    fb <- miss %>% select(-lat, -lon, -query) %>%
      mutate(query = paste(club_name, coalesce(country, ""), sep = ", ")) %>%
      geocode(address = query, method = "osm", lat = lat, long = lon, min_time = 1)
    res <- bind_rows(res %>% filter(!is.na(lat), !is.na(lon)), fb)
  }

  # ZAPYTANIE 2 (kontrolne): sam klub + kraj
  res <- res %>%
    mutate(query2 = paste(club_name, coalesce(country, ""), sep = ", ")) %>%
    geocode(address = query2, method = "osm",
            lat = lat_check, long = lon_check, min_time = 1)

  # porownanie i flaga
  res <- res %>%
    mutate(
      dist_km = if_else(!is.na(lat) & !is.na(lat_check),
                        hav_km(lat, lon, lat_check, lon_check), NA_real_),
      flag = is.na(lat) | (!is.na(dist_km) & dist_km > 40)
    )

  new_rows <- res %>%
    transmute(club_id, lat, lon, geo_query = query,
              lat_check, lon_check, dist_km, flag)
  cached <- bind_rows(cached %>% filter(!club_id %in% new_rows$club_id), new_rows)
  write_csv(cached, cache_path)
}

# ---- finalny zbior -------------------------------------------------------
clubs_geocoded <- clubs %>%
  left_join(cached %>% select(club_id, lat, lon, geo_query, dist_km, flag),
            by = "club_id")
write_csv(clubs_geocoded, output_path)

# ---- raport --------------------------------------------------------------
cat("\n--- GEOKODOWANIE: PODSUMOWANIE ---\n")
cat("Zgeokodowano:", sum(!is.na(clubs_geocoded$lat)), "/", nrow(clubs_geocoded), "\n")

flagged <- clubs_geocoded %>% filter(flag | is.na(lat))
cat("Do recznego sprawdzenia (flag):", nrow(flagged), "\n\n")
if (nrow(flagged) > 0) {
  flagged %>%
    select(club_id, club_name, country, lat, lon, dist_km) %>%
    arrange(desc(dist_km)) %>% print(n = 60)
}
