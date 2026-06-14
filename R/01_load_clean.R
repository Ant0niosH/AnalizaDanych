# R/01_load_clean.R
# Krok 2-3: wczytanie i czyszczenie danych transferowych (Transfermarkt / Kaggle)
# Wynik: data/processed/transfers_clean.rds  oraz  data/processed/clubs_to_geocode.csv

library(tidyverse)

# ---- Parametry (edytuj tutaj) --------------------------------------------
# Kraje, których pierwsza liga wchodzi do analizy:
target_countries <- c("England", "Spain", "Germany", "Italy", "France", "Poland")

# Sezony transferowe (format z danych: "YY/YY"):
target_seasons <- c("21/22", "22/23", "23/24", "24/25", "25/26")

# Reguła zakresu sieci:
#   "both"   = oba kluby (źródłowy i docelowy) muszą być w ligach docelowych
#              -> mniejszy, czysty graf, idealny na pierwsze uruchomienie
#   "either" = wystarczy, że jeden koniec jest w ligach docelowych
#              -> szerszy graf (import/eksport spoza Europy), lepszy do analizy
#                 odległości; więcej klubów do geokodowania
scope_rule <- "both"

# ---- Wczytanie -----------------------------------------------------------
transfers <- read_csv("data/raw/transfers.csv",    show_col_types = FALSE)
clubs     <- read_csv("data/raw/clubs.csv",        show_col_types = FALSE)
comps     <- read_csv("data/raw/competitions.csv", show_col_types = FALSE)

# ---- Ligi docelowe (pierwszy poziom rozgrywek w wybranych krajach) -------
target_comps <- comps %>%
  filter(type == "domestic_league",
         sub_type == "first_tier",
         country_name %in% target_countries) %>%
  select(competition_id, country_name)

cat("Ligi docelowe:\n")
print(target_comps)

# ---- Słownik klubów: kraj + stadion + czy w zakresie ---------------------
club_lookup <- clubs %>%
  left_join(comps %>% select(competition_id, country_name),
            by = c("domestic_competition_id" = "competition_id")) %>%
  transmute(
    club_id,
    club_name    = name,
    stadium_name,
    league_id    = domestic_competition_id,
    country      = country_name,
    in_scope     = domestic_competition_id %in% target_comps$competition_id
  )

# ---- Czyszczenie i filtrowanie transferów --------------------------------
transfers_clean <- transfers %>%
  filter(transfer_season %in% target_seasons) %>%
  filter(from_club_id != to_club_id) %>%
  left_join(club_lookup %>% select(club_id,
                                   from_country  = country,
                                   from_in_scope = in_scope),
            by = c("from_club_id" = "club_id")) %>%
  left_join(club_lookup %>% select(club_id,
                                   to_country  = country,
                                   to_in_scope = in_scope),
            by = c("to_club_id" = "club_id")) %>%
  mutate(from_in_scope = coalesce(from_in_scope, FALSE),
         to_in_scope   = coalesce(to_in_scope, FALSE))

# reguła zakresu
transfers_clean <- if (scope_rule == "both") {
  transfers_clean %>% filter(from_in_scope & to_in_scope)
} else {
  transfers_clean %>% filter(from_in_scope | to_in_scope)
}

# ---- Lista unikalnych klubów w sieci (do geokodowania w kroku 4) ---------
clubs_in_network <- bind_rows(
  transfers_clean %>% select(club_id = from_club_id, club_name = from_club_name),
  transfers_clean %>% select(club_id = to_club_id,   club_name = to_club_name)
) %>%
  distinct(club_id, .keep_all = TRUE) %>%
  left_join(club_lookup %>% select(club_id, stadium_name, country, league_id),
            by = "club_id") %>%
  arrange(country, club_name)

# ---- Zapis ---------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(transfers_clean, "data/processed/transfers_clean.rds")
write_csv(clubs_in_network, "data/processed/clubs_to_geocode.csv")

# ---- Krótkie podsumowanie ------------------------------------------------
cat("\n--- PODSUMOWANIE ---\n")
cat("Transferow po czyszczeniu :", nrow(transfers_clean), "\n")
cat("Unikalnych klubow w sieci :", nrow(clubs_in_network), "\n")
cat("Sezony                    :", paste(target_seasons, collapse = ", "), "\n")
cat("Regula zakresu            :", scope_rule, "\n\n")

cat("Transfery wg kraju docelowego:\n")
transfers_clean %>% count(to_country, sort = TRUE) %>% print(n = 20)
