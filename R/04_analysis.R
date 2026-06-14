# R/04_analysis.R
# Krok 7: analizy sieci transferowej (odpowiedzi na 4 pytania badawcze)
# Wejscie : graph_igraph.rds, graph_nodes.rds, graph_edges.rds, transfers_clean.rds
# Wynik   : graph_nodes_metrics.rds (+ pliki w figures/ i data/processed/)

library(tidyverse)
library(sf)
library(igraph)

g         <- readRDS("data/processed/graph_igraph.rds")
nodes_sf  <- readRDS("data/processed/graph_nodes.rds")
edges_sf  <- readRDS("data/processed/graph_edges.rds")
transfers <- readRDS("data/processed/transfers_clean.rds")

dir.create("figures", showWarnings = FALSE)

# =========================================================================
# 1. CENTRALNOSC: huby sieciowe vs finansowe
# =========================================================================
# Kierunek krawedzi: sprzedajacy -> kupujacy. Oplata plynie od kupca do sprzedawcy.
#   fee_received = pieniadze ze sprzedazy (krawedzie wychodzace)
#   fee_spent    = pieniadze na zakupy   (krawedzie przychodzace)
vt <- igraph::as_data_frame(g, what = "vertices") %>%
  as_tibble() %>%
  rename(club_id = name) %>%
  mutate(club_id = as.numeric(club_id))

vt$fee_received <- as.numeric(strength(g, mode = "out", weights = E(g)$total_fee))
vt$fee_spent    <- as.numeric(strength(g, mode = "in",  weights = E(g)$total_fee))
vt$fee_net      <- vt$fee_received - vt$fee_spent

# graf nieskierowany do wspolnot i centralnosci wektorowej
gu <- as.undirected(g, mode = "collapse",
                    edge.attr.comb = list(n_transfers = "sum", total_fee = "sum",
                                          total_mv = "sum", "ignore"))
vt$eigen <- eigen_centrality(gu, weights = E(gu)$n_transfers)$vector

cat("\n=== 1. HUBY SIECIOWE (liczba powiazan) ===\n")
vt %>% arrange(desc(deg_total)) %>%
  transmute(club_name, country, deg_total, betw = round(betw)) %>%
  head(10) %>% print()

cat("\n=== 1. HUBY FINANSOWE: najwieksi SPRZEDAJACY (mln EUR) ===\n")
vt %>% arrange(desc(fee_received)) %>%
  transmute(club_name, country, mln = round(fee_received / 1e6, 1)) %>%
  head(10) %>% print()

cat("\n=== 1. HUBY FINANSOWE: najwieksi KUPUJACY (mln EUR) ===\n")
vt %>% arrange(desc(fee_spent)) %>%
  transmute(club_name, country, mln = round(fee_spent / 1e6, 1)) %>%
  head(10) %>% print()

# =========================================================================
# 2. ODLEGLOSCI TRANSFEROW (lokalne vs globalne)
# =========================================================================
dist_tbl <- edges_sf %>% st_drop_geometry() %>% select(n_transfers, dist_km)
dvec <- rep(dist_tbl$dist_km, dist_tbl$n_transfers)   # wazone liczba transferow

cat("\n=== 2. ODLEGLOSCI TRANSFEROW (km) ===\n")
cat("Mediana :", round(median(dvec)), "km\n")
cat("Srednia :", round(mean(dvec)),   "km\n")
cat("Udzial < 300 km (lokalne) :", round(mean(dvec < 300)  * 100), "%\n")
cat("Udzial > 1000 km (dalekie):", round(mean(dvec > 1000) * 100), "%\n")

p_dist <- ggplot(tibble(dist_km = dvec), aes(dist_km)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "white") +
  geom_vline(xintercept = median(dvec), linetype = "dashed") +
  labs(title = "Rozklad odleglosci transferow",
       x = "Odleglosc (km)", y = "Liczba transferow") +
  theme_minimal()
ggsave("figures/odleglosci.png", p_dist, width = 8, height = 5, dpi = 150)

# =========================================================================
# 3. WSPOLNOTY (Louvain) - czy klastry pokrywaja sie z ligami?
# =========================================================================
set.seed(1)
comm <- cluster_louvain(gu, weights = E(gu)$n_transfers)
vt$community <- as.integer(membership(comm))

cat("\n=== 3. WSPOLNOTY (Louvain) ===\n")
cat("Liczba wspolnot:", length(unique(vt$community)), "\n")
cat("Modularnosc    :", round(modularity(comm), 3), "\n\n")
cat("Sklad krajowy najwiekszych wspolnot:\n")
vt %>% count(community, country) %>%
  group_by(community) %>% mutate(razem = sum(n)) %>%
  ungroup() %>% filter(razem >= 10) %>%
  arrange(desc(razem), desc(n)) %>% print(n = 30)

# =========================================================================
# 4. BILANS TRANSFEROWY KRAJOW
# =========================================================================
sold <- transfers %>% filter(!is.na(from_country)) %>%
  group_by(country = from_country) %>%
  summarise(players_sold = n(), revenue = sum(transfer_fee, na.rm = TRUE), .groups = "drop")
bought <- transfers %>% filter(!is.na(to_country)) %>%
  group_by(country = to_country) %>%
  summarise(players_bought = n(), spending = sum(transfer_fee, na.rm = TRUE), .groups = "drop")

country_balance <- full_join(sold, bought, by = "country") %>%
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         net_spend_mln   = round((spending - revenue) / 1e6, 1),
         net_players     = players_bought - players_sold)

cat("\n=== 4. BILANS KRAJOW ===\n")
country_balance %>%
  transmute(country, players_sold, players_bought, net_players,
            revenue_mln = round(revenue/1e6), spending_mln = round(spending/1e6),
            net_spend_mln) %>%
  arrange(desc(net_spend_mln)) %>% print()

p_bal <- country_balance %>%
  ggplot(aes(reorder(country, net_spend_mln), net_spend_mln,
             fill = net_spend_mln > 0)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("forestgreen", "firebrick"), guide = "none") +
  labs(title = "Bilans finansowy krajow (zakupy - sprzedaz)",
       subtitle = "czerwony = kraj wydaje wiecej niz zarabia",
       x = NULL, y = "Saldo (mln EUR)") +
  theme_minimal()
ggsave("figures/bilans_krajow.png", p_bal, width = 8, height = 5, dpi = 150)

# =========================================================================
# ZAPIS metryk (do mapy i raportu)
# =========================================================================
saveRDS(vt, "data/processed/graph_nodes_metrics.rds")
write_csv(country_balance, "data/processed/country_balance.csv")

nodes_sf <- nodes_sf %>%
  left_join(vt %>% select(club_id, fee_received, fee_spent, fee_net,
                          community, eigen), by = "club_id")
saveRDS(nodes_sf, "data/processed/graph_nodes.rds")

cat("\nZapisano metryki, bilans krajow i wykresy (figures/).\n")
