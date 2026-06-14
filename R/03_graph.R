# R/03_graph.R
# Krok 5-6: lista krawedzi + graf przestrzenny (kierunkowy: sprzedajacy -> kupujacy)
# Wejscie : data/processed/transfers_clean.rds, data/processed/clubs_geocoded.csv
# Wynik   : graph_nodes.rds, graph_edges.rds, graph_igraph.rds (w data/processed/)

library(tidyverse)
library(sf)
library(igraph)
library(rnaturalearth)

transfers <- readRDS("data/processed/transfers_clean.rds")
clubs     <- read_csv("data/processed/clubs_geocoded.csv", show_col_types = FALSE)

node_xy <- clubs %>% select(club_id, club_name, country, league_id, lat, lon)

# ---- 5. Lista krawedzi (kierunkowa, zagregowana) -------------------------
edges <- transfers %>%
  filter(from_club_id != to_club_id) %>%
  group_by(from_club_id, to_club_id) %>%
  summarise(
    n_transfers = n(),
    total_fee   = sum(transfer_fee,        na.rm = TRUE),
    total_mv    = sum(market_value_in_eur, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(from_club_id %in% node_xy$club_id,
         to_club_id   %in% node_xy$club_id)

# ---- 6a. Graf igraph + metryki sieciowe ----------------------------------
g <- graph_from_data_frame(
  d = edges %>% select(from = from_club_id, to = to_club_id,
                       n_transfers, total_fee, total_mv),
  directed = TRUE,
  vertices = node_xy
)

V(g)$deg_in    <- degree(g, mode = "in")                              # ilu sprzedawcow
V(g)$deg_out   <- degree(g, mode = "out")                             # ilu kupcow
V(g)$deg_total <- degree(g, mode = "all")
V(g)$wdeg_in   <- strength(g, mode = "in",  weights = E(g)$n_transfers)
V(g)$wdeg_out  <- strength(g, mode = "out", weights = E(g)$n_transfers)
V(g)$betw      <- betweenness(g, directed = TRUE)                     # posrednictwo

nodes <- igraph::as_data_frame(g, what = "vertices") %>%
  as_tibble() %>%
  rename(club_id = name) %>%
  mutate(club_id = as.numeric(club_id))

# ---- 6b. Warstwy przestrzenne sf -----------------------------------------
nodes_sf <- nodes %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

edge_lines <- edges %>%
  left_join(node_xy %>% select(club_id, x1 = lon, y1 = lat),
            by = c("from_club_id" = "club_id")) %>%
  left_join(node_xy %>% select(club_id, x2 = lon, y2 = lat),
            by = c("to_club_id"   = "club_id"))

geoms <- st_sfc(
  mapply(function(x1, y1, x2, y2)
           st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)),
         edge_lines$x1, edge_lines$y1, edge_lines$x2, edge_lines$y2,
         SIMPLIFY = FALSE),
  crs = 4326
)
edges_sf <- st_sf(
  edge_lines %>% select(from_club_id, to_club_id, n_transfers, total_fee, total_mv),
  geometry = geoms
)
edges_sf$dist_km <- as.numeric(st_length(edges_sf)) / 1000   # odleglosc wielkokolowa

# ---- Zapis ---------------------------------------------------------------
saveRDS(nodes_sf, "data/processed/graph_nodes.rds")
saveRDS(edges_sf, "data/processed/graph_edges.rds")
saveRDS(g,        "data/processed/graph_igraph.rds")

# ---- Podsumowanie --------------------------------------------------------
cat("\n--- GRAF: PODSUMOWANIE ---\n")
cat("Wezly (kluby):", vcount(g), "\n")
cat("Krawedzie (pary klub->klub):", ecount(g), "\n")
cat("Transferow lacznie:", sum(edges$n_transfers), "\n\n")

cat("TOP 15 klubow wg stopnia calkowitego (huby):\n")
nodes %>%
  arrange(desc(deg_total)) %>%
  select(club_name, country, deg_in, deg_out, deg_total, betw) %>%
  head(15) %>% print()

# ---- Mapa kontrolna ------------------------------------------------------
europe <- ne_countries(scale = "medium", continent = "Europe",
                        returnclass = "sf")

p <- ggplot() +
  geom_sf(data = europe, fill = "grey96", color = "white") +
  geom_sf(data = edges_sf, aes(linewidth = n_transfers),
          color = "steelblue", alpha = 0.25) +
  geom_sf(data = nodes_sf, aes(size = deg_total),
          color = "firebrick", alpha = 0.8) +
  scale_linewidth(range = c(0.1, 2), name = "Liczba transferow") +
  scale_size(range = c(0.5, 5), name = "Stopien") +
  coord_sf(xlim = c(-12, 25), ylim = c(35, 60)) +
  labs(title = "Siec transferow klubow (top 5 lig + Ekstraklasa)") +
  theme_minimal()
print(p)
