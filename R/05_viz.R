# R/05_viz.R
# Krok 8: wizualizacje raportowej jakosci
# Wejscie : graph_nodes.rds (z metrykami), graph_edges.rds, country_balance.csv
# Wynik   : mapy w figures/

library(tidyverse)
library(sf)
library(rnaturalearth)

nodes_sf <- readRDS("data/processed/graph_nodes.rds")
edges_sf <- readRDS("data/processed/graph_edges.rds")
cbal     <- read_csv("data/processed/country_balance.csv", show_col_types = FALSE)

europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")
xlim <- c(-12, 25); ylim <- c(35, 60)

dir.create("figures", showWarnings = FALSE)

# etykiety najwiekszych hubow
top_hubs <- nodes_sf %>% arrange(desc(deg_total)) %>% slice(1:8)

# ---- A. Mapa sieci (przeplywy + huby) ------------------------------------
p_net <- ggplot() +
  geom_sf(data = europe, fill = "grey97", color = "white") +
  geom_sf(data = edges_sf, aes(linewidth = n_transfers),
          color = "steelblue", alpha = 0.2) +
  geom_sf(data = nodes_sf, aes(size = deg_total),
          color = "firebrick", alpha = 0.85) +
  geom_sf_text(data = top_hubs, aes(label = club_name),
               size = 3, fontface = "bold", color = "grey20",
               nudge_y = 0.4, check_overlap = TRUE) +
  scale_linewidth(range = c(0.1, 1.8), name = "Transfery") +
  scale_size(range = c(0.5, 6), name = "Stopien") +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Siec transferow klubow (top 5 lig + Ekstraklasa)",
       subtitle = "Wielkosc punktu = liczba powiazan; grubosc linii = liczba transferow") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/mapa_sieci.png", p_net, width = 9, height = 7, dpi = 150)

# ---- B. Mapa wspolnot (klastry handlowe) ---------------------------------
p_comm <- ggplot() +
  geom_sf(data = europe, fill = "grey97", color = "white") +
  geom_sf(data = edges_sf, color = "grey75", alpha = 0.15, linewidth = 0.2) +
  geom_sf(data = nodes_sf, aes(color = factor(community), size = deg_total),
          alpha = 0.9) +
  scale_color_brewer(palette = "Set1", name = "Wspolnota") +
  scale_size(range = c(0.5, 5), guide = "none") +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Wspolnoty klubow (Louvain)",
       subtitle = "Klastry pokrywaja sie z ligami narodowymi (modularnosc 0,59)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/mapa_wspolnoty.png", p_comm, width = 9, height = 7, dpi = 150)

# ---- C. Kartogram: bilans finansowy krajow -------------------------------
# England -> United Kingdom (nazwa poligonu w rnaturalearth)
cbal_poly <- cbal %>% mutate(poly_name = recode(country, "England" = "United Kingdom"))
europe_bal <- europe %>%
  left_join(cbal_poly %>% select(poly_name, net_spend_mln),
            by = c("name" = "poly_name"))

p_choro <- ggplot(europe_bal) +
  geom_sf(aes(fill = net_spend_mln), color = "white") +
  scale_fill_gradient2(low = "forestgreen", mid = "grey95", high = "firebrick",
                       midpoint = 0, na.value = "grey90",
                       name = "Saldo (mln EUR)") +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Bilans finansowy krajow",
       subtitle = "czerwony = kupuje wiecej niz sprzedaje (np. Anglia)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/kartogram_bilans.png", p_choro, width = 9, height = 7, dpi = 150)

cat("Zapisano mapy do figures/:\n",
    "- mapa_sieci.png\n - mapa_wspolnoty.png\n - kartogram_bilans.png\n")

print(p_net)
