# Sieć transferów piłkarskich w Europie — analiza danych przestrzennych

Projekt z przedmiotu *Analiza danych przestrzennych*. Wizualizacja
europejskiego rynku transferowego jako **grafu na mapie**: kluby to węzły
(z georeferencją wg lokalizacji), a transfery zawodników to krawędzie.

## Struktura projektu

```
spatial-transfers/
├── spatial-transfers.Rproj   # projekt RStudio
├── _quarto.yml               # konfiguracja Quarto
├── report.qmd                # główny raport (renderuje się do .html)
├── references.bib            # literatura (BibTeX)
├── setup.R                   # instalacja pakietów
├── R/                        # skrypty pomocnicze
│   ├── 01_load_clean.R       # wczytanie i czyszczenie danych
│   ├── 02_geocode.R          # geokodowanie klubów (OSM/Nominatim)
│   └── 03_graph.R            # budowa grafu + metryki
├── data/
│   ├── raw/                  # surowe CSV z Kaggle (NIE w repo)
│   └── processed/            # cache geokodowania itp. (w repo)
└── _output/                  # wyrenderowany raport (NIE w repo)
```

## Wymagania

- R (>= 4.2) + RStudio
- Quarto (>= 1.4)
- pakiety: patrz `setup.R`

## Jak uruchomić

1. Sklonuj repo i otwórz `spatial-transfers.Rproj` w RStudio.
2. Zainstaluj pakiety: `source("setup.R")`.
3. Pobierz dane (patrz niżej) do `data/raw/`.
4. Wyrenderuj raport: w RStudio przycisk **Render** lub `quarto render`.

## Dane

Zbiór: **Football Data from Transfermarkt**
(https://www.kaggle.com/datasets/davidcariboo/player-scores).
Pobierz ręcznie z Kaggle i rozpakuj pliki `transfers.csv`, `clubs.csv`,
`competitions.csv` do `data/raw/`. Surowe dane nie są commitowane (zob.
`.gitignore`) — każda osoba pobiera je u siebie.

## Autorzy

- Antoni Handschuh
- Antoni Mirkowski
