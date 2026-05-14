# Citroen ë-SpaceTourer FINN value model

Quarto/R analysis of Citroen ë-SpaceTourer listings on FINN.no.

The scrape on 2026-05-14 found 6 relevant Citroen electric SpaceTourer
listings, including FINN code `441656103`. The regression is estimated on the
broader Stellantis sibling market: Citroen ë-SpaceTourer, Opel Zafira-e Life,
Peugeot e-Traveller, and Toyota Proace Verso Electric. Citroen listings are then
ranked separately by residual.

## Run

```bash
Rscript R/scrape_finn.R
quarto render index.qmd
rm -rf docs
mkdir -p docs
cp -a index.html index_files docs/
touch docs/.nojekyll
```

Outputs:

- `data/processed/spacetourer_listings.csv`
- `data/processed/stellantis_sibling_market.csv`
- `data/processed/spacetourer_relevant.csv`
- `data/processed/spacetourer_ranked_relevant.csv`
- `docs/index.html`

## Focus ad

The report supports a focused FINN ad via the Quarto parameter `focus_url`.
Default:

```yaml
focus_url: "https://www.finn.no/441656103"
```
