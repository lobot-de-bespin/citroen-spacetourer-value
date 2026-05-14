# Citroen ë-SpaceTourer FINN value model

Quarto/R analysis of Citroen ë-SpaceTourer listings on FINN.no.

The current market is small. The scrape on 2026-05-14 found 6 relevant Citroen
electric SpaceTourer listings, including FINN code `441656103`. The model is
therefore a screening model for residuals, not a high-precision valuation model.

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
- `data/processed/spacetourer_relevant.csv`
- `data/processed/spacetourer_ranked_relevant.csv`
- `docs/index.html`

## Focus ad

The report supports a focused FINN ad via the Quarto parameter `focus_url`.
Default:

```yaml
focus_url: "https://www.finn.no/441656103"
```
