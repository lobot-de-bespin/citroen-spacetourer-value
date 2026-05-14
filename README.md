# Stellantis electric MPV FINN depreciation model

Quarto/R analysis of electric Stellantis MPV listings on FINN.no:
Citroen ë-SpaceTourer, Opel Zafira-e Life, Peugeot e-Traveller, and Toyota
Proace Verso Electric.

The scrape on 2026-05-14 found 32 current candidate listings, including FINN
code `441656103`. The report estimates current new-car price for each listing
from Norwegian 2026 price lists/configurators, computes
`depreciation = estimated_new_price - used_price`, then ranks all candidates by
depreciation residual.

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
- `data/processed/stellantis_ranked_candidates.csv`
- `data/processed/spacetourer_relevant.csv`
- `data/processed/spacetourer_ranked_relevant.csv`
- `data/raw/new_price_sources/`
- `docs/index.html`

## Focus ad

The report supports a focused FINN ad via the Quarto parameter `focus_url`.
Default:

```yaml
focus_url: "https://www.finn.no/441656103"
```
