# Manual / language-model annotations

This directory is for fields that are not reliably available from FINN's structured specification text.

Rules:
- Reproducible structured fields (price, km, seats, drive, model year, etc.) must come from `R/scrape_finn.R`.
- Language-model or human judgement annotations go in `llm_annotations.csv`.
- Every annotation must include an evidence snippet and annotator.
- Analysis should be able to run without these annotations.
