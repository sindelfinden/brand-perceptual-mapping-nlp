# Brand Perceptual Mapping Using NLP and Embeddings

This repository contains a reproducible R-based pipeline for constructing brand perceptual maps from Amazon Beauty review data using TF-IDF, Word2Vec, and Sentence-BERT representations.

## Data

The raw Amazon review data are not included in this repository. Users should download the Amazon Beauty review files from the official Amazon Reviews 2023 / McAuley dataset source and place them in `data/raw/`.

## Reproducibility

The R environment is controlled with `renv`. Python dependencies for Sentence-BERT are handled through `reticulate` and a Conda environment.

## Run

```r
source("run_pipeline.R")
