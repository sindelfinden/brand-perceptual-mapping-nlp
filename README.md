# Brand Perceptual Mapping Using NLP and Embeddings

This repository provides a fully reproducible R-based pipeline for constructing brand perceptual maps from large-scale consumer review data using TF-IDF, Word2Vec, and Sentence-BERT representations.

The implementation accompanies the research paper:

**"Uncovering Latent Brand Positions from Consumer Reviews: An Embedding-Based Approach to Perceptual Mapping"**

---

## Overview

This project introduces a data-driven framework for extracting brand positioning from unstructured text data.

The pipeline performs:

- Text preprocessing of consumer reviews  
- Semantic representation learning:
  - TF-IDF (baseline)
  - Word2Vec
  - Sentence-BERT  
- Brand-level embedding aggregation  
- Dimensionality reduction:
  - PCA
  - UMAP
  - t-SNE  
- Quantitative validation:
  - Mantel test
  - kNN neighborhood preservation  
- Bootstrap-based stability analysis  
- External validation via manual brand coding  

The goal is to recover latent perceptual structures directly from consumer discourse.

---

## Data

The empirical analysis uses the **Amazon Review Data (2018)**, specifically the **All_Beauty** category.

The dataset is **not included** in this repository due to size and redistribution restrictions.

Download the dataset from:

👉 https://cseweb.ucsd.edu/~jmcauley/datasets/amazon_v2/

Required files:
- `All_Beauty.json.gz`
- `meta_All_Beauty.json.gz`


Place them in: `data/raw/`


If needed, rename them to:

- `All_Beauty_2018.json.gz`
- `meta_All_Beauty_2018.json.gz`


---

## Reproducibility

### R Environment

This project uses `renv` for reproducibility.

```r
install.packages("renv")
renv::restore()
```

### Python Environment (for Sentence-BERT)

The pipeline uses `reticulate` to interface with Python.

To automatically configure the environment:

```r
source("01_python_env.R")
```

This installs:
- sentence-transformers
- PyTorch (GPU-enabled if available)
- required dependencies
---

### GPU Support
- GPU is **optional but recommended**
- Tested with NVIDIA GPU + CUDA
- Falls back to CPU if CUDA is not available
---

### Running the Pipeline

To run the full pipeline:
```r
source("run_pipeline.R")
```
---

### Pipeline Configuration

Key parameters are controlled via:

```r
CFG <- list(
    dataset_version = "2018",
    min_chars = 20L,
    min_brand_reviews = 50L,
    english_only = TRUE,
    
    run_full_representations = TRUE,
    run_brand_aggregation = TRUE,
    run_primary_maps = TRUE,
    run_primary_metrics = TRUE,
    run_bootstrap = TRUE,
    run_manual_validation = FALSE,
    run_external_validation = FALSE,
    run_full_grid = FALSE,
    run_figures = TRUE,
    
    bootstrap_B = 100L
)
```
---

### Outputs

The pipeline generates:

### Data
- Cleaned review datasets
- Review-level embeddings
- Brand-level embeddings

### Models
- PCA, UMAP, t-SNE perceptual maps

### Evaluation
- Mantel correlation results
- kNN preservation metrics
- Bootstrap stability results

### External Validation
- PERMANOVA (adonis)
- Same-label distance test

### Figures
- Perceptual maps (PDF)
- Metric summaries
---

### Project Structure
```r
.
├── 00_setup.R
├── 01_python_env.R
├── 02_Load_data.R
├── 03_preprocess.R
├── 04_representations.R
├── 05_aggregate_brands.R
├── 06_maps.R
├── 07_metrics.R
├── 08_bootstrap.R
├── 09_manual_validation.R
├── 10_external_tests.R
├── 11_robustness_grid.R
├── 12_figures.R
├── run_pipeline.R
├── renv.lock
├── requirements-gpu.txt
└── README.md
```
---

### Notes
- Large intermediate files (.rds, embeddings, cache) are excluded
- The pipeline is fully reproducible from raw data
- Execution time depends on hardware (GPU recommended)
- Bootstrap and Mantel tests are computationally intensive
---
### Scientific Contribution
This repository demonstrates:
- Extraction of perceptual maps from text without survey data
- Integration of classical and transformer-based NLP models
- Stability validation via bootstrap resampling
- External validation through human-coded brand categories
---
### Citation
If you use this code, please cite the associated paper.
---

### License
MIT License
