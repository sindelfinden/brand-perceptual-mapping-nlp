# ----------- __________________
# 00_setup.R
# ----------- __________________

SEED <- 5250

# Reproducible RNG
RNGkind(kind = "Mersenne-Twister",
        normal.kind = "Inversion",
        sample.kind = "Rejection")
set.seed(SEED)

# Project folders
project_dirs <- c(
    "data/raw",
    "data/derived",
    "data/manual_coding",
    "cache/embeddings",
    "outputs/figures",
    "outputs/tables"
)
invisible(lapply(project_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# Minimal CRAN stack
cran_pkgs <- c(
    "jsonlite", "data.table", "dplyr", "quanteda", "tidytext", "stopwords",
    "cld3", "word2vec", "text2vec", "reticulate", "Rtsne", "uwot",
    "vegan", "ggplot2", "ggrepel", "irr", "renv"
)

to_install <- setdiff(cran_pkgs, rownames(installed.packages()))
if (length(to_install) > 0L) {
    install.packages(to_install, repos = "https://cloud.r-project.org")
}

# Record package versions actually installed on your machine
pkg_version_tbl <- data.frame(
    package = cran_pkgs,
    version = vapply(cran_pkgs, function(p) {
        as.character(utils::packageVersion(p))
    }, character(1)),
    stringsAsFactors = FALSE
)

data.table::fwrite(pkg_version_tbl, "outputs/tables/package_versions.csv")

# Optional but strongly recommended:
# initialize renv and snapshot once the environment is stable
if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
}
# renv::init(bare = TRUE)   # run once in a fresh project
# renv::snapshot()          # run after successful package installation
