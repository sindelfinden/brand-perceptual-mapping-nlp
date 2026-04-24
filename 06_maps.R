# =========================
# 06_maps.R
# =========================

library(Rtsne)
library(uwot)

prepare_dr_input <- function(X, max_rank = 50L) {
    Xd <- as.matrix(X)
    rank_use <- min(max_rank, nrow(Xd) - 1L, ncol(Xd))
    if (rank_use >= 2L) {
        stats::prcomp(Xd, center = TRUE, scale. = FALSE, rank. = rank_use)$x
    } else {
        Xd
    }
}

run_maps <- function(X, seed = SEED,
                     tsne_perplexity = NULL,
                     umap_neighbors = 15L,
                     umap_min_dist = 0.05) {
    
    X_in <- prepare_dr_input(X)
    n <- nrow(X_in)
    
    if (is.null(tsne_perplexity)) {
        tsne_perplexity <- max(5L, min(30L, floor((n - 1L) / 3L)))
    }
    
    # PCA
    set.seed(seed)
    pca <- prcomp(X_in, center = TRUE, scale. = FALSE, rank. = 2L)$x[, 1:2, drop = FALSE]
    
    # t-SNE
    set.seed(seed)
    tsne <- Rtsne::Rtsne(
        X_in,
        dims = 2,
        pca = FALSE,
        perplexity = tsne_perplexity,
        theta = 0.5,
        check_duplicates = FALSE,
        max_iter = 1000L,
        num_threads = 1L,
        verbose = FALSE
    )$Y
    
    # UMAP: reproducible settings
    set.seed(seed)
    umap <- uwot::umap(
        X_in,
        n_neighbors = min(umap_neighbors, n - 1L),
        min_dist = umap_min_dist,
        n_components = 2L,
        init = "spectral",
        batch = TRUE,
        fast_sgd = FALSE,
        n_threads = 1L,
        n_sgd_threads = 1L,
        seed = seed,
        verbose = FALSE
    )
    
    rownames(pca)  <- rownames(X)
    rownames(tsne) <- rownames(X)
    rownames(umap) <- rownames(X)
    
    list(pca = pca, tsne = tsne, umap = umap)
}
