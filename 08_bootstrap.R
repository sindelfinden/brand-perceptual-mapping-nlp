# =========================
# 08_bootstrap.R
# =========================

library(vegan)
library(text2vec)

bootstrap_brand_stability_dense <- function(
        review_matrix,
        brands,
        weights = NULL,
        B = 100L,
        projection = "umap",
        seed = SEED
) {
    set.seed(seed)
    
    baseline_brand <- aggregate_dense_weighted_mean(review_matrix, brands, weights)
    baseline_brand <- l2_normalize_rows(baseline_brand)
    baseline_xy <- run_maps(baseline_brand, seed = seed)[[projection]]
    
    by_brand <- split(seq_len(nrow(review_matrix)), brands)
    
    # Pre-generate bootstrap samples BEFORE run_maps() resets seeds
    boot_indices <- vector("list", B)
    for (b in seq_len(B)) {
        boot_indices[[b]] <- unlist(
            lapply(by_brand, function(idx) sample(idx, size = length(idx), replace = TRUE)),
            use.names = FALSE
        )
    }
    
    out <- vector("list", B)
    
    for (b in seq_len(B)) {
        idx <- boot_indices[[b]]
        
        boot_brand <- aggregate_dense_weighted_mean(
            review_matrix[idx, , drop = FALSE],
            brands[idx],
            if (is.null(weights)) NULL else weights[idx]
        )
        
        boot_brand <- l2_normalize_rows(boot_brand)
        
        # Use a different deterministic seed for each map
        boot_xy <- run_maps(boot_brand, seed = seed + b)[[projection]]
        
        prot <- vegan::protest(
            baseline_xy,
            boot_xy,
            permutations = 999L
        )
        
        self_cos <- diag(text2vec::sim2(
            as.matrix(baseline_brand),
            as.matrix(boot_brand),
            method = "cosine",
            norm = "l2"
        ))
        
        out[[b]] <- data.frame(
            iter = b,
            mean_self_cosine = mean(self_cos),
            protest_r = prot$t0,
            protest_p = prot$signif
        )
    }
    
    data.table::rbindlist(out)
}