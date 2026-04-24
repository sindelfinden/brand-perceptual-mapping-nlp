# =========================
# 07_metrics.R
# =========================

library(text2vec)
library(vegan)

cosine_sim <- function(X) {
    as.matrix(text2vec::sim2(X, X, method = "cosine", norm = "l2"))
}

cosine_dist <- function(X) {
    stats::as.dist(1 - cosine_sim(X))
}

knn_overlap <- function(X_high, X_low, k = 10L) {
    k <- min(k, nrow(X_high) - 1L, nrow(X_low) - 1L)
    
    d_high <- as.matrix(stats::dist(X_high))
    d_low  <- as.matrix(stats::dist(X_low))
    
    diag(d_high) <- Inf
    diag(d_low) <- Inf
    
    nn_high <- t(apply(d_high, 1, order))[, seq_len(k), drop = FALSE]
    nn_low  <- t(apply(d_low, 1, order))[, seq_len(k), drop = FALSE]
    
    mean(vapply(seq_len(nrow(nn_high)), function(i) {
        length(intersect(nn_high[i, ], nn_low[i, ])) / k
    }, numeric(1)))
}

evaluate_map <- function(X_brand, coords, permutations = 999L) {
    d_high <- cosine_dist(X_brand)
    d_low  <- stats::dist(coords)
    
    man <- vegan::mantel(d_high, d_low, method = "spearman", permutations = permutations)
    
    data.frame(
        mantel_r = unname(man$statistic),
        mantel_p = man$signif,
        knn10 = knn_overlap(as.matrix(X_brand), coords, k = 10L)
    )
}

compare_maps <- function(coords_a, coords_b, permutations = 9999L) {
    proc <- vegan::procrustes(coords_a, coords_b, scale = TRUE)
    prot <- vegan::protest(coords_a, coords_b, permutations = permutations)
    
    data.frame(
        procrustes_ss = proc$ss,
        protest_r = prot$t0,
        protest_p = prot$signif
    )
}

same_label_distance_test <- function(dist_obj, labels, B = 9999L, seed = SEED) {
    set.seed(seed)
    D <- as.matrix(dist_obj)
    same <- outer(labels, labels, "==") & upper.tri(D)
    obs <- mean(D[same], na.rm = TRUE)
    
    null <- replicate(B, {
        lab <- sample(labels)
        mask <- outer(lab, lab, "==") & upper.tri(D)
        mean(D[mask], na.rm = TRUE)
    })
    
    data.frame(
        observed = obs,
        null_mean = mean(null),
        p_lower = (1 + sum(null <= obs)) / (B + 1L)
    )
}
