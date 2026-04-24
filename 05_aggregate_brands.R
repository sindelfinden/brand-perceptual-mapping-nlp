# =========================
# 05_aggregate_brands.R
# =========================

aggregate_brand_vectors <- function(dt, reps) {
    list(
        tfidf = list(
            mean    = l2_normalize_rows(aggregate_sparse_weighted_mean(reps$tfidf_review, dt$brand)),
            rating  = l2_normalize_rows(aggregate_sparse_weighted_mean(reps$tfidf_review, dt$brand, dt$rating_weight)),
            helpful = l2_normalize_rows(aggregate_sparse_weighted_mean(reps$tfidf_review, dt$brand, dt$helpful_weight))
        ),
        w2v = list(
            mean    = l2_normalize_rows(aggregate_dense_weighted_mean(reps$w2v_review, dt$brand)),
            rating  = l2_normalize_rows(aggregate_dense_weighted_mean(reps$w2v_review, dt$brand, dt$rating_weight)),
            helpful = l2_normalize_rows(aggregate_dense_weighted_mean(reps$w2v_review, dt$brand, dt$helpful_weight))
        ),
        sbert = list(
            mean    = l2_normalize_rows(aggregate_dense_weighted_mean(reps$sbert_review, dt$brand)),
            rating  = l2_normalize_rows(aggregate_dense_weighted_mean(reps$sbert_review, dt$brand, dt$rating_weight)),
            helpful = l2_normalize_rows(aggregate_dense_weighted_mean(reps$sbert_review, dt$brand, dt$helpful_weight))
        )
    )
}
