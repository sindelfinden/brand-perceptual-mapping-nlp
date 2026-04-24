# =========================
# 11_robustness_grid.R
# =========================

library(data.table)

run_one_model <- function(model_name, agg_name, brand_embeddings) {
    X <- brand_embeddings[[model_name]][[agg_name]]
    maps <- run_maps(X)
    
    rbindlist(lapply(c("pca", "tsne", "umap"), function(prj) {
        cbind(
            model = model_name,
            aggregation = agg_name,
            projection = prj,
            evaluate_map(X, maps[[prj]])
        )
    }))
}

run_grid <- function(brand_embeddings) {
    rbindlist(lapply(names(brand_embeddings), function(m) {
        rbindlist(lapply(names(brand_embeddings[[m]]), function(a) {
            run_one_model(m, a, brand_embeddings)
        }))
    }))
}

# Example:
# grid_res <- run_grid(brand_emb)
# fwrite(grid_res, "outputs/tables/robustness_results.csv")
