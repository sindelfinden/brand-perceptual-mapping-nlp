# =========================
# 12_figures.R
# Final production version, LNCS-compatible
# =========================

library(ggplot2)
library(ggrepel)
library(data.table)

# --------------------------------------------------
# 1. Theme for publication-ready figures
# --------------------------------------------------
theme_lncs <- function(base_size = 11) {
    theme_minimal(base_size = base_size) +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.25),
            axis.title = element_text(face = "bold"),
            plot.title = element_text(face = "bold", hjust = 0.5),
            legend.title = element_text(face = "bold"),
            legend.position = "right"
        )
}

# --------------------------------------------------
# 2. Generic helper to save plots safely
# --------------------------------------------------
save_plot_safe <- function(plot_obj, out_file, width = 6.5, height = 4.5, dpi = 300) {
    if (is.null(out_file)) return(invisible(NULL))
    
    out_dir <- dirname(out_file)
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    ggsave(
        filename = out_file,
        plot = plot_obj,
        width = width,
        height = height,
        dpi = dpi,
        units = "in"
    )
}

# --------------------------------------------------
# 3. Brand perceptual map
# --------------------------------------------------
plot_brand_map <- function(
        coords,
        brand_summary,
        title = NULL,
        label_top_n = 30L,
        point_alpha = 0.8,
        out_file = NULL
) {
    stopifnot(!is.null(coords))
    stopifnot(!is.null(brand_summary))
    
    coords <- as.matrix(coords)
    
    if (is.null(rownames(coords))) {
        stop("The 'coords' object must have row names equal to brand names.")
    }
    
    brand_summary <- as.data.table(brand_summary)
    stopifnot(all(c("brand", "n_reviews") %in% names(brand_summary)))
    
    df <- merge(
        data.table(
            brand = rownames(coords),
            dim1 = coords[, 1],
            dim2 = coords[, 2]
        ),
        brand_summary,
        by = "brand",
        all.x = TRUE
    )
    
    # Fallback in case some brands do not appear in brand_summary
    df[is.na(n_reviews), n_reviews := 1L]
    
    top_brands <- df[order(-n_reviews)]$brand[seq_len(min(label_top_n, nrow(df)))]
    
    p <- ggplot(df, aes(x = dim1, y = dim2)) +
        geom_point(aes(size = n_reviews), alpha = point_alpha, shape = 16) +
        geom_text_repel(
            data = df[brand %in% top_brands],
            aes(label = brand),
            max.overlaps = Inf,
            size = 3,
            box.padding = 0.25,
            point.padding = 0.15,
            segment.linewidth = 0.25
        ) +
        coord_equal() +
        theme_lncs(base_size = 11) +
        labs(
            title = title,
            x = "Dimension 1",
            y = "Dimension 2",
            size = "Reviews"
        )
    
    save_plot_safe(p, out_file, width = 6.5, height = 4.5, dpi = 300)
    p
}

# --------------------------------------------------
# 4. Primary metrics figure
#    Compatible with run_pipeline.R -> primary_metrics
# --------------------------------------------------
plot_primary_metrics <- function(metrics_df, out_file = NULL) {
    stopifnot(!is.null(metrics_df))
    
    metrics_df <- as.data.table(metrics_df)
    required_cols <- c("model", "projection", "mantel_r", "knn10")
    missing_cols <- setdiff(required_cols, names(metrics_df))
    
    if (length(missing_cols) > 0L) {
        stop(
            "The following required columns are missing from metrics_df: ",
            paste(missing_cols, collapse = ", ")
        )
    }
    
    metrics_long <- rbindlist(list(
        metrics_df[, .(model, projection, metric = "Mantel r", value = mantel_r)],
        metrics_df[, .(model, projection, metric = "kNN overlap", value = knn10)]
    ))
    
    metrics_long[, panel := paste(model, projection, sep = " | ")]
    
    p <- ggplot(metrics_long, aes(x = projection, y = value, fill = metric)) +
        geom_col(position = "dodge", width = 0.7) +
        facet_wrap(~ model, ncol = 1, scales = "free_y") +
        theme_lncs(base_size = 11) +
        labs(
            title = "Primary map quality metrics by semantic model and projection",
            x = "Projection method",
            y = "Metric value",
            fill = "Metric"
        )
    
    save_plot_safe(p, out_file, width = 7.0, height = 5.5, dpi = 300)
    p
}

# --------------------------------------------------
# 5. Robustness figure
#    Compatible with full robustness grid output
# --------------------------------------------------
plot_robustness_grid <- function(grid_res, out_file = NULL) {
    stopifnot(!is.null(grid_res))
    
    grid_res <- as.data.table(grid_res)
    required_cols <- c("model", "aggregation", "projection", "mantel_r")
    missing_cols <- setdiff(required_cols, names(grid_res))
    
    if (length(missing_cols) > 0L) {
        stop(
            "The following required columns are missing from grid_res: ",
            paste(missing_cols, collapse = ", ")
        )
    }
    
    grid_res[, model_agg := paste(model, aggregation, sep = " × ")]
    
    p <- ggplot(grid_res, aes(x = model_agg, y = mantel_r)) +
        geom_boxplot() +
        facet_wrap(~ projection, ncol = 1) +
        coord_flip() +
        theme_lncs(base_size = 11) +
        labs(
            title = "Map fidelity across semantic models, aggregation rules, and projections",
            x = "Model × aggregation",
            y = "Mantel r"
        )
    
    save_plot_safe(p, out_file, width = 7.0, height = 6.0, dpi = 300)
    p
}

# --------------------------------------------------
# 6. Bootstrap stability figure
# --------------------------------------------------
plot_bootstrap_stability <- function(boot_df, out_file = NULL) {
    stopifnot(!is.null(boot_df))
    
    boot_df <- as.data.table(boot_df)
    required_cols <- c("iter", "mean_self_cosine", "protest_r")
    missing_cols <- setdiff(required_cols, names(boot_df))
    
    if (length(missing_cols) > 0L) {
        stop(
            "The following required columns are missing from boot_df: ",
            paste(missing_cols, collapse = ", ")
        )
    }
    
    long_df <- rbindlist(list(
        boot_df[, .(iter, metric = "Mean self-cosine", value = mean_self_cosine)],
        boot_df[, .(iter, metric = "Procrustes protest r", value = protest_r)]
    ))
    
    p <- ggplot(long_df, aes(x = metric, y = value)) +
        geom_boxplot() +
        theme_lncs(base_size = 11) +
        labs(
            title = "Bootstrap stability diagnostics",
            x = NULL,
            y = "Value"
        )
    
    save_plot_safe(p, out_file, width = 6.0, height = 4.5, dpi = 300)
    p
}

# --------------------------------------------------
# 7. Two-map comparison figure
#    Useful for PCA vs UMAP or UMAP vs t-SNE
# --------------------------------------------------
plot_map_pair <- function(
        coords_a,
        coords_b,
        brand_summary,
        title_a = "Map A",
        title_b = "Map B",
        label_top_n = 25L,
        out_file = NULL
) {
    coords_a <- as.matrix(coords_a)
    coords_b <- as.matrix(coords_b)
    
    if (is.null(rownames(coords_a)) || is.null(rownames(coords_b))) {
        stop("Both map matrices must have row names equal to brand names.")
    }
    
    brand_summary <- as.data.table(brand_summary)
    stopifnot(all(c("brand", "n_reviews") %in% names(brand_summary)))
    
    df_a <- merge(
        data.table(brand = rownames(coords_a), dim1 = coords_a[, 1], dim2 = coords_a[, 2], map = title_a),
        brand_summary,
        by = "brand",
        all.x = TRUE
    )
    
    df_b <- merge(
        data.table(brand = rownames(coords_b), dim1 = coords_b[, 1], dim2 = coords_b[, 2], map = title_b),
        brand_summary,
        by = "brand",
        all.x = TRUE
    )
    
    df <- rbind(df_a, df_b, fill = TRUE)
    df[is.na(n_reviews), n_reviews := 1L]
    
    top_brands <- df[order(-n_reviews)]$brand |> unique() |> head(label_top_n)
    
    p <- ggplot(df, aes(x = dim1, y = dim2)) +
        geom_point(aes(size = n_reviews), alpha = 0.8, shape = 16) +
        geom_text_repel(
            data = df[brand %in% top_brands],
            aes(label = brand),
            max.overlaps = Inf,
            size = 2.8,
            box.padding = 0.2,
            point.padding = 0.15,
            segment.linewidth = 0.25
        ) +
        facet_wrap(~ map, ncol = 2, scales = "free") +
        theme_lncs(base_size = 11) +
        labs(
            title = "Comparison of two perceptual maps",
            x = "Dimension 1",
            y = "Dimension 2",
            size = "Reviews"
        )
    
    save_plot_safe(p, out_file, width = 8.5, height = 4.5, dpi = 300)
    p
}