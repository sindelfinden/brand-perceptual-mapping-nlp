# =========================
# 10_external_tests.R
# Category-based external validation
# =========================

library(data.table)
library(vegan)

compare_labels_to_map <- function(map_coords, brand_matrix, coding_file, permutations = 9999L) {
    
    coding <- data.table::fread(coding_file)
    
    required_cols <- c("brand", "category_code", "category_label", "confidence", "include_in_analysis")
    missing_cols <- setdiff(required_cols, names(coding))
    if (length(missing_cols) > 0L) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    
    coding[, brand := trimws(brand)]
    coding <- coding[brand %in% rownames(brand_matrix)]
    coding <- coding[include_in_analysis == 1]
    
    coding[, category_label := factor(category_label)]
    coding[, confidence := as.numeric(confidence)]
    
    # High-dimensional semantic validation
    D_high <- cosine_dist(brand_matrix[coding$brand, , drop = FALSE])
    
    fit_adonis <- vegan::adonis2(
        D_high ~ category_label,
        data = coding,
        permutations = permutations
    )
    
    # 2D map validation
    coords <- data.table(
        brand = rownames(map_coords),
        dim1 = map_coords[, 1],
        dim2 = map_coords[, 2]
    )
    
    val <- merge(coords, coding, by = "brand", all.y = TRUE)
    val <- val[complete.cases(dim1, dim2, category_label)]
    
    # Test whether same-category brands are spatially closer in the 2D map
    fit_same <- same_label_distance_test(
        dist_obj = stats::dist(val[, .(dim1, dim2)]),
        labels = val$category_label,
        B = permutations
    )
    
    # Optional: categorical centroids for interpretation
    centroids <- val[, .(
        n = .N,
        mean_dim1 = mean(dim1),
        mean_dim2 = mean(dim2),
        mean_confidence = mean(confidence, na.rm = TRUE)
    ), by = category_label][order(category_label)]
    
    list(
        adonis = fit_adonis,
        same_label = fit_same,
        coded_data = val,
        category_centroids = centroids
    )
}