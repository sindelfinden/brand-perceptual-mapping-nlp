# =========================
# 09_manual_validation.R
# =========================

library(data.table)
library(irr)
library(vegan)

make_brand_packet <- function(dt, map_coords, n_top_reviews = 5L) {
    brand_summary <- dt[, .(
        n_reviews = .N,
        sample_titles = paste(unique(na.omit(product_title))[1:min(3L, uniqueN(product_title))], collapse = " || "),
        sample_features = paste(unique(na.omit(features))[1:min(3L, uniqueN(features))], collapse = " || "),
        sample_reviews = paste(head(text_raw[order(-helpful_weight)], n_top_reviews), collapse = " || ")
    ), by = brand]
    
    coords <- data.table(
        brand = rownames(map_coords),
        dim1 = map_coords[, 1],
        dim2 = map_coords[, 2]
    )
    
    packet <- merge(brand_summary, coords, by = "brand", all.x = TRUE)
    fwrite(packet, "data/manual_coding/brand_coding_packet.csv")
    
    packet
}

make_validation_template <- function(packet, sample_n = 24L, seed = SEED) {
    set.seed(seed)
    
    # stratify by map position (k-means) and review volume quartile
    qs <- unique(quantile(packet$n_reviews, probs = seq(0, 1, 0.25), na.rm = TRUE))
    packet[, vol_q := cut(n_reviews, breaks = qs, include.lowest = TRUE)]
    k_use <- min(6L, nrow(packet))
    packet[, cluster := stats::kmeans(scale(.SD), centers = k_use, nstart = 20)$cluster, .SDcols = c("dim1", "dim2")]
    
    val_sample <- packet[, .SD[sample(.N, size = min(1L, .N))], by = .(cluster, vol_q)]
    if (nrow(val_sample) < sample_n) {
        extra <- packet[!brand %in% val_sample$brand][sample(.N, min(sample_n - nrow(val_sample), .N))]
        val_sample <- rbind(val_sample, extra, fill = TRUE)
    }
    
    template <- data.frame(
        brand = val_sample$brand,
        coder = NA_character_,
        quality_positioning = NA_character_,
        price_value_positioning = NA_character_,
        natural_clinical = NA_character_,
        safety_sensitive_skin = NA_character_,
        premium_mass = NA_character_,
        quality_score = NA_real_,
        price_score = NA_real_,
        notes = NA_character_,
        stringsAsFactors = FALSE
    )
    
    fwrite(template, "data/manual_coding/brand_labels_template.csv")
    template
}

compute_kappa <- function(coding_file, variable, weight = "unweighted") {
    x <- data.table::fread(coding_file)
    wide <- data.table::dcast(x, brand ~ coder, value.var = variable)
    ratings <- as.data.frame(wide[, -1])
    ratings <- ratings[complete.cases(ratings), , drop = FALSE]
    
    stopifnot(ncol(ratings) == 2L)
    irr::kappa2(ratings, weight = weight)
}

# Example planning call for a binary code:
irr::N.cohen.kappa(rate1 = 0.5, rate2 = 0.5, k1 = 0.70, k0 = 0.40, alpha = 0.05, power = 0.80)
