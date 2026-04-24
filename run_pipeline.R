# =========================
# run_pipeline.R
# Production / paper-ready orchestration script
# =========================

rm(list = ls())
gc()

# --------------------------------------------------
# 0. GLOBAL OPTIONS
# --------------------------------------------------
options(stringsAsFactors = FALSE)
options(width = 120)

# --------------------------------------------------
# 1. SOURCE MODULES
# --------------------------------------------------
source("00_setup.R")
source("01_python_env.R")
source("02_Load_data.R")
source("03_preprocess.R")
source("04_representations.R")
source("05_aggregate_brands.R")
source("06_maps.R")
source("07_metrics.R")
source("08_bootstrap.R")
source("09_manual_validation.R")
source("10_external_tests.R")
source("11_robustness_grid.R")
source("12_figures.R")

# --------------------------------------------------
# 2. PROJECT CONFIGURATION
# --------------------------------------------------
CFG <- list(
    dataset_version = "2018",   # "2018" or "2023"
    min_chars = 20L,
    min_brand_reviews = 50L,
    english_only = TRUE,
    
    run_full_representations = TRUE,
    run_brand_aggregation = TRUE,
    run_primary_maps = TRUE,
    run_primary_metrics = FALSE,
    run_bootstrap = TRUE,       # expensive
    run_manual_validation = FALSE,
    run_external_validation = FALSE,
    run_full_grid = FALSE,      # very expensive
    run_figures = FALSE,
    
    bootstrap_B = 100L,
    seed = SEED
)

# --------------------------------------------------
# 3. PATHS
# --------------------------------------------------
dir.create("cache", recursive = TRUE, showWarnings = FALSE)
dir.create("cache/embeddings", recursive = TRUE, showWarnings = FALSE)
dir.create("cache/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

suffix <- paste0(CFG$dataset_version, "_gpu")

PATHS <- list(
    reviews_raw   = file.path("cache", paste0("reviews_raw_", CFG$dataset_version, ".rds")),
    reviews_clean = file.path("cache", paste0("reviews_clean_", CFG$dataset_version, ".rds")),
    reps          = file.path("cache/embeddings", paste0("review_representations_", suffix, ".rds")),
    brand_emb     = file.path("data/derived", paste0("brand_embeddings_", suffix, ".rds")),
    maps          = file.path("data/derived", paste0("maps_primary_", suffix, ".rds")),
    grid          = file.path("outputs/tables", paste0("robustness_grid_", suffix, ".csv")),
    bootstrap     = file.path("outputs/tables", paste0("bootstrap_sbert_umap_", suffix, ".csv")),
    brand_packet  = file.path("data/manual_coding", "brand_coding_packet.csv"),
    coding_tpl    = file.path("data/manual_coding", "brand_labels_template.csv"),
    timings       = file.path("outputs/tables", paste0("pipeline_timings_", suffix, ".csv")),
    log_file      = file.path("cache/logs", paste0("pipeline_log_", suffix, ".txt"))
)

# --------------------------------------------------
# 4. LIGHTWEIGHT LOGGER
# --------------------------------------------------
log_message <- function(..., append = TRUE) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste(..., collapse = ""))
    cat(msg, "\n")
    cat(msg, "\n", file = PATHS$log_file, append = append)
}

time_step <- function(step_name, expr) {
    log_message("START: ", step_name)
    t0 <- Sys.time()
    result <- force(expr)
    t1 <- Sys.time()
    
    elapsed_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    timing_row <- data.frame(
        step = step_name,
        start_time = as.character(t0),
        end_time = as.character(t1),
        elapsed_seconds = elapsed_sec,
        stringsAsFactors = FALSE
    )
    
    if (file.exists(PATHS$timings)) {
        old <- data.table::fread(PATHS$timings)
        old <- as.data.frame(old)
        timing_out <- rbind(old, timing_row)
    } else {
        timing_out <- timing_row
    }
    
    data.table::fwrite(timing_out, PATHS$timings)
    log_message("END: ", step_name, " | elapsed_seconds = ", round(elapsed_sec, 2))
    
    result
}

# --------------------------------------------------
# 5. CHECK PYTHON / GPU ENVIRONMENT
# --------------------------------------------------
log_message("Checking Python / GPU environment...")
py_info <- tryCatch(readRDS("outputs/tables/python_versions.rds"), error = function(e) NULL)

if (!is.null(py_info)) {
    log_message("Python executable: ", py_info$python_executable)
    log_message("sentence-transformers: ", py_info$sentence_transformers)
    log_message("torch: ", py_info$torch)
    log_message("torch CUDA: ", py_info$torch_cuda)
    log_message("CUDA available: ", py_info$cuda_available)
    log_message("Device count: ", py_info$device_count)
    log_message("GPU name: ", py_info$gpu_name)
} else {
    log_message("WARNING: python_versions.rds not found.")
}

# --------------------------------------------------
# 6. LOAD OR BUILD RAW DATA
# --------------------------------------------------
reviews_raw <- if (file.exists(PATHS$reviews_raw)) {
    time_step("load_cached_reviews_raw", {
        readRDS(PATHS$reviews_raw)
    })
} else {
    time_step("load_amazon_all_beauty", {
        tmp <- load_amazon_all_beauty(CFG$dataset_version)
        saveRDS(tmp, PATHS$reviews_raw)
        tmp
    })
}

log_message("reviews_raw dimensions: ", paste(dim(reviews_raw), collapse = " x "))
log_message("reviews_raw columns: ", paste(names(reviews_raw), collapse = ", "))

# --------------------------------------------------
# 7. LOAD OR BUILD CLEAN DATA
# --------------------------------------------------
reviews_clean <- if (file.exists(PATHS$reviews_clean)) {
    time_step("load_cached_reviews_clean", {
        readRDS(PATHS$reviews_clean)
    })
} else {
    time_step("prep_reviews", {
        tmp <- prep_reviews(
            reviews_raw,
            min_chars = CFG$min_chars,
            min_brand_reviews = CFG$min_brand_reviews,
            english_only = CFG$english_only
        )
        saveRDS(tmp, PATHS$reviews_clean)
        tmp
    })
}

log_message("reviews_clean dimensions: ", paste(dim(reviews_clean), collapse = " x "))

brand_counts <- reviews_clean[, .N, by = brand][order(-N)]
log_message("Number of retained brands: ", nrow(brand_counts))
log_message("Top 10 brands by review count:")
capture.output(print(head(brand_counts, 10)), file = PATHS$log_file, append = TRUE)

# --------------------------------------------------
# 8. LOAD OR BUILD REPRESENTATIONS
# --------------------------------------------------
if (CFG$run_full_representations) {
    reps <- if (file.exists(PATHS$reps)) {
        time_step("load_cached_representations", {
            readRDS(PATHS$reps)
        })
    } else {
        time_step("build_representations", {
            tmp <- build_representations(reviews_clean)
            saveRDS(tmp, PATHS$reps)
            tmp
        })
    }
    
    log_message("Representations built / loaded successfully.")
    log_message("SBERT device: ", reps$sbert_device)
    log_message("SBERT batch size: ", reps$sbert_batch_size)
    log_message("TF-IDF shape: ", paste(dim(reps$tfidf_review), collapse = " x "))
    log_message("Word2Vec shape: ", paste(dim(reps$w2v_review), collapse = " x "))
    log_message("SBERT shape: ", paste(dim(reps$sbert_review), collapse = " x "))
} else {
    reps <- NULL
    log_message("Skipping representation stage by configuration.")
}

# --------------------------------------------------
# 9. LOAD OR BUILD BRAND EMBEDDINGS
# --------------------------------------------------
if (CFG$run_brand_aggregation) {
    if (is.null(reps)) stop("Brand aggregation requested, but 'reps' is NULL.")
    
    brand_emb <- if (file.exists(PATHS$brand_emb)) {
        time_step("load_cached_brand_embeddings", {
            readRDS(PATHS$brand_emb)
        })
    } else {
        time_step("aggregate_brand_vectors", {
            tmp <- aggregate_brand_vectors(reviews_clean, reps)
            saveRDS(tmp, PATHS$brand_emb)
            tmp
        })
    }
    
    log_message("Brand embeddings built / loaded successfully.")
    log_message("TF-IDF brand matrix: ", paste(dim(brand_emb$tfidf$mean), collapse = " x "))
    log_message("Word2Vec brand matrix: ", paste(dim(brand_emb$w2v$mean), collapse = " x "))
    log_message("SBERT brand matrix: ", paste(dim(brand_emb$sbert$mean), collapse = " x "))
} else {
    brand_emb <- NULL
    log_message("Skipping brand aggregation stage by configuration.")
}

# --------------------------------------------------
# 10. PRIMARY MAPS
# --------------------------------------------------
if (CFG$run_primary_maps) {
    if (is.null(brand_emb)) stop("Primary maps requested, but 'brand_emb' is NULL.")
    
    maps_primary <- if (file.exists(PATHS$maps)) {
        time_step("load_cached_primary_maps", {
            readRDS(PATHS$maps)
        })
    } else {
        time_step("run_primary_maps_sbert_mean", {
            tmp <- list(
                sbert = run_maps(brand_emb$sbert$mean),
                w2v   = run_maps(brand_emb$w2v$mean),
                tfidf = run_maps(brand_emb$tfidf$mean)
            )
            saveRDS(tmp, PATHS$maps)
            tmp
        })
    }
    
    log_message("Primary maps available.")
} else {
    maps_primary <- NULL
    log_message("Skipping primary maps stage by configuration.")
}

# --------------------------------------------------
# 11. PRIMARY METRICS
# --------------------------------------------------
if (CFG$run_primary_metrics) {
    if (is.null(brand_emb) || is.null(maps_primary)) stop("Primary metrics requested, but maps or brand embeddings are NULL.")
    
    primary_metrics <- time_step("evaluate_primary_maps", {
        res <- data.table::rbindlist(list(
            cbind(model = "sbert", projection = "pca",  evaluate_map(brand_emb$sbert$mean, maps_primary$sbert$pca)),
            cbind(model = "sbert", projection = "umap", evaluate_map(brand_emb$sbert$mean, maps_primary$sbert$umap)),
            cbind(model = "sbert", projection = "tsne", evaluate_map(brand_emb$sbert$mean, maps_primary$sbert$tsne)),
            cbind(model = "w2v",   projection = "pca",  evaluate_map(brand_emb$w2v$mean,   maps_primary$w2v$pca)),
            cbind(model = "w2v",   projection = "umap", evaluate_map(brand_emb$w2v$mean,   maps_primary$w2v$umap)),
            cbind(model = "w2v",   projection = "tsne", evaluate_map(brand_emb$w2v$mean,   maps_primary$w2v$tsne)),
            cbind(model = "tfidf", projection = "pca",  evaluate_map(brand_emb$tfidf$mean, maps_primary$tfidf$pca)),
            cbind(model = "tfidf", projection = "umap", evaluate_map(brand_emb$tfidf$mean, maps_primary$tfidf$umap)),
            cbind(model = "tfidf", projection = "tsne", evaluate_map(brand_emb$tfidf$mean, maps_primary$tfidf$tsne))
        ), fill = TRUE)
        res
    })
    
    data.table::fwrite(
        primary_metrics,
        file.path("outputs/tables", paste0("primary_metrics_", suffix, ".csv"))
    )
    
    log_message("Primary metrics written successfully.")
} else {
    primary_metrics <- NULL
    log_message("Skipping primary metrics stage by configuration.")
}

# --------------------------------------------------
# 12. BOOTSTRAP STABILITY
# --------------------------------------------------
if (CFG$run_bootstrap) {
    if (is.null(reps)) stop("Bootstrap requested, but 'reps' is NULL.")
    
    boot_sbert_umap <- time_step("bootstrap_brand_stability_sbert_umap", {
        bootstrap_brand_stability_dense(
            review_matrix = reps$sbert_review,
            brands = reviews_clean$brand,
            weights = NULL,
            B = CFG$bootstrap_B,
            projection = "umap",
            seed = CFG$seed
        )
    })
    
    data.table::fwrite(boot_sbert_umap, PATHS$bootstrap)
    log_message("Bootstrap results written successfully.")
} else {
    log_message("Skipping bootstrap stage by configuration.")
}

# --------------------------------------------------
# 13. MANUAL VALIDATION PACKET
# --------------------------------------------------
if (CFG$run_manual_validation) {
    if (is.null(maps_primary)) stop("Manual validation requested, but maps are NULL.")
    
    brand_packet <- time_step("make_brand_packet", {
        make_brand_packet(
            dt = reviews_clean,
            map_coords = maps_primary$sbert$umap,
            n_top_reviews = 5L
        )
    })
    
    validation_template <- time_step("make_validation_template", {
        make_validation_template(
            packet = brand_packet,
            sample_n = 24L,
            seed = CFG$seed
        )
    })
    
    log_message("Manual validation packet and template created.")
} else {
    log_message("Skipping manual validation packet stage by configuration.")
}

# --------------------------------------------------
# 14. EXTERNAL VALIDATION
# --------------------------------------------------
if (CFG$run_external_validation) {
    coding_file <- "data/manual_coding/brand_labels_final.csv"
    
    if (!file.exists(coding_file)) {
        stop("External validation requested, but coding file does not exist: ", coding_file)
    }
    
    ext_sbert <- time_step("external_validation_sbert_umap", {
        compare_labels_to_map(
            map_coords = maps_primary$sbert$umap,
            brand_matrix = brand_emb$sbert$mean,
            coding_file = coding_file
        )
    })
    
    capture.output(ext_sbert$adonis, file = file.path("outputs/tables", paste0("adonis_", suffix, ".txt")))
    capture.output(ext_sbert$envfit, file = file.path("outputs/tables", paste0("envfit_", suffix, ".txt")))
    data.table::fwrite(ext_sbert$same_label, file.path("outputs/tables", paste0("same_label_", suffix, ".csv")))
    
    log_message("External validation outputs written successfully.")
} else {
    log_message("Skipping external validation stage by configuration.")
}

# --------------------------------------------------
# 15. FULL ROBUSTNESS GRID
# --------------------------------------------------
if (CFG$run_full_grid) {
    if (is.null(brand_emb)) stop("Full grid requested, but 'brand_emb' is NULL.")
    
    grid_res <- time_step("run_full_robustness_grid", {
        run_grid(brand_emb)
    })
    
    data.table::fwrite(grid_res, PATHS$grid)
    log_message("Full robustness grid written successfully.")
} else {
    log_message("Skipping full robustness grid by configuration.")
}

# --------------------------------------------------
# 16. FIGURES
# --------------------------------------------------
if (CFG$run_figures) {
    if (is.null(maps_primary)) stop("Figures requested, but maps are NULL.")
    
    brand_summary <- reviews_clean[, .(n_reviews = .N), by = brand]
    
    time_step("save_primary_figures", {
        plot_brand_map(
            coords = maps_primary$sbert$umap,
            brand_summary = brand_summary,
            title = "Sentence-BERT + UMAP Brand Perceptual Map",
            label_top_n = 30L,
            out_file = file.path("outputs/figures", paste0("map_sbert_umap_", suffix, ".pdf"))
        )
        
        plot_brand_map(
            coords = maps_primary$sbert$pca,
            brand_summary = brand_summary,
            title = "Sentence-BERT + PCA Brand Perceptual Map",
            label_top_n = 30L,
            out_file = file.path("outputs/figures", paste0("map_sbert_pca_", suffix, ".pdf"))
        )
        
        if (!is.null(primary_metrics)) {
            plot_primary_metrics(
                metrics_df = primary_metrics,
                out_file = file.path("outputs/figures", paste0("primary_metrics_", suffix, ".pdf"))
            )
        }
    })
    
    log_message("Figures written successfully.")
} else {
    log_message("Skipping figure generation stage by configuration.")
}

# --------------------------------------------------
# 17. FINAL SUMMARY
# --------------------------------------------------
log_message("Pipeline completed successfully.")
log_message("Dataset version: ", CFG$dataset_version)
log_message("Suffix: ", suffix)

session_info_file <- file.path("outputs/tables", paste0("session_info_", suffix, ".txt"))
capture.output(sessionInfo(), file = session_info_file)

log_message("sessionInfo() written to: ", session_info_file)