# ----------- __________________
# 02_load_data.R
# ----------- __________________

library(jsonlite)
library(data.table)

read_jsonl_gz <- function(path, chunk_size = 5000L) {
    con <- gzfile(path, open = "rt")
    on.exit(close(con), add = TRUE)
    
    out <- list()
    i <- 1L
    
    normalize_record <- function(rec) {
        rec <- as.list(rec)
        
        for (nm in names(rec)) {
            val <- rec[[nm]]
            
            # Keep only true scalar atomic values as regular columns
            if (is.null(val)) {
                rec[[nm]] <- list(NULL)
            } else if (is.atomic(val) && length(val) == 1L && is.null(dim(val))) {
                rec[[nm]] <- val
            } else {
                # Everything else becomes a list-column
                rec[[nm]] <- list(val)
            }
        }
        
        rec
    }
    
    repeat {
        lines <- readLines(con, n = chunk_size, warn = FALSE, encoding = "UTF-8")
        if (!length(lines)) break
        
        parsed <- lapply(lines, function(x) {
            rec <- jsonlite::parse_json(x, simplifyVector = FALSE)
            normalize_record(rec)
        })
        
        out[[i]] <- data.table::rbindlist(parsed, fill = TRUE, use.names = TRUE)
        i <- i + 1L
    }
    
    data.table::rbindlist(out, fill = TRUE, use.names = TRUE)
}

flatten_list_col <- function(x) {
    vapply(x, function(z) {
        if (is.null(z) || length(z) == 0L) return(NA_character_)
        z <- unlist(z, recursive = TRUE, use.names = FALSE)
        z <- z[!is.na(z) & nzchar(trimws(z))]
        if (!length(z)) return(NA_character_)
        paste(z, collapse = " | ")
    }, character(1))
}

normalize_brand <- function(x) {
    x <- trimws(as.character(x))
    x <- gsub("[[:cntrl:]]", " ", x)
    x <- gsub("[^[:alnum:]]+", " ", toupper(x))
    x <- gsub("\\s+", " ", x)
    x[x %in% c("", "NA", "NONE", "NULL")] <- NA_character_
    x
}

parse_brand_from_details_2023 <- function(details_col) {
    vapply(details_col, function(d) {
        if (is.null(d) || length(d) == 0L) return(NA_character_)
        if (is.character(d) && length(d) == 1L) {
            # Some loaders may keep details as a JSON string
            d <- tryCatch(jsonlite::parse_json(d, simplifyVector = TRUE), error = function(e) NULL)
        }
        if (is.null(d)) return(NA_character_)
        
        # Common brand-like keys; extend only if your audit shows a need
        keys <- c("Brand", "Manufacturer", "Brand Name")
        vals <- unname(unlist(d[keys], recursive = TRUE, use.names = FALSE))
        vals <- vals[!is.na(vals) & nzchar(trimws(vals))]
        if (!length(vals)) NA_character_ else vals[1]
    }, character(1))
}

load_amazon_all_beauty <- function(version = c("2018", "2023")) {
    version <- match.arg(version)
    
    if (version == "2018") {
        rev <- read_jsonl_gz("data/raw/All_Beauty_2018.json.gz")
        met <- read_jsonl_gz("data/raw/meta_All_Beauty_2018.json.gz")
        
        reviews <- data.table(
            doc_id = seq_len(nrow(rev)),
            item_id = rev$asin,
            variant_asin = NA_character_,
            rating = as.numeric(rev$overall),
            helpful_vote = suppressWarnings(as.numeric(gsub(",", "", as.character(rev$vote)))),
            verified_purchase = as.logical(rev$verified),
            timestamp = as.integer(rev$unixReviewTime),
            review_title = as.character(rev$summary),
            review_text = as.character(rev$reviewText)
        )
        
        meta <- data.table(
            item_id = met$asin,
            product_title = as.character(met$title),
            brand = normalize_brand(met$brand),
            features = if ("feature" %in% names(met)) flatten_list_col(met$feature) else NA_character_,
            description = if ("description" %in% names(met)) flatten_list_col(met$description) else NA_character_,
            categories = if ("categories" %in% names(met)) flatten_list_col(met$categories) else NA_character_
        )
        
    } else {
        rev <- read_jsonl_gz("data/raw/All_Beauty_2023.jsonl.gz")
        met <- read_jsonl_gz("data/raw/meta_All_Beauty_2023.jsonl.gz")
        
        reviews <- data.table(
            doc_id = seq_len(nrow(rev)),
            item_id = rev$parent_asin,        # official 2023 join key
            variant_asin = rev$asin,
            rating = as.numeric(rev$rating),
            helpful_vote = as.numeric(rev$helpful_vote),
            verified_purchase = as.logical(rev$verified_purchase),
            timestamp = as.numeric(rev$timestamp),
            review_title = as.character(rev$title),
            review_text = as.character(rev$text)
        )
        
        meta <- data.table(
            item_id = met$parent_asin,
            product_title = as.character(met$title),
            store = as.character(met$store),
            details = met$details,
            features = if ("features" %in% names(met)) flatten_list_col(met$features) else NA_character_,
            description = if ("description" %in% names(met)) flatten_list_col(met$description) else NA_character_,
            categories = if ("categories" %in% names(met)) flatten_list_col(met$categories) else NA_character_
        )
        
        meta[, brand := normalize_brand(fifelse(
            !is.na(store) & nzchar(store),
            store,
            parse_brand_from_details_2023(details)
        ))]
    }
    
    meta <- unique(meta, by = "item_id")
    merge(reviews, meta, by = "item_id", all.x = TRUE)
}

