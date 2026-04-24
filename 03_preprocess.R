# ----------- __________________
# 03_preprocess.R
# ----------- __________________

library(data.table)
library(quanteda)
library(stopwords)

normalize_text <- function(x) {
    x <- enc2utf8(ifelse(is.na(x), "", x))
    x <- gsub("<[^>]+>", " ", x, perl = TRUE)                     # HTML tags
    x <- gsub("https?://\\S+|www\\.\\S+", " ", x, perl = TRUE)    # URLs
    x <- gsub("&amp;", " and ", x, fixed = TRUE)
    x <- gsub("\\bwon['’]?t\\b", "will not", x, ignore.case = TRUE)
    x <- gsub("\\bcan['’]?t\\b", "can not", x, ignore.case = TRUE)
    x <- gsub("n['’]t\\b", " not", x, perl = TRUE)                # isn't -> is not
    x <- gsub("[[:cntrl:]]", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(tolower(x))
}

detect_lang_safe <- function(x) {
    if (!requireNamespace("cld3", quietly = TRUE)) {
        return(rep(NA_character_, length(x)))
    }
    vapply(x, function(txt) {
        if (is.na(txt) || !nzchar(txt) || nchar(txt) < 20L) return(NA_character_)
        tryCatch(cld3::detect_language(txt), error = function(e) NA_character_)
    }, character(1))
}

prep_reviews <- function(dt, min_chars = 20L, min_brand_reviews = 50L, english_only = TRUE) {
    dt <- copy(dt)
    
    dt[, text_raw := normalize_text(paste(review_title, review_text))]
    dt[text_raw == "", text_raw := NA_character_]
    
    # Exact duplicates by key
    dup_key <- intersect(c("item_id", "timestamp", "text_raw"), names(dt))
    dt <- unique(dt, by = dup_key)
    
    # Language filter
    dt[, lang := detect_lang_safe(substr(text_raw, 1L, 1000L))]
    if (english_only) dt <- dt[is.na(lang) | lang == "en"]
    
    # Review and brand support filters
    dt <- dt[!is.na(text_raw) & !is.na(brand) & nchar(text_raw) >= min_chars]
    
    bc <- dt[, .N, by = brand]
    dt <- dt[brand %in% bc[N >= min_brand_reviews, brand]]
    
    # Weights for alternative aggregation
    dt[, rating_weight := pmax(rating, 1)]
    dt[, helpful_weight := 1 + log1p(fifelse(is.na(helpful_vote), 0, helpful_vote))]
    
    dt
}

make_tokens <- function(text_vec) {
    toks <- quanteda::tokens(
        text_vec,
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE
    )
    
    # Light-touch negation compounding: "not greasy" -> "not_greasy"
    toks <- quanteda::tokens_compound(
        toks,
        pattern = c("not", "no", "never"),
        window = c(0, 1),
        join = FALSE
    )
    
    toks <- quanteda::tokens_remove(toks, stopwords::stopwords("en"))
    toks
}
