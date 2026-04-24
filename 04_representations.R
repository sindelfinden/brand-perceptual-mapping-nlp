# =========================
# 04_representations.R
# =========================

library(quanteda)
library(word2vec)
library(text2vec)
library(Matrix)
library(data.table)
library(reticulate)
library(tidytext)

if (!exists("SEED")) SEED <- 42L
set.seed(SEED)

l2_normalize_rows <- function(mat) {
    if (inherits(mat, "sparseMatrix")) {
        norms <- sqrt(as.numeric(Matrix::rowSums(mat * mat)))
        norms[is.na(norms) | norms == 0] <- 1
        out <- Matrix::Diagonal(x = 1 / norms) %*% mat
        return(as(out, "dgCMatrix"))
    } else {
        norms <- sqrt(rowSums(mat * mat))
        norms[is.na(norms) | norms == 0] <- 1
        return(mat / norms)
    }
}

aggregate_sparse_weighted_mean <- function(mat, groups, weights = NULL) {
    if (is.null(weights)) weights <- rep(1, nrow(mat))
    
    groups <- factor(groups)
    weights <- as.numeric(weights)
    
    denom <- tapply(weights, groups, sum)
    denom <- as.numeric(denom)
    names(denom) <- levels(groups)
    
    scaled_w <- weights / denom[as.character(groups)]
    scaled_w <- as.numeric(scaled_w)
    
    G <- Matrix::sparseMatrix(
        i = as.integer(groups),
        j = seq_along(groups),
        x = scaled_w,
        dims = c(nlevels(groups), length(groups))
    )
    
    out <- G %*% mat
    rownames(out) <- levels(groups)
    out
}

aggregate_dense_weighted_mean <- function(mat, groups, weights = NULL) {
    if (is.null(weights)) weights <- rep(1, nrow(mat))
    
    groups <- factor(groups)
    split_idx <- split(seq_len(nrow(mat)), groups)
    
    out <- t(vapply(split_idx, function(idx) {
        w <- as.numeric(weights[idx])
        w <- w / sum(w)
        colSums(mat[idx, , drop = FALSE] * w)
    }, numeric(ncol(mat))))
    
    rownames(out) <- names(split_idx)
    out
}

build_representations <- function(dt) {
    
    # ------------------------------------------------------------
    # 0. Basic defensive checks
    # ------------------------------------------------------------
    stopifnot(is.data.table(dt) || is.data.frame(dt))
    stopifnot(all(c("doc_id", "text_raw") %in% names(dt)))
    
    dt <- as.data.table(dt)
    
    # ------------------------------------------------------------
    # 1. Tokenization
    # ------------------------------------------------------------
    toks <- make_tokens(dt$text_raw)
    toks_list <- as.list(toks)
    
    # ------------------------------------------------------------
    # 2. TF-IDF
    # ------------------------------------------------------------
    dfm_ <- quanteda::dfm(toks)
    dfm_ <- quanteda::dfm_trim(dfm_, min_termfreq = 5L)
    
    tfidf_review <- quanteda::dfm_tfidf(dfm_, scheme_tf = "prop")
    tfidf_review <- as(tfidf_review, "dgCMatrix")
    
    # Optional tidytext long view for later interpretation
    tidy_counts <- data.table(
        doc_id = rep(dt$doc_id, lengths(toks_list)),
        term = unlist(toks_list, use.names = FALSE)
    )[, .N, by = .(doc_id, term)]
    
    tidy_tfidf <- tidytext::bind_tf_idf(
        tbl = as.data.frame(tidy_counts),
        term = term,
        document = doc_id,
        n = N
    )
    
    # ------------------------------------------------------------
    # 3. Word2Vec
    # ------------------------------------------------------------
    w2v_model <- word2vec::word2vec(
        x = toks_list,
        type = "skip-gram",
        dim = 200L,
        window = 10L,
        iter = 15L,
        min_count = 5L,
        negative = 10L,
        sample = 0.001,
        threads = max(1L, parallel::detectCores() - 1L)
    )
    
    w2v_vocab <- as.matrix(w2v_model)
    
    doc_from_tokens <- function(tok, emb) {
        tok <- tok[tok %in% rownames(emb)]
        if (!length(tok)) return(rep(0, ncol(emb)))
        colMeans(emb[tok, , drop = FALSE])
    }
    
    w2v_review <- t(vapply(
        toks_list,
        doc_from_tokens,
        numeric(ncol(w2v_vocab)),
        emb = w2v_vocab
    ))
    
    rownames(w2v_review) <- dt$doc_id
    w2v_review <- l2_normalize_rows(w2v_review)
    
    # ------------------------------------------------------------
    # 4. Sentence-BERT, GPU-aware
    # ------------------------------------------------------------
    mods <- setup_python_sbert()
    st <- mods$st
    torch <- mods$torch
    
    # Re-seed Python side for reproducibility
    py_run_string(sprintf("
import os, random, numpy as np
os.environ['PYTHONHASHSEED'] = '%d'
random.seed(%d)
np.random.seed(%d)
try:
    import torch
    torch.manual_seed(%d)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(%d)
except Exception:
    pass
", SEED, SEED, SEED, SEED, SEED))
    
    # Explicit device selection
    device <- if (isTRUE(torch$cuda$is_available())) "cuda" else "cpu"
    
    # Load model explicitly on selected device
    model <- st$SentenceTransformer(
        "sentence-transformers/all-MiniLM-L6-v2",
        device = device
    )
    
    # Conservative batch sizing for GTX 960
    batch_size <- if (device == "cuda") 64L else 32L
    
    # Preallocate by chunks
    starts <- seq(1L, nrow(dt), by = batch_size)
    sbert_chunks <- vector("list", length(starts))
    
    for (ii in seq_along(starts)) {
        s <- starts[ii]
        idx <- s:min(s + batch_size - 1L, nrow(dt))
        
        emb <- model$encode(
            sentences = as.list(dt$text_raw[idx]),
            batch_size = as.integer(batch_size),
            show_progress_bar = FALSE,
            convert_to_numpy = TRUE,
            normalize_embeddings = FALSE,
            device = device
        )
        
        sbert_chunks[[ii]] <- as.matrix(emb)
    }
    
    sbert_review <- do.call(rbind, sbert_chunks)
    rownames(sbert_review) <- dt$doc_id
    sbert_review <- l2_normalize_rows(sbert_review)
    
    # Optional GPU memory cleanup after inference
    if (device == "cuda") {
        try(torch$cuda$empty_cache(), silent = TRUE)
    }
    
    # ------------------------------------------------------------
    # 5. Return all representations
    # ------------------------------------------------------------
    list(
        tfidf_review = tfidf_review,
        w2v_review = w2v_review,
        sbert_review = sbert_review,
        tidy_tfidf = tidy_tfidf,
        sbert_device = device,
        sbert_batch_size = batch_size
    )
}