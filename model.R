default_weights <- data.frame(
  kode = c("F21", "F60.0", "F60.1", "F60.2", "F60.3", "F60.4", "F60.5", "F60.6", "F60.7"),
  diagnose = c(
    "Schizotyp lidelse",
    "Paranoid personlighetsforstyrrelse",
    "Schizoid personlighetsforstyrrelse",
    "Dyssosial personlighetsforstyrrelse",
    "Emosjonelt ustabil personlighetsforstyrrelse (borderline-type)",
    "Histrionisk personlighetsforstyrrelse",
    "Tvangspreget personlighetsforstyrrelse",
    "Engstelig (unnvikende) personlighetsforstyrrelse",
    "Avhengig personlighetsforstyrrelse"
  ),
  neg = c(0.3, 0.4, 0.1, 0.0, 0.6, 0.1, 0.4, 0.5, 0.5),
  det = c(0.6, 0.1, 0.7, 0.0, 0.1, 0.1, 0.0, 0.7, 0.2),
  ant = c(0.1, 0.6, 0.1, 0.7, 0.3, 0.6, 0.1, 0.1, 0.1),
  dis = c(0.0, 0.0, 0.0, 0.6, 0.6, 0.6, 0.3, 0.0, 0.0),
  psy = c(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
)

trait_ids <- c("neg", "det", "ant", "dis", "psy")
default_trait_value <- 3
score_range <- c(0, 6)
default_weights_path <- "tuned_weights.csv"

load_weights <- function(path = default_weights_path, fallback = default_weights) {
  if (!file.exists(path)) {
    return(fallback)
  }

  loaded <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required_columns <- c("kode", "diagnose", trait_ids)

  if (!all(required_columns %in% names(loaded))) {
    warning("Ignorerer vektfil med ugyldige kolonner: ", path)
    return(fallback)
  }

  loaded <- loaded[, required_columns]
  loaded[, trait_ids] <- lapply(loaded[, trait_ids, drop = FALSE], as.numeric)
  loaded
}

save_weights <- function(weights_df, path = default_weights_path) {
  write.csv(weights_df[, c("kode", "diagnose", trait_ids)], path, row.names = FALSE)
  invisible(path)
}

weights <- load_weights()

compute_scores <- function(user_traits, lambda, weights_df = weights) {
  raw_scores <- apply(weights_df[, trait_ids, drop = FALSE], 1, function(w) sum(user_traits * w, na.rm = TRUE))
  spread <- max(raw_scores) - min(raw_scores)

  if (isTRUE(all.equal(spread, 0))) {
    scaled_scores <- rep(mean(score_range), length(raw_scores))
  } else {
    scaled_scores <- (raw_scores - min(raw_scores)) / spread
    scaled_scores <- (scaled_scores^(1 / lambda)) * diff(score_range) + score_range[1]
  }

  pmin(pmax(scaled_scores, score_range[1]), score_range[2])
}

rank_profile <- function(traits, lambda = 1, weights_df = weights, scale_max = score_range[2]) {
  scores <- compute_scores(user_traits = traits / scale_max, lambda = lambda, weights_df = weights_df)
  ranked <- weights_df
  ranked$score <- scores
  ranked[order(-ranked$score, ranked$diagnose), ]
}

weights_from_vector <- function(x, template = default_weights) {
  updated <- template
  updated[, trait_ids] <- matrix(x, nrow = nrow(template), ncol = length(trait_ids), byrow = FALSE)
  updated
}

weights_to_vector <- function(weights_df = default_weights) {
  as.numeric(as.matrix(weights_df[, trait_ids, drop = FALSE]))
}
