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

default_tolerances <- data.frame(
  kode = default_weights$kode,
  diagnose = default_weights$diagnose,
  neg = c(0.35, 0.30, 0.25, 0.25, 0.35, 0.30, 0.22, 0.28, 0.30),
  det = c(0.30, 0.22, 0.30, 0.18, 0.22, 0.18, 0.35, 0.32, 0.25),
  ant = c(0.20, 0.30, 0.18, 0.32, 0.25, 0.30, 0.18, 0.15, 0.15),
  dis = c(0.15, 0.18, 0.15, 0.30, 0.32, 0.25, 0.18, 0.12, 0.12),
  psy = c(0.35, 0.15, 0.15, 0.12, 0.12, 0.12, 0.12, 0.10, 0.10)
)

default_bias <- data.frame(
  kode = default_weights$kode,
  diagnose = default_weights$diagnose,
  bias = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
)

trait_ids <- c("neg", "det", "ant", "dis", "psy")
default_trait_value <- 3
score_range <- c(0, 6)
default_weights_path <- "tuned_weights.csv"
mismatch_tolerances_path <- "tuned_tolerances.csv"
bias_path <- "tuned_bias.csv"
mismatch_strength <- 0.02
mismatch_weights <- c(
  neg = 0.01,
  det = 0.04,
  ant = 0.02,
  dis = 0.04,
  psy = 0.01
)

load_trait_frame <- function(path, fallback, value_columns = trait_ids) {
  if (!file.exists(path)) {
    return(fallback)
  }

  loaded <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required_columns <- c("kode", "diagnose", value_columns)

  if (!all(required_columns %in% names(loaded))) {
    warning("Ignorerer vektfil med ugyldige kolonner: ", path)
    return(fallback)
  }

  loaded <- loaded[, required_columns]
  loaded[, value_columns] <- lapply(loaded[, value_columns, drop = FALSE], as.numeric)
  loaded
}

load_weights <- function(path = default_weights_path, fallback = default_weights) {
  load_trait_frame(path = path, fallback = fallback, value_columns = trait_ids)
}

save_weights <- function(weights_df, path = default_weights_path) {
  write.csv(weights_df[, c("kode", "diagnose", trait_ids)], path, row.names = FALSE)
  invisible(path)
}

load_tolerances <- function(path = mismatch_tolerances_path, fallback = default_tolerances) {
  load_trait_frame(path = path, fallback = fallback, value_columns = trait_ids)
}

save_tolerances <- function(tolerances_df, path = mismatch_tolerances_path) {
  write.csv(tolerances_df[, c("kode", "diagnose", trait_ids)], path, row.names = FALSE)
  invisible(path)
}

load_bias <- function(path = bias_path, fallback = default_bias) {
  load_trait_frame(path = path, fallback = fallback, value_columns = "bias")
}

save_bias <- function(bias_df, path = bias_path) {
  write.csv(bias_df[, c("kode", "diagnose", "bias")], path, row.names = FALSE)
  invisible(path)
}

weights <- load_weights()
tolerances <- load_tolerances()
bias <- load_bias()

raw_profile_score <- function(user_traits, diagnosis_weights, diagnosis_tolerances, diagnosis_bias = 0, mismatch_penalty = mismatch_strength) {
  alignment <- sum(user_traits * diagnosis_weights, na.rm = TRUE)
  tolerance_scaled_mismatch <- ((user_traits - diagnosis_weights)^2) / pmax(diagnosis_tolerances, 1e-6)
  mismatch <- sum(tolerance_scaled_mismatch * mismatch_weights[names(diagnosis_weights)], na.rm = TRUE)
  alignment - mismatch_penalty * mismatch + diagnosis_bias
}

compute_scores <- function(user_traits, lambda, weights_df = weights, tolerances_df = tolerances, bias_df = bias) {
  raw_scores <- vapply(
    seq_len(nrow(weights_df)),
    function(i) {
      raw_profile_score(
        user_traits = user_traits,
        diagnosis_weights = as.numeric(weights_df[i, trait_ids, drop = TRUE]),
        diagnosis_tolerances = as.numeric(tolerances_df[i, trait_ids, drop = TRUE]),
        diagnosis_bias = bias_df$bias[i]
      )
    },
    numeric(1)
  )
  spread <- max(raw_scores) - min(raw_scores)

  if (isTRUE(all.equal(spread, 0))) {
    scaled_scores <- rep(mean(score_range), length(raw_scores))
  } else {
    scaled_scores <- (raw_scores - min(raw_scores)) / spread
    scaled_scores <- (scaled_scores^(1 / lambda)) * diff(score_range) + score_range[1]
  }

  pmin(pmax(scaled_scores, score_range[1]), score_range[2])
}

rank_profile <- function(traits, lambda = 1, weights_df = weights, tolerances_df = tolerances, bias_df = bias, scale_max = score_range[2]) {
  scores <- compute_scores(
    user_traits = traits / scale_max,
    lambda = lambda,
    weights_df = weights_df,
    tolerances_df = tolerances_df,
    bias_df = bias_df
  )
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

tolerances_from_vector <- function(x, template = default_tolerances) {
  updated <- template
  updated[, trait_ids] <- matrix(x, nrow = nrow(template), ncol = length(trait_ids), byrow = FALSE)
  updated
}

tolerances_to_vector <- function(tolerances_df = default_tolerances) {
  as.numeric(as.matrix(tolerances_df[, trait_ids, drop = FALSE]))
}

bias_from_vector <- function(x, template = default_bias) {
  updated <- template
  updated$bias <- x
  updated
}

bias_to_vector <- function(bias_df = default_bias) {
  as.numeric(bias_df$bias)
}
