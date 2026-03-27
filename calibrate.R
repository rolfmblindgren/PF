source("model.R", local = TRUE)

anchor_profiles <- do.call(
  rbind,
  list(
    data.frame(profile_id = "schizotyp_core", neg = 3, det = 5, ant = 1, dis = 0, psy = 6, expected_top1 = "F21", acceptable_top3 = "F21|F60.1|F60.6", forbidden_top1 = "F60.2", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "schizoid_core", neg = 1, det = 6, ant = 0, dis = 0, psy = 0, expected_top1 = "F60.1", acceptable_top3 = "F60.1|F60.6|F21", forbidden_top1 = "F60.2", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "paranoid_core", neg = 2, det = 1, ant = 5, dis = 0, psy = 0, expected_top1 = "F60.0", acceptable_top3 = "F60.0|F60.2|F60.4", forbidden_top1 = "F60.6", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "dyssosial_core", neg = 1, det = 0, ant = 6, dis = 6, psy = 0, expected_top1 = "F60.2", acceptable_top3 = "F60.2|F60.4|F60.3", forbidden_top1 = "F60.6", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "borderline_core", neg = 6, det = 2, ant = 3, dis = 6, psy = 0, expected_top1 = "F60.3", acceptable_top3 = "F60.3|F60.4|F60.7", forbidden_top1 = "F60.5", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "histrionisk_core", neg = 2, det = 1, ant = 5, dis = 5, psy = 0, expected_top1 = "F60.4", acceptable_top3 = "F60.4|F60.2|F60.3", forbidden_top1 = "F60.5", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "tvangspreget_core", neg = 3, det = 1, ant = 1, dis = 2, psy = 0, expected_top1 = "F60.5", acceptable_top3 = "F60.5|F60.6|F60.7", forbidden_top1 = "F60.2", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "unnvikende_core", neg = 5, det = 6, ant = 0, dis = 0, psy = 0, expected_top1 = "F60.6", acceptable_top3 = "F60.6|F60.7|F60.1", forbidden_top1 = "F60.2", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "avhengig_core", neg = 4, det = 3, ant = 0, dis = 0, psy = 0, expected_top1 = "F60.7", acceptable_top3 = "F60.7|F60.6|F60.3", forbidden_top1 = "F60.2", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "paranoid_vs_dyssosial", neg = 2, det = 1, ant = 6, dis = 4, psy = 0, expected_top1 = "F60.0", acceptable_top3 = "F60.0|F60.2|F60.4", forbidden_top1 = "F60.6", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "unnvikende_vs_avhengig", neg = 5, det = 5, ant = 0, dis = 0, psy = 0, expected_top1 = "F60.6", acceptable_top3 = "F60.6|F60.7|F60.1", forbidden_top1 = "F60.2", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "tvangspreget_vs_borderline", neg = 3, det = 1, ant = 1, dis = 1, psy = 0, expected_top1 = "F60.5", acceptable_top3 = "F60.5|F60.6|F60.7", forbidden_top1 = "F60.3", weight = 3, stringsAsFactors = FALSE),
    data.frame(profile_id = "histrionisk_vs_dyssosial", neg = 5, det = 0, ant = 1, dis = 1, psy = 0, expected_top1 = "F60.4", acceptable_top3 = "F60.4|F60.2|F60.3", forbidden_top1 = "F60.2", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "mixed_externalizing", neg = 3, det = 1, ant = 5, dis = 5, psy = 0, expected_top1 = NA, acceptable_top3 = "F60.3|F60.4|F60.2", forbidden_top1 = "F60.5", weight = 2, stringsAsFactors = FALSE),
    data.frame(profile_id = "flat_profile", neg = 3, det = 3, ant = 3, dis = 3, psy = 3, expected_top1 = NA, acceptable_top3 = "F60.3|F60.4|F60.5", forbidden_top1 = "", weight = 1, stringsAsFactors = FALSE)
  )
)

evaluate_anchor <- function(anchor_row, weights_df, lambda = 1) {
  traits <- as.numeric(anchor_row[trait_ids])
  ranked <- rank_profile(traits = traits, lambda = lambda, weights_df = weights_df)
  top_codes <- ranked$kode[seq_len(min(3, nrow(ranked)))]
  expected_top1 <- anchor_row[["expected_top1"]]
  acceptable_top3 <- strsplit(anchor_row[["acceptable_top3"]], "\\|")[[1]]
  forbidden_top1 <- anchor_row[["forbidden_top1"]]
  case_weight <- as.numeric(anchor_row[["weight"]])

  penalty <- 0
  rank_expected <- NA_integer_

  if (!is.na(expected_top1)) {
    rank_expected <- match(expected_top1, ranked$kode)
    penalty <- penalty + (rank_expected - 1) * 3

    if (!identical(top_codes[1], expected_top1)) {
      penalty <- penalty + 2
    }
  }

  if (!any(top_codes %in% acceptable_top3)) {
    penalty <- penalty + 8
  }

  if (nzchar(forbidden_top1) && identical(top_codes[1], forbidden_top1)) {
    penalty <- penalty + 10
  }

  weighted_penalty <- penalty * case_weight

  data.frame(
    profile_id = anchor_row[["profile_id"]],
    expected_top1 = expected_top1,
    predicted_top1 = top_codes[1],
    predicted_top3 = paste(top_codes, collapse = "|"),
    rank_expected = rank_expected,
    forbidden_top1 = forbidden_top1,
    case_weight = case_weight,
    penalty = penalty,
    weighted_penalty = weighted_penalty,
    stringsAsFactors = FALSE
  )
}

evaluate_weights <- function(weights_df, anchors = anchor_profiles, lambda = 1) {
  details <- do.call(
    rbind,
    lapply(seq_len(nrow(anchors)), function(i) evaluate_anchor(anchors[i, , drop = FALSE], weights_df, lambda = lambda))
  )

  list(
    loss = sum(details$weighted_penalty),
    details = details
  )
}

objective_function <- function(x, anchors = anchor_profiles, lambda = 1, template = default_weights) {
  weights_df <- weights_from_vector(x, template = template)
  eval <- evaluate_weights(weights_df = weights_df, anchors = anchors, lambda = lambda)

  # Mild regularization keeps weights from drifting too far from the hand-tuned starting point.
  regularization <- 0.25 * sum((x - weights_to_vector(template))^2)
  eval$loss + regularization
}

random_search_optimize <- function(
  initial_vector,
  anchors = anchor_profiles,
  lambda = 1,
  template = default_weights,
  iterations = 800,
  step_size = 0.12,
  seed = 123
) {
  set.seed(seed)

  best_vector <- initial_vector
  best_value <- objective_function(best_vector, anchors = anchors, lambda = lambda, template = template)
  accepted <- 0L

  for (i in seq_len(iterations)) {
    candidate <- best_vector + rnorm(length(best_vector), mean = 0, sd = step_size)
    candidate <- pmin(pmax(candidate, 0), 1)
    candidate_value <- objective_function(candidate, anchors = anchors, lambda = lambda, template = template)

    if (candidate_value < best_value) {
      best_vector <- candidate
      best_value <- candidate_value
      accepted <- accepted + 1L
    }
  }

  list(
    par = best_vector,
    value = best_value,
    convergence = 0L,
    counts = c(`function` = iterations),
    accepted = accepted,
    method = "random_search"
  )
}

grid_profiles <- function(scale_values = 0:6) {
  grid <- expand.grid(
    neg = scale_values,
    det = scale_values,
    ant = scale_values,
    dis = scale_values,
    psy = scale_values
  )

  grid$profile_id <- paste0("grid_", seq_len(nrow(grid)))
  grid
}

summarize_grid <- function(weights_df, lambda = 1, scale_values = 0:6) {
  grid <- grid_profiles(scale_values = scale_values)
  ranked_codes <- apply(grid[, trait_ids, drop = FALSE], 1, function(traits) {
    rank_profile(as.numeric(traits), lambda = lambda, weights_df = weights_df)$kode[1]
  })

  counts <- sort(table(ranked_codes), decreasing = TRUE)
  data.frame(
    kode = names(counts),
    n_profiles = as.integer(counts),
    share = as.numeric(counts) / nrow(grid),
    row.names = NULL
  )
}

compare_grid_summary <- function(baseline_weights, tuned_weights, lambda = 1, scale_values = 0:6) {
  baseline <- summarize_grid(baseline_weights, lambda = lambda, scale_values = scale_values)
  tuned <- summarize_grid(tuned_weights, lambda = lambda, scale_values = scale_values)
  merged <- merge(
    baseline,
    tuned,
    by = "kode",
    all = TRUE,
    suffixes = c("_baseline", "_tuned")
  )
  merged[is.na(merged)] <- 0
  merged$delta_profiles <- merged$n_profiles_tuned - merged$n_profiles_baseline
  merged$delta_share <- merged$share_tuned - merged$share_baseline
  merged[order(-abs(merged$delta_profiles)), ]
}

write_calibration_outputs <- function(result, output_dir = "calibration_outputs", lambda = 1) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  details <- merge(
    result$baseline$details,
    result$tuned$details,
    by = "profile_id",
    suffixes = c("_baseline", "_tuned")
  )
  grid_comparison <- compare_grid_summary(default_weights, result$tuned_weights, lambda = lambda)

  write.csv(details, file.path(output_dir, "anchor_results.csv"), row.names = FALSE)
  write.csv(grid_comparison, file.path(output_dir, "grid_comparison.csv"), row.names = FALSE)
  save_weights(result$tuned_weights, file.path(output_dir, "tuned_weights.csv"))

  invisible(output_dir)
}

run_calibration <- function(lambda = 1, iterations = 800, step_size = 0.12, seed = 123) {
  initial_vector <- weights_to_vector(default_weights)
  baseline <- evaluate_weights(weights, lambda = lambda)

  fit <- random_search_optimize(
    initial_vector = initial_vector,
    anchors = anchor_profiles,
    lambda = lambda,
    template = default_weights,
    iterations = iterations,
    step_size = step_size,
    seed = seed
  )

  tuned_weights <- weights_from_vector(fit$par, template = default_weights)
  tuned <- evaluate_weights(tuned_weights, lambda = lambda)

  list(
    baseline = baseline,
    tuned = tuned,
    tuned_weights = tuned_weights,
    optim = fit
  )
}

print_calibration_report <- function(result, lambda = 1, include_grid = TRUE) {
  cat("Kalibrering for lambda =", lambda, "\n\n")
  cat("Starttap:", result$baseline$loss, "\n")
  cat("Slutttap:", result$tuned$loss, "\n")
  cat("Metode:", result$optim$method, "\n")
  cat("Aksepterte forbedringer:", result$optim$accepted, "\n")
  cat("Konvergenskode:", result$optim$convergence, "\n\n")

  cat("Profiler som fortsatt har straff etter tuning:\n")
  remaining <- result$tuned$details[result$tuned$details$penalty > 0, , drop = FALSE]
  if (nrow(remaining) == 0) {
    cat("Ingen.\n\n")
  } else {
    print(remaining, row.names = FALSE)
    cat("\n")
  }

  cat("Oppdaterte vekter:\n")
  print(result$tuned_weights, row.names = FALSE)
  cat("\n")

  if (isTRUE(include_grid)) {
    cat("Fordeling av topp-1 på fullt 7^5-grid:\n")
    print(summarize_grid(result$tuned_weights, lambda = lambda), row.names = FALSE)
    cat("\n")
    cat("Endring mot baseline på fullt 7^5-grid:\n")
    print(compare_grid_summary(default_weights, result$tuned_weights, lambda = lambda), row.names = FALSE)
  }
}

if (sys.nframe() == 0) {
  result <- run_calibration()
  print_calibration_report(result)
  write_calibration_outputs(result)
}
