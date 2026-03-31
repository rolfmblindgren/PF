source("model.R", local = TRUE)

grid_profiles <- function(scale_values = 0:6) {
  expand.grid(
    neg = scale_values,
    det = scale_values,
    ant = scale_values,
    dis = scale_values,
    psy = scale_values
  )
}

score_grid <- function(
  weights_df = weights,
  tolerances_df = tolerances,
  bias_df = bias,
  scale_values = 0:6
) {
  grid <- grid_profiles(scale_values = scale_values)
  trait_matrix <- as.matrix(grid[, trait_ids, drop = FALSE]) / score_range[2]
  weight_matrix <- as.matrix(weights_df[, trait_ids, drop = FALSE])
  tolerance_matrix <- as.matrix(tolerances_df[, trait_ids, drop = FALSE])
  bias_vector <- bias_df$bias
  mismatch_weight_vector <- mismatch_weights[colnames(weight_matrix)]

  alignment <- trait_matrix %*% t(weight_matrix)
  mismatch <- sapply(
    seq_len(nrow(weight_matrix)),
    function(i) {
      rowSums(
        ((sweep(trait_matrix, 2, weight_matrix[i, ], "-"))^2 /
          pmax(tolerance_matrix[i, ], 1e-6)) *
          rep(mismatch_weight_vector, each = nrow(trait_matrix))
      )
    }
  )

  raw_scores <- alignment -
    mismatch_strength * mismatch +
    matrix(bias_vector, nrow = nrow(trait_matrix), ncol = length(bias_vector), byrow = TRUE)

  top1_idx <- max.col(raw_scores, ties.method = "first")
  top1_score <- raw_scores[cbind(seq_len(nrow(raw_scores)), top1_idx)]
  masked_scores <- raw_scores
  masked_scores[cbind(seq_len(nrow(masked_scores)), top1_idx)] <- -Inf
  top2_idx <- max.col(masked_scores, ties.method = "first")
  top2_score <- masked_scores[cbind(seq_len(nrow(masked_scores)), top2_idx)]

  data.frame(
    profile_id = paste0("grid_", seq_len(nrow(grid))),
    grid,
    total = rowSums(grid[, trait_ids, drop = FALSE]),
    spread = apply(grid[, trait_ids, drop = FALSE], 1, function(row) max(row) - min(row)),
    top1_kode = weights_df$kode[top1_idx],
    top1_diagnose = weights_df$diagnose[top1_idx],
    top1_score = top1_score,
    top2_kode = weights_df$kode[top2_idx],
    top2_diagnose = weights_df$diagnose[top2_idx],
    top2_score = top2_score,
    margin = top1_score - top2_score,
    row.names = NULL
  )
}

flag_counterintuitive_zones <- function(scored_grid) {
  checks <- list(
    list(
      rule_id = "flat_profile_schizotyp",
      description = "Flate profiler (spredning <= 1) som likevel gir schizotyp som topp-1.",
      data = subset(scored_grid, spread <= 1 & top1_kode == "F21")
    ),
    list(
      rule_id = "low_psy_schizotyp",
      description = "Profiler med lav psykotisisme (psy <= 1) som likevel gir schizotyp som topp-1.",
      data = subset(scored_grid, psy <= 1 & top1_kode == "F21")
    ),
    list(
      rule_id = "low_externalizing_dyssosial",
      description = "Profiler med lav antagonisme og disinhibisjon (<= 1) som likevel gir dyssosial som topp-1.",
      data = subset(scored_grid, ant <= 1 & dis <= 1 & top1_kode == "F60.2")
    ),
    list(
      rule_id = "near_zero_strong_type",
      description = "Nesten tomme profiler (sum <= 2) som likevel gir en tydelig typeprofil som topp-1.",
      data = subset(scored_grid, total <= 2 & top1_kode %in% c("F21", "F60.2", "F60.3", "F60.4", "F60.5"))
    ),
    list(
      rule_id = "high_psy_introverted_not_schizotyp",
      description = "Profiler med svært høy psykotisisme, tydelig tilbaketrukkenhet og lav eksternalisering som ikke gir schizotyp som topp-1.",
      data = subset(scored_grid, psy >= 5 & det >= 3 & ant <= 2 & dis <= 2 & top1_kode != "F21")
    ),
    list(
      rule_id = "orderly_but_not_obsessive",
      description = "Ordnede og hemmede profiler med lav psykotisisme (det >= 4, dis <= 1, ant <= 1, psy <= 1) som ikke gir tvangspreget som topp-1.",
      data = subset(scored_grid, det >= 4 & dis <= 1 & ant <= 1 & psy <= 1 & top1_kode != "F60.5")
    ),
    list(
      rule_id = "low_psy_controlled_still_schizotyp",
      description = "Kontrollerte profiler med lav psykotisisme (psy <= 1, dis <= 1, ant <= 1) som likevel gir schizotyp som topp-1.",
      data = subset(scored_grid, psy <= 1 & dis <= 1 & ant <= 1 & top1_kode == "F21")
    )
  )

  flagged <- do.call(
    rbind,
    lapply(checks, function(check) {
      if (nrow(check$data) == 0) {
        return(NULL)
      }

      transform(
        check$data,
        rule_id = check$rule_id,
        rule_description = check$description
      )
    })
  )

  if (is.null(flagged) || nrow(flagged) == 0) {
    return(data.frame())
  }

  flagged[, c(
    "rule_id", "rule_description", "profile_id", trait_ids, "total", "spread",
    "top1_kode", "top1_diagnose", "top2_kode", "top2_diagnose", "margin"
  )]
}

summarize_flags <- function(flagged_profiles, total_profiles) {
  if (nrow(flagged_profiles) == 0) {
    return(data.frame())
  }

  counts <- aggregate(profile_id ~ rule_id + rule_description, data = flagged_profiles, FUN = length)
  names(counts)[names(counts) == "profile_id"] <- "n_profiles"
  counts$share_of_grid <- counts$n_profiles / total_profiles
  counts[order(-counts$n_profiles, counts$rule_id), ]
}

top_examples_per_rule <- function(flagged_profiles, n_examples = 10) {
  if (nrow(flagged_profiles) == 0) {
    return(data.frame())
  }

  ordered <- flagged_profiles[order(flagged_profiles$rule_id, -flagged_profiles$margin), ]
  split_profiles <- split(ordered, ordered$rule_id)
  do.call(
    rbind,
    lapply(split_profiles, function(df) head(df, n_examples))
  )
}

top1_distribution <- function(scored_grid) {
  counts <- sort(table(scored_grid$top1_kode), decreasing = TRUE)
  data.frame(
    kode = names(counts),
    n_profiles = as.integer(counts),
    share = as.numeric(counts) / nrow(scored_grid),
    row.names = NULL
  )
}

top1_by_psy <- function(scored_grid) {
  counts <- aggregate(
    profile_id ~ psy + top1_kode,
    data = scored_grid,
    FUN = length
  )
  names(counts)[names(counts) == "profile_id"] <- "n_profiles"
  counts <- counts[order(counts$psy, -counts$n_profiles, counts$top1_kode), ]
  do.call(
    rbind,
    lapply(split(counts, counts$psy), function(df) head(df, 3))
  )
}

write_grid_report <- function(output_dir = "calibration_outputs") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  scored_grid <- score_grid()
  flagged_profiles <- flag_counterintuitive_zones(scored_grid)
  flag_summary <- summarize_flags(flagged_profiles, total_profiles = nrow(scored_grid))
  flag_examples <- top_examples_per_rule(flagged_profiles, n_examples = 10)
  distribution <- top1_distribution(scored_grid)
  by_psy <- top1_by_psy(scored_grid)

  write.csv(distribution, file.path(output_dir, "grid_top1_distribution.csv"), row.names = FALSE)
  write.csv(by_psy, file.path(output_dir, "grid_top1_by_psy.csv"), row.names = FALSE)
  write.csv(flag_summary, file.path(output_dir, "grid_flag_summary.csv"), row.names = FALSE)
  write.csv(flag_examples, file.path(output_dir, "grid_flagged_profiles.csv"), row.names = FALSE)

  report_path <- file.path(output_dir, "grid_report.md")
  report_lines <- c(
    "# Grid Report",
    "",
    sprintf("- Totalt antall profiler: %s", nrow(scored_grid)),
    sprintf("- Antall flaggede profiler: %s", nrow(flagged_profiles)),
    "",
    "## Topp-1-fordeling",
    ""
  )

  report_lines <- c(
    report_lines,
    apply(distribution, 1, function(row) {
      sprintf("- %s: %s profiler (%.1f%%)", row[["kode"]], row[["n_profiles"]], 100 * as.numeric(row[["share"]]))
    }),
    "",
    "## Mest interessante flagg",
    ""
  )

  if (nrow(flag_summary) == 0) {
    report_lines <- c(report_lines, "- Ingen flagg ble truffet.")
  } else {
    report_lines <- c(
      report_lines,
      apply(flag_summary, 1, function(row) {
        sprintf(
          "- %s: %s profiler (%.1f%%). %s",
          row[["rule_id"]],
          row[["n_profiles"]],
          100 * as.numeric(row[["share_of_grid"]]),
          row[["rule_description"]]
        )
      })
    )
  }

  writeLines(report_lines, report_path)

  invisible(list(
    scored_grid = scored_grid,
    flagged_profiles = flagged_profiles,
    flag_summary = flag_summary,
    distribution = distribution,
    by_psy = by_psy,
    report_path = report_path
  ))
}

if (sys.nframe() == 0) {
  result <- write_grid_report()
  cat("Skrev grid-rapport til", result$report_path, "\n")
}
