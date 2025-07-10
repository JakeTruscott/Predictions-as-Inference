#' Compute Average Marginal Effects from Interaction
#'
#' @param model PAI Model Output
#' @param full_data Full Data (Model Output --> Parameters --> Full Data)
#' @param D # Independent Variable of Interest
#' @param moderators # Moderator -- Can Declare Up to 2
#' @param n_boot # Number of Bootstrap Intervals
#' @param conf_level # Confidence Interval Levels
#'
#' @return
#' @export
#'
#' @examples
compute_interaction_AME <- function(model, full_data, D, moderators,
                                    n_boot = 100, conf_level = 0.95) {
  stopifnot(length(moderators) %in% c(1, 2))

  D <- as.character(D)
  moderators <- as.character(moderators)

  is_binary <- function(v) {
    u <- unique(v[!is.na(v)])
    all_vals <- c(0, 1, TRUE, FALSE, "0", "1")
    length(u) == 2 && all(u %in% all_vals)
  }

  D_binary <- is_binary(full_data[[D]])
  delta_D <- if (!D_binary) 0.1 * sd(full_data[[D]], na.rm = TRUE) else NA

  get_vals <- function(var) {
    if (is_binary(full_data[[var]])) {
      sort(unique(na.omit(full_data[[var]])))
    } else {
      mean_val <- mean(full_data[[var]], na.rm = TRUE)
      sd_val <- sd(full_data[[var]], na.rm = TRUE)
      seq(mean_val - 2 * sd_val, mean_val + 2 * sd_val, length.out = 5)
    }
  }

  mod_vals <- lapply(moderators, get_vals)
  names(mod_vals) <- moderators

  grid <- expand.grid(mod_vals)

  get_ame <- function(data, mod_combo) {
    data_low <- data
    data_high <- data

    for (j in seq_along(moderators)) {
      var <- moderators[j]
      val <- mod_combo[[var]]
      if (is.factor(full_data[[var]])) {
        levels_vec <- levels(full_data[[var]])
        data_low[[var]] <- factor(val, levels = levels_vec)
        data_high[[var]] <- factor(val, levels = levels_vec)
      } else {
        data_low[[var]] <- val
        data_high[[var]] <- val
      }
    }

    if (D_binary) {
      data_low[[D]] <- 0
      data_high[[D]] <- 1
    } else {
      data_low[[D]] <- data_low[[D]] - delta_D
      data_high[[D]] <- data_high[[D]] + delta_D
    }

    pred_low <- predict(model, newdata = data_low)
    pred_high <- predict(model, newdata = data_high)

    if (is.factor(pred_low) || is.matrix(pred_low)) {
      pred_low <- predict(model, newdata = data_low, type = "prob")[, 2]
      pred_high <- predict(model, newdata = data_high, type = "prob")[, 2]
    }

    mean(pred_high - pred_low, na.rm = TRUE)
  }

  results <- lapply(seq_len(nrow(grid)), function(i) {
    mod_combo <- as.list(grid[i, , drop = FALSE])
    point_est <- get_ame(full_data, mod_combo)

    boot_vals <- replicate(n_boot, {
      boot_data <- full_data[sample(nrow(full_data), replace = TRUE), ]
      tryCatch(get_ame(boot_data, mod_combo), error = function(e) NA)
    })

    alpha <- 1 - conf_level
    lower <- quantile(boot_vals, probs = alpha / 2, na.rm = TRUE)
    upper <- quantile(boot_vals, probs = 1 - alpha / 2, na.rm = TRUE)

    data.frame(
      as.list(mod_combo),
      setNames(list(point_est), D),
      CI_lower = lower,
      CI_upper = upper,
      stringsAsFactors = FALSE
    )
  })


  result_df <- do.call(rbind, results)
  numeric_cols <- c(D, "CI_lower", "CI_upper")
  result_df[numeric_cols] <- lapply(result_df[numeric_cols], as.numeric)

  return(result_df)

}
