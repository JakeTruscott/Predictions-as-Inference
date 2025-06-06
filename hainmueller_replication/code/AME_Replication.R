################################################################################
# Recover Average Marginal Effects of Hainmueller Interactions
# Updated June 2025
################################################################################

################################################################################
# Load Source Files
################################################################################

#pai_dir <- *DIRECTORY* # Set Directory to Folder Location Containing Files

source(file.path(pai_dir, 'code',  'pai_main.R')) # Source PAI Main -- Will Load Necessary Packages
source(file.path(pai_dir, 'code', 'pai_diagnostic.R')) # Source PAI Diagnostic


################################################################################
# Recover Average Marginal Effects (Quantiles & SD)
################################################################################

pai_runs <- list.files('hainmueller_replication/pai_runs', full.names = T)
source('R/pai_diagnostic.R') # Source PAI Diagnostic

temp_run <- get(load(pai_runs[i])) # Replace I w/ Associated Index (Can Also Convert to Loop)
temp_run_meta <- temp_run$meta # Recover Meta
temp_run <- temp_run$run # Assign Run to Object
temp_pai_name <- gsub('.*pai_runs\\/', '', gsub('\\.rdata', '', pai_runs[i])) # Recover Name of Replication

temp_interaction <- c(temp_run_meta$D, temp_run_meta$X) # Recover Interaction (D is Main IV & X is Moderator)
temp_full_data <- temp_run$parameters$full_data # Recover Full Data from PAI Object
D <- temp_interaction[1] # Assign Name of D
X <- temp_interaction[2] # Assign Name of X



compute_interaction_AME <- function(model, full_data, D, X,
                                    quantiles = seq(0, 1, 0.1),
                                    n_boot = 100, conf_level = 0.95) {
  D <- as.character(D)
  X <- as.character(X)

  is_binary <- function(v) {
    u <- unique(v[!is.na(v)])
    all_vals <- c(0, 1, TRUE, FALSE, "0", "1")
    length(u) == 2 && all(u %in% all_vals)
  } # Check if Either D or X Are BInary

  D_binary <- is_binary(full_data[[D]])
  X_binary <- is_binary(full_data[[X]])

  delta_X <- if (!X_binary) 0.1 * sd(full_data[[X]], na.rm = TRUE) else NA

  get_ame <- function(data, x_val) {
    data_low <- data
    data_high <- data

    data_low[[X]] <- x_val # Set moderator X to fixed value
    data_high[[X]] <- x_val

    if (D_binary) {
      data_low[[D]] <- 0
      data_high[[D]] <- 1
    } else {
      data_low[[D]] <- data_low[[D]] - delta_X
      data_high[[D]] <- data_high[[D]] + delta_X
    } # Perturb D (Main IV of Interst)

    pred_low <- predict(model, newdata = data_low) # Predict
    pred_high <- predict(model, newdata = data_high)

    if (is.factor(pred_low) || is.matrix(pred_low)) {
      pred_low <- predict(model, newdata = data_low, type = "prob")[, 2]
      pred_high <- predict(model, newdata = data_high, type = "prob")[, 2]
    } # Classification handling

    mean(pred_high - pred_low, na.rm = TRUE) # Recover Mean
  }

  if (X_binary) {
    vals <- unique(na.omit(full_data[[X]]))
    if (is.numeric(vals)) {
      vals <- sort(round(vals))
    } else {
      vals <- sort(vals)
    }
    x_vals <- vals
  } else {
    x_vals <- quantile(full_data[[X]], probs = quantiles, na.rm = TRUE)
  } # Get Moderator Values Conditioned on Binary Status -- Assign Quantiles

  results <- lapply(x_vals, function(x_val) {
    point_est <- get_ame(full_data, x_val) # Get AME of X Value

    boot_vals <- replicate(n_boot, {
      boot_data <- full_data[sample(nrow(full_data), replace = TRUE), ]
      tryCatch(get_ame(boot_data, x_val), error = function(e) NA)
    }) # Boostrap Sample (W/ Replacement) from Full Data

    alpha <- 1 - conf_level # Recover Alpha
    lower <- quantile(boot_vals, probs = alpha / 2, na.rm = TRUE) # Assign Lower CI
    upper <- quantile(boot_vals, probs = 1 - alpha / 2, na.rm = TRUE) # Assign Upper CI

    data.frame(X = x_val, AME_D = point_est, CI_lower = lower, CI_upper = upper) # Compile as DF
  })

  result_df <- do.call(rbind, results) # Bind Rows

  if (!X_binary) {
    result_df$Quantile <- names(x_vals)
  }

  rownames(result_df) <- NULL
  return(result_df)
} # Compute AME (Quantiles)

{

  result <- compute_interaction_AME(
    model = temp_run$declared_model,
    full_data = temp_run$parameters$full_data,
    D = temp_run_meta$D,  # D = treatment/independent variable
    X = temp_run_meta$X   # X = moderator
  )

  ame <- result %>%
    ggplot(aes(x = X, y = AME_D)) +
    geom_rect(aes(xmin = X - 0.01, xmax = X + 0.01, ymin = CI_lower, ymax = CI_upper),
              colour = 'black', fill = 'grey75') +
    geom_point(size = 4, shape = 21, colour = 'black', fill = 'white') +
    labs(x = paste0('\n', as.character(X), ' (Moderator)'),
         y = paste0(as.character(D), ' (Treatment)\n')) +
    theme_minimal() +
    theme(panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'))

} # Recover AME w/ Quantiles


compute_interaction_AME_SD <- function(model, full_data, D, X,
                                       n_boot = 100, conf_level = 0.95) {
  D <- as.character(D)
  X <- as.character(X)

  is_binary <- function(v) {
    u <- unique(v[!is.na(v)])
    all_vals <- c(0, 1, TRUE, FALSE, "0", "1")
    length(u) == 2 && all(u %in% all_vals)
  }

  D_binary <- is_binary(full_data[[D]])
  X_binary <- is_binary(full_data[[X]])

  delta_X <- if (!D_binary) 0.1 * sd(full_data[[D]], na.rm = TRUE) else NA

  get_ame <- function(data, x_val) {
    data_low <- data
    data_high <- data

    data_low[[X]] <- x_val
    data_high[[X]] <- x_val

    if (D_binary) {
      data_low[[D]] <- 0
      data_high[[D]] <- 1
    } else {
      data_low[[D]] <- data_low[[D]] - delta_X
      data_high[[D]] <- data_high[[D]] + delta_X
    }

    pred_low <- predict(model, newdata = data_low)
    pred_high <- predict(model, newdata = data_high)

    if (is.factor(pred_low) || is.matrix(pred_low)) {
      pred_low <- predict(model, newdata = data_low, type = "prob")[, 2]
      pred_high <- predict(model, newdata = data_high, type = "prob")[, 2]
    }

    mean(pred_high - pred_low, na.rm = TRUE)
  }

  if (X_binary) {
    x_vals <- sort(unique(na.omit(full_data[[X]])))
  } else {
    x_mean <- mean(full_data[[X]], na.rm = TRUE)
    x_sd <- sd(full_data[[X]], na.rm = TRUE)
    x_vals <- seq(x_mean - 2 * x_sd, x_mean + 2 * x_sd, length.out = 50)
  }

  results <- lapply(x_vals, function(x_val) {
    point_est <- get_ame(full_data, x_val)

    boot_vals <- replicate(n_boot, {
      boot_data <- full_data[sample(nrow(full_data), replace = TRUE), ]
      tryCatch(get_ame(boot_data, x_val), error = function(e) NA)
    })

    alpha <- 1 - conf_level
    lower <- quantile(boot_vals, probs = alpha / 2, na.rm = TRUE)
    upper <- quantile(boot_vals, probs = 1 - alpha / 2, na.rm = TRUE)

    data.frame(X_value = x_val, AME_D = point_est, CI_lower = lower, CI_upper = upper)
  })

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
} # Same Function but Uses SDs Instead


{

  result_SD <- compute_interaction_AME_SD(model = temp_run$declared_model,
                                          full_data = temp_run$parameters$full_data,
                                          D = temp_run_meta$D,
                                          X = temp_run_meta$X)

  ame_SD <- result_SD %>%
    ggplot(aes(x = X_value, y = AME_D)) +
    geom_rect(aes(xmin = X_value - 0.01, xmax = X_value + 0.01, ymin = CI_lower, ymax = CI_upper),
              colour = 'black', fill = 'grey75') +
    geom_point(size = 4, shape = 21, colour = 'black', fill = 'white') +
    labs(x = paste0('\n', as.character(X), ' (Moderator)'),
         y = paste0(as.character(D), ' (Treatment)\n')) +
    theme_minimal() +
    theme(panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'))

} # Recover AME w/ SD

################################################################################
# Recover Distribution of X
################################################################################

{

  x_distribution  <- temp_full_data %>%
    select(temp_run_meta$X)

  x_distribution_figure <- ggplot(aes(x = x_distribution[,1]), data = x_distribution) +
    geom_density(fill = 'gray', colour = 'black', alpha = 1/2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = mean(x_distribution[,1]), linetype = 2, alpha = 1/2) +
    labs(x = paste0('\n', as.character(X), ' (Moderator)'),
         y = 'Density\n') +
    theme_minimal() +
    theme(panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'))



} # Distribution Re: Moderator (X)


################################################################################
# Combine Figures to Single Plot Space (patchwork)
################################################################################\

ame_combined <- ame +
  labs(title = 'AME - Quantiles',
       x = ' ') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold', size = 12))

ame_SD_combined <- ame_SD +
  labs(title = 'AME - Standard Deviations',
       x = ' ',
       y = ' ') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(size = 14))

x_distribution_figure_combined <- x_distribution_figure +
  labs(title = paste0('Distribution of ', as.character(X)),
       x = paste0('\n', as.character(X), ' (Moderator)')) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title = element_text(size = 14),)

ame_combined_figure <- (ame_combined + ame_SD_combined) / x_distribution_figure_combined +
  plot_layout(guides = 'collect', heights = c(2, 1)) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, colour = 'black', face = 'bold'),
    plot.margin = ggplot2::margin(t = 5, r = 5, b = 20, l = 5)
  )



