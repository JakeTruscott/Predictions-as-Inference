#' Title
#'
#' @param model Declared Model from PAI Object
#' @param full_data Full Data stored in training parameters
#' @param D Independent variable of interest
#' @param moderators Moderators
#' @param n_boot Number of bootstrap iterations
#' @param conf_level Confidence Level
#' @param quantiles Sequence of quantiles
#'
#' @return
#' @export
#'
#' @examples
compute_interaction_AME <- function(model,
                                    full_data,
                                    D,
                                    moderators,
                                    n_boot = 100,
                                    conf_level = 0.95,
                                    quantiles = seq(0.1, 1, 0.1)){


  ame_combined <- list()

  {

    ame_quantiles <- function(model, full_data, D, moderators,
                              quantiles = seq(0, 1, 0.1),
                              n_boot = 100, conf_level = 0.95) {

      #moderators <- as.character(moderators)
      #D <- as.character(D)

      is_binary_or_factor <- function(v) {
        u <- unique(v[!is.na(v)])
        is_binary_vals <- length(u) == 2
        is_factor <- is.factor(v)
        return(is_binary_vals || is_factor)
      }

      D_binary <- is_binary_or_factor(full_data[[D]])
      delta_D <- if (!D_binary) 0.1 * sd(full_data[[D]], na.rm = TRUE) else NA

      get_vals <- function(var) {
        x <- na.omit(full_data[[var]])
        if (is_binary_or_factor(x)) {
          sort(unique(x))
        } else {
          unique(quantile(x, probs = quantiles, na.rm = TRUE))
        }
      }

      mod_vals <- lapply(moderators, get_vals)
      names(mod_vals) <- moderators
      grid <- expand.grid(mod_vals, stringsAsFactors = FALSE)

      get_ame <- function(data, mod_combo) {
        data_low <- data
        data_high <- data

        for (j in seq_along(moderators)) {
          var <- moderators[j]
          val <- mod_combo[[var]]
          if (is.factor(full_data[[var]])) {
            data_low[[var]] <- factor(val, levels = levels(full_data[[var]]))
            data_high[[var]] <- factor(val, levels = levels(full_data[[var]]))
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
          AME = point_est,
          CI_lower = lower,
          CI_upper = upper,
          stringsAsFactors = FALSE
        )
      })

      result_df <- do.call(rbind, results)
      rownames(result_df) <- NULL
      return(result_df)
    }



    ame_sd <- function(model, full_data, D, moderators,
                       n_boot = 100, conf_level = 0.95) {

      is_binary_or_factor <- function(v) {
        u <- unique(v[!is.na(v)])
        is_binary_vals <- length(u) == 2
        is_factor <- is.factor(v)
        return(is_binary_vals || is_factor)
      }

      D <- as.character(D)
      moderators <- as.character(moderators)

      D_binary <- is_binary_or_factor(full_data[[D]])
      delta_D <- if (!D_binary) 0.1 * sd(full_data[[D]], na.rm = TRUE) else NA

      get_vals <- function(var) {
        x <- full_data[[var]]
        x <- na.omit(x)
        min_val <- min(as.numeric(x))
        max_val <- max(as.numeric(x))

        if (is_binary_or_factor(x)) {
          vals <- sort(unique(x))
        } else {
          mean_val <- mean(x)
          sd_val <- sd(x)
          seq_vals <- seq(mean_val - 2 * sd_val, mean_val + 2 * sd_val, length.out = 10)
          vals <- pmin(pmax(seq_vals, min_val), max_val)
        }

        vals <- round(vals, digits = 3)
        unique(vals)
      }

      mod_vals <- lapply(moderators, get_vals)
      names(mod_vals) <- moderators
      grid <- expand.grid(mod_vals, stringsAsFactors = FALSE)

      get_ame <- function(data, mod_combo) {
        data_low <- data
        data_high <- data

        for (j in seq_along(moderators)) {
          var <- moderators[j]
          val <- mod_combo[[var]]
          if (is.factor(full_data[[var]])) {
            data_low[[var]] <- factor(val, levels = levels(full_data[[var]]))
            data_high[[var]] <- factor(val, levels = levels(full_data[[var]]))
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
          AME = point_est,
          CI_lower = lower,
          CI_upper = upper,
          stringsAsFactors = FALSE
        )
      })

      result_df <- do.call(rbind, results)
      rownames(result_df) <- NULL
      return(result_df)

    }


    x_distribution <- function(full_data,
                               moderators){

      x_distribution_figures <- list()

      for (i in 1:length(moderators)){

        x_distribution <- full_data %>%
          select(moderators[i])
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


        x_distribution_figures[[as.character(moderators[i])]] <- x_distribution_figure

      }

      return(x_distribution_figures)

    }

  } # AME Quantiles & SD Functions (+ X-distribution Function)



  ame_combined[['ame_sd']] <- ame_sd(model,
                                     full_data,
                                     D,
                                     moderators,
                                     n_boot = 10)

  {

    ame_sd_output <- ame_combined[['ame_sd']]

    ame_sd_output <- ame_sd_output %>%
      mutate(
        type = if (length(moderators) == 2) {
          paste0(moderators[1], ' = ', .[[1]], '\n', moderators[2], ' = ', .[[2]])
        } else {
          as.character(.[[1]])
        },
        x_numeric = as.numeric(factor(type))
      )

    ame_sd_figure <-  ame_sd_output %>%
      ggplot(aes(x = x_numeric, y = AME)) +
      geom_linerange(aes(x = x_numeric, ymin = CI_lower, ymax = CI_upper), linewidth = 1.5) +
      geom_point(size = 5, shape = 21, colour = 'black', fill = 'white') +
      scale_x_continuous(breaks = ame_sd_output$x_numeric, labels = ame_sd_output$type) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = paste0('\n', as.character(paste(moderators, collapse = ' + ')), ' (Moderator)'),
           y = paste0(as.character(D), ' (Treatment)\n')) +
      theme_minimal() +
      theme(panel.background = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 16, colour = 'black'))

    if (nrow(ame_sd_output) >= 5 & length(moderators) == 2){
      ame_sd_figure <- ame_sd_figure +
        theme(axis.text.x = element_text(size = 10, colour = 'black', vjust = 0.5, angle = 45))
    }

    ame_combined[['ame_sd_figure']] <- ame_sd_figure


  } # AME SD Figure

  ame_combined[['ame_quantiles']] <- ame_quantiles(model,
                                                   full_data,
                                                   D,
                                                   moderators,
                                                   n_boot = 10)

  {

    ame_quantiles_output <- ame_combined[['ame_quantiles']]

    ame_quantiles_output <- ame_quantiles_output %>%
      mutate(
        type = if (length(moderators) == 2) {
          paste0(moderators[1], ' = ', .[[1]], '\n', moderators[2], ' = ', .[[2]])
        } else {
          as.character(.[[1]])
        },
        x_numeric = as.numeric(factor(type))
      )

    ame_quantiles_figure <- ame_quantiles_output %>%
      ggplot( aes(x = x_numeric, y = AME)) +
      geom_linerange(aes(x = x_numeric, ymin = CI_lower, ymax = CI_upper), linewidth = 1.5) +
      geom_point(size = 5, shape = 21, colour = 'black', fill = 'white') +
      scale_x_continuous(breaks = ame_quantiles_output$x_numeric, labels = ame_quantiles_output$type) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = paste0('\n', as.character(paste(moderators, collapse = ' + ')), ' (Moderator)'),
           y = paste0(as.character(D), ' (Treatment)\n')) +
      theme_minimal() +
      theme(panel.background = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 14, colour = 'black'),
            axis.title = element_text(size = 16, colour = 'black'))

    if (nrow(ame_quantiles_output) >= 5 & length(moderators) == 2){
      ame_quantiles_figure <- ame_quantiles_figure +
        theme(axis.text.x = element_text(size = 10, colour = 'black', vjust = 0.5, angle = 45))
    }

    ame_combined[['ame_quantiles_figure']] <- ame_quantiles_figure


  } # AME Quantiles Figure



  ame_combined[['x_distributions_figure']] <- x_distribution(full_data,
                                                             moderators)


  return(ame_combined)



}
