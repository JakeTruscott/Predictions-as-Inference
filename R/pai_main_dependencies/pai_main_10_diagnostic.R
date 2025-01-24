pai_diagnostic <- function(output){

  diagnostics <- list() #Create Empty List to Store Outputs
  parameters <- output$parameters #Grab Parameters

  {

    placebo_vars <- output$placebo$var

    {

      dv_type = output$parameters$outcome_type
      if (dv_type == 'Binomial'){
        y_label = 'Change in Predictive Accuracy\n'
      } else {
        y_label = 'Change in RMSE\n'
      }

      placebo_figure <- output$fit_change %>%
        filter(var %in% placebo_vars) %>%
        mutate(var = ifelse(grepl("\\*", var), gsub("\\*", " x\n", var), var)) %>%
        ggplot(aes(x = factor(var_id))) +
        geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
        geom_rect(aes(xmin = var_id - 0.15, xmax = var_id + 0.15,
                      ymin = lwr, ymax = upr, fill = 'Range of Predicted\nAccuracy from Placebos'), colour = 'gray5') +
        geom_point(aes(y = fit_change, colour = 'Prediction from Model Fit\nAfter Dropping Information'), size = 3, shape = 10) +
        labs(y = y_label,
             x = '\nVariable\n',
             fill = ' ',
             colour = ' ') +
        scale_fill_manual(values = c("gray", "gray50")) +
        scale_colour_manual(values = 'black') +
        theme_minimal() +
        theme(legend.position = 'bottom',
              panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 10, colour = 'black'),
              axis.title = element_text(size = 12, colour = 'black')) +
        scale_x_discrete(labels = output$fit_change$var)


    } #Render Placebo Figure
    {

      placebo_CI_figure <- output$placebo_all %>%
        left_join(output$fit_change %>%
                    select(var, var_id), by = 'var') %>%
        mutate(accuracy_change = shuffled_accuracy - output$placebo$base)

      placebo_CI_figure <- ggplot(data = placebo_CI_figure, aes(x = accuracy_change, y = factor(var_id))) +
        geom_vline(xintercept = 0, linetype = 2, colour = 'gray5') +
        geom_density_ridges(alpha = 0.75, scale = 0.9) +
        coord_flip() +
        labs(x = y_label,
             y = '\nVariable\n',
             fill = ' ',
             colour = ' ') +
        scale_fill_manual(values = c("gray", "gray50")) +
        scale_colour_manual(values = 'black') +
        theme_minimal() +
        theme(legend.position = 'bottom',
              panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 10, colour = 'black'),
              axis.title = element_text(size = 12, colour = 'black')) +
        scale_y_discrete(labels = placebo_CI_figure$var)

      placebo_CI_figure

    } # Render Placebo Confidence Interval Figure
    {

      bootstrap_omit_vars <- data.frame()
      bootstrap_output <- output$omitting_variables_bootstrap

      {

        for (i in 1:length(bootstrap_output)){

          temp_bootstrap_list <- bootstrap_output[[i]]

          for (b in 1:length(temp_bootstrap_list)){
            temp_bootstrap_data <- temp_bootstrap_list[[b]]$bootstrap_data

            if (is.null(temp_bootstrap_data)){
              next
            } #Error Handling (Skip)

            temp_bootstrap_accuracy <- temp_bootstrap_list[[b]]$bootstrap_accuracies #Get Temp Accuracy
            temp_bootstrap <- data.frame(accuracy = temp_bootstrap_accuracy, bootstrap_id = b, dropped_var = names(bootstrap_output)[i]) #Combine
            bootstrap_omit_vars <- dplyr::bind_rows(bootstrap_omit_vars, temp_bootstrap) #Append

          }

        }

      } # Retrieve & Organization Bootstrap Data

      {

        bootstrap_distribution <- bootstrap_omit_vars %>%
          select(accuracy, bootstrap_id, dropped_var) %>%
          unique() %>%
          group_by(dropped_var) %>%
          mutate(median = median(accuracy), #Median
                 mean = mean(accuracy), #Mean
                 conf_lower = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[1]), #2.5%
                 conf_higher = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[2]), #97.5
                 max = max(accuracy), #Max
                 min = min(accuracy), #Min,
                 bootstraps = max(bootstrap_id)) %>%
          left_join(output$placebo %>%
                      select(var, var_id) %>%
                      rename(dropped_var = var), by = 'dropped_var')

      } # Density of Accuracies by Var When Dropped

      {

        dv_type = output$parameters$outcome_type
        if (dv_type == 'Binomial'){
          y_label = 'Change in Predictive Accuracy After Omitting Information\n'
        } else {
          y_label = 'Change in RMSE After Omitting Information\n'
        }

        labels <- setNames(bootstrap_distribution$dropped_var, bootstrap_distribution$var_id)


        placebo_bootstrap_omit_figure <- bootstrap_distribution %>%
          ggplot(aes(x = accuracy, y = factor(var_id))) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5') +
          geom_density_ridges(alpha = 0.75, scale = 0.9) +
          scale_y_discrete(labels = labels) +
          coord_flip() +
          labs(x = y_label,
               y = '\nVariable\n',
               fill = ' ',
               colour = ' ') +
          scale_fill_manual(values = c("gray", "gray50")) +
          scale_colour_manual(values = 'black') +
          theme_minimal() +
          theme(legend.position = 'bottom',
                panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
                axis.text = element_text(size = 10, colour = 'black'),
                axis.title = element_text(size = 12, colour = 'black'))


      } #Plot Density of Accuracy Change by Var When Dropped

    } #bootstrap Predictions from Dropping

    if (length(placebo_vars) >= 5){
      placebo_figure <- placebo_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      placebo_CI_figure <- placebo_CI_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      placebo_bootstrap_omit_figure <- placebo_bootstrap_omit_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } # Change X-Axis Label Tilt if More than 5 Vars (Avoids Overlapping Var Labels)

    diagnostics[['placebo']] <- list()

    diagnostics[['placebo']][['placebo_full']] <- placebo_figure #Add Placebo Figure to Diagnostic
    diagnostics[['placebo']][['placebo_CI']] <- placebo_CI_figure #Add Placebo Figure to Diagnostic
    diagnostics[['placebo']][['bootstrap_omit_information']] <- placebo_bootstrap_omit_figure # Add Omit Variable Figure to Diagnostic

  } #Placebo Iterations

  {

    linear_vars <- names(output$push)
    parameters <- output$parameters
    push_output <- output$push #Grab Push Output
    original_test_data <- parameters$test_set

    diagnostic_push <- list() #Create Empty List for Output

    for (var in linear_vars){
      diagnostic_push[[var]] <- list()
    } #Create Var-Level List for diagnostic_push

    for (var in linear_vars) {
      tryCatch({
        temp_dat <- push_output[[var]] # Grab Temp Var

        if (var %in% c(unlist(output$parameters$factors))) {

          if (parameters$outcome_type == 'Binomial'){
            base_plot <- ggplot(data = temp_dat, aes(x = step, y = onecount)) +
              geom_point() +
              labs(x = paste0('\n', as.character(var))) # Generate Base Plot
          } else {
            base_plot <- ggplot(data = temp_dat, aes(x = step, y = diff)) +
              geom_point() +
              labs(x = paste0('\n', as.character(var))) # Generate Base Plot
          }

        } else {

          if (parameters$outcome_type == 'Binomial'){
            base_plot <- ggplot(data = temp_dat, aes(x = step, y = onecount)) +
              geom_point() +
              labs(x = paste0('\n', as.character(var))) # Generate Base Plot
          } else {
            base_plot <- ggplot(data = temp_dat, aes(x = step, y = diff)) +
              geom_point() +
              labs(x = paste0('\n', as.character(var))) # Generate Base Plot
          }


        } # Create Base Plot (Assign x as Factor if in Parameters$factors)

        scott_info <- hist(temp_dat$step, breaks = "scott", plot = FALSE) # Get # Bins from Scott's Normal Reference Rule
        breakpoints <- length(scott_info$breaks)
        temp_dat$bin <- cut_interval(as.numeric(temp_dat$step), n = breakpoints) # Assign Bins

        for (temp_bin in 1:length(unique(temp_dat$bin))) {
          bin_label <- unique(temp_dat$bin)[temp_bin]
          temp_bin_dat <- temp_dat %>%
            filter(bin == unique(temp_dat$bin)[temp_bin])

          if (parameters$outcome_type == 'Binomial'){
            lm_bin_temp <- lm(onecount ~ step, data = temp_bin_dat)
          } else {
            lm_bin_temp <- lm(diff ~ step, data = temp_bin_dat)
          }

          suppressWarnings({
            diagnostic_push[[var]][['linear_fit']][[as.character(bin_label)]] <- broom::tidy(lm_bin_temp) %>%
              mutate(sig = case_when(
                .default = '',
                p.value <= 0.05 & p.value > 0.01 ~ '*',
                p.value <= 0.01 & p.value > 0.001 ~ '**',
                p.value <= 0.001 ~ '***'
              ))

            base_plot <- base_plot +
              geom_smooth(method = 'lm',  data = temp_bin_dat)
          })
        } # Calculate LM by Bins & Append to Base Plot

        base_plot <- base_plot +
          theme_minimal() +
          labs(y = '\nPush Prediction\n') +
          theme(
            panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 12, colour = 'black')
          ) # Create base Plot

        diagnostic_push[[var]][['linear_plot']] <- base_plot # Combine and Store

      }, error = function(e) {
      })
    } # By Var - Calculate Linear Fit & Plot

    diagnostics[['push']] <- diagnostic_push

  } #Linear Fit Across Bins (Push)

  {

    bootstrap_output <- output$bootstrap_predictions_CI #Get Bootstrap List Output
    bootstrap_accuracies <- data.frame() #Create Empty DF to Create Combined Frame
    diagnostics[['bootstrap']] <- list()

    {

      for (i in 1:length(bootstrap_output)){

        temp_bootstrap_data <- bootstrap_output[[i]]$bootstrap_data #Get Temp Bootstrap Test Data

        if (is.null(temp_bootstrap_data)){
          next
        } #Error Handling (Skip)

        temp_bootstrap_accuracy <- bootstrap_output[[i]]$bootstrap_accuracies #Get Temp Accuracy
        temp_bootstrap <- data.frame(temp_bootstrap_data, accuracy = temp_bootstrap_accuracy, bootstrap_id = i) #Combine
        bootstrap_accuracies <- dplyr::bind_rows(bootstrap_accuracies, temp_bootstrap) #Append

      }

    } # Retrieve & Organization Bootstrap Data

    {

      bootstrap_distribution <- bootstrap_accuracies %>%
        select(accuracy, bootstrap_id) %>%
        unique() %>%
        mutate(median = median(accuracy), #Median
               mean = mean(accuracy), #Mean
               conf_lower = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[1]), #2.5%
               conf_higher = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[2]), #97.5
               max = max(accuracy), #Max
               min = min(accuracy), #Min,
               bootstraps = max(bootstrap_id))

      mean_label <- paste0('Mean (', round(bootstrap_distribution$mean[1], 2), ')')
      median_label <- paste0('Median (', round(bootstrap_distribution$median[1], 2), ')')

      if (output$parameters$outcome_type == 'Binomial'){
        x_label = '\nAccuracy from Bootstrapped Test Sets'
      } else {
        x_label = '\nRMSE from bootstrapped Test Sets'
      }


      bootstrap_ci_figure <- ggplot(data = bootstrap_distribution, aes(x = accuracy)) +
        geom_density(fill = 'gray50', colour = 'gray5') +
        geom_vline(aes(xintercept = mean, colour = 'Mean', linetype = 'Mean')) +
        geom_vline(aes(xintercept = median, colour = 'Median', linetype = 'Median')) +
        geom_hline(yintercept = 0, colour = 'black') +
        scale_colour_manual(name = " ",
                            values = c('Mean' = 'gray5', 'Median' = 'gray5'),
                            labels = c(mean_label, median_label)) +
        scale_linetype_manual(name = " ",
                              values = c('Mean' = "solid", 'Median' = "dashed"),
                              labels = c(mean_label, median_label)) +
        theme_minimal() +
        labs(x = x_label,
             y = '\n') +
        theme(
          panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 12, colour = 'black'),
          axis.title = element_text(size = 12, colour = 'black'),
          legend.position = 'bottom',
          legend.title = element_blank(),
          legend.text = element_text(size = 10, colour = 'black'),
          legend.key = element_rect(colour = "black")
        )

      diagnostics$bootstrap[['bootstrap_output']] <- bootstrap_accuracies
      diagnostics$bootstrap[['bootstrap_distribution']] <- bootstrap_ci_figure

    } #Density of Accuracies Figure

    {

      bootstrap_vars <- names(bootstrap_accuracies[!names(bootstrap_accuracies) %in% c('accuracy', 'bootstrap_id')])
      diagnostics$bootstrap[['variable_figures']] <- list()

      for (var in bootstrap_vars){

        temp_var <- var

        temp_var_bootstrap_data <- bootstrap_accuracies[, c(temp_var, 'accuracy', 'bootstrap_id')] %>%
          group_by(.data[[var]]) %>%
          summarise(mean_accuracy = mean(accuracy),
                    lower_ci = mean_accuracy - 1.96 * sd(accuracy) / sqrt(n()),
                    upper_ci = mean_accuracy + 1.96 * sd(accuracy) / sqrt(n()))

        temp_var_bootstrap_figure <- ggplot(temp_var_bootstrap_data, aes(x = if (temp_var %in% unlist(parameters$factors)) factor(.data[[temp_var]]) else .data[[temp_var]], y = mean_accuracy)) +
          geom_segment(aes(x = if (temp_var %in% unlist(parameters$factors)) factor(.data[[temp_var]]) else .data[[temp_var]], y = lower_ci, yend = upper_ci)) +
          #geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5) +
          geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% Confidence\nIntervals"), alpha = 0.2) +  # Add shaded region
          geom_point(size = 2, aes(shape = 'Mean\nAccuracy'), fill = 'white') +  # Add points
          theme_minimal() +
          scale_fill_manual(values = 'deepskyblue3') +
          scale_colour_manual(values = 'black') +
          scale_shape_manual(values = 21) +
          labs(x = paste0('\n', as.character(temp_var)),
               y = '\n') +
          theme(
            panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 12, colour = 'black'),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = element_text(size = 10, colour = 'black'),
            legend.key = element_rect(colour = "black"))

        if (!temp_var %in% unlist(parameters$factors)){
          temp_var_bootstrap_figure <- temp_var_bootstrap_figure +
            stat_smooth(method = 'loess', aes(colour = 'Locally Weighted Scatterplot\nSmoothing (LOESS) Fit w/ SEs'), linetype = 2)
        }


        diagnostics$bootstrap$variable_figures[[var]] <- temp_var_bootstrap_figure

      }


    } # Confidence Intervals by Var


  } #Confidence Intervals (Bootstrap)

  return(diagnostics)

} # Diagnostic Plots & Stats
