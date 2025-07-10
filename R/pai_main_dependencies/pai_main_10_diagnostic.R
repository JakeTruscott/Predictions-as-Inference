pai_diagnostic <- function(output){

  diagnostics <- list()

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
             x = '\n\n',
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
             y = '\n\n',
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
          y_label = 'Change in Predictive Accuracy\nAfter Omitting Information\n'
        } else {
          y_label = 'Change in RMSE\nAfter Omitting Information\n'
        }

        labels <- setNames(bootstrap_distribution$dropped_var, bootstrap_distribution$var_id)


        placebo_bootstrap_omit_figure <- bootstrap_distribution %>%
          ggplot(aes(x = accuracy, y = factor(var_id))) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5') +
          geom_density_ridges(alpha = 0.75, scale = 0.9) +
          scale_y_discrete(labels = labels) +
          coord_flip() +
          labs(x = y_label,
               y = '\n\n',
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
    {

      combined_figure <- placebo_figure/(placebo_CI_figure + placebo_bootstrap_omit_figure)

    } # Combined Placebo Figure

    if (length(placebo_vars) >= 5){
      placebo_figure <- placebo_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      placebo_CI_figure <- placebo_CI_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      placebo_bootstrap_omit_figure <- placebo_bootstrap_omit_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      combined_figure <- combined_figure +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } # Change X-Axis Label Tilt if More than 5 Vars (Avoids Overlapping Var Labels)

    diagnostics[['placebo']] <- list()

    diagnostics[['placebo']][['placebo_full']] <- placebo_figure #Add Placebo Figure to Diagnostic
    diagnostics[['placebo']][['placebo_CI']] <- placebo_CI_figure #Add Placebo Figure to Diagnostic
    diagnostics[['placebo']][['bootstrap_omit_information']] <- placebo_bootstrap_omit_figure # Add Omit Variable Figure to Diagnostic
    diagnostics[['placebo']][['placebo_combined_figure']] <- combined_figure

  } # Placebo Shuffles

  {

    push_recovery <-  function(push_data = NULL,
                               variables = NULL,
                               bins = 10,
                               bin_cut = 5){

      push_output <- list()

      for (var in 1:length(variables)) {

        process_variable <- function(var_index) {
          tryCatch({
            temp_dat <- data.frame(push_data[[variables[var_index]]])
            temp_dat <- temp_dat[, c(1, 2)] # Step & Pred/Prob
            names(temp_dat) <- c('steps', 'pred')
            temp_dat$steps <- as.numeric(temp_dat$steps)

            lm_temp <- lm(pred ~ steps, data = temp_dat)
            ci_temp <- predict(lm_temp, interval = "confidence")
            temp_dat <- cbind(temp_dat, ci_temp)

            summary_lm <- suppressWarnings(summary(lm_temp))
            slope <- coef(summary_lm)[2]
            se_slope <- summary_lm$coefficients[2, "Std. Error"]
            p_value <- summary_lm$coefficients[2, "Pr(>|t|)"]

            slope_frame <- data.frame(
              slope = round(slope, 3),
              se = round(se_slope, 3),
              p_value = round(p_value, 5)) %>%
              mutate(p_value = case_when(
                .default = "",
                p_value < 0.05 & p_value >= 0.01 ~ "*",
                p_value < 0.01 & p_value >= 0.001 ~ "**",
                p_value < 0.001 ~ "***"
              )) %>%
              unique()

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = pred)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', linetype = 'Linear Fit\n(w/ 95% CI)'), method = "lm", formula = y ~ x,  se = FALSE, size = 1) +
              geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, colour = 'gray5') +
              stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)', linetype = 'Loess Fit\n(w/ SE)'), method = "loess", formula = y ~ x, se = TRUE, size = 1) +
              theme_minimal() +
              scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = ifelse(output$parameters$outcome_type == 'Continuous', 'Prediction\n', paste0('p(', output$parameters$outcome, '= 1)\n') ),
                color = 'Fit Type',
                linetype = 'Fit Type'
              ) +
              theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 16),
                panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
                legend.title.align = 0.5,
                legend.text.align = 0.25,
                legend.title = element_blank(),
                legend.text = element_text(size = 15, color = "gray5"),
                legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
                legend.position = "bottom",
                strip.text = element_text(size = 14, face = "bold"),
                strip.background = element_rect(fill = "gray", color = "gray5"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 15),
                plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))

            push_output$figures$linear[[variables[var_index]]] <<- temp_figure_stand_alone
            push_output$slope_tables$linear[[variables[var_index]]] <<- slope_frame

          }, error = function(e) {
            message(paste("Skipping", variables[var_index], "due to error:", e$message))
          })
        }

        process_variable(var)
      } # Simple Linear Fit

      for (var in 1:length(variables)) {

        process_static_bins <- function(var_index) {
          tryCatch({
            temp_dat <- data.frame(push_data[[variables[var_index]]])
            temp_dat <- temp_dat[, c(1, 2)]
            names(temp_dat) <- c('steps', 'pred')

            temp_dat$steps <- as.numeric(temp_dat$steps)
            num_bins <- bins
            temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

            temp_dat <- temp_dat %>%
              rowwise() %>%
              mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                                  gsub(".*\\,", "", gsub("\\]", "", bin)),
                                  0)) %>%
              mutate(cut = as.numeric(cut))

            ci_temp <- data.frame()
            slope_temp <- data.frame()
            unique_bins <- unique(temp_dat$bin)

            for (b in unique_bins) {
              temp_bin <- temp_dat[temp_dat$bin == b,]
              lm_bin_temp <- suppressWarnings(lm(pred ~ steps, data = temp_bin))

              summary_lm <- suppressWarnings(summary(lm_bin_temp))
              slope <- coef(summary_lm)[2]
              se_slope <- summary_lm$coefficients[2, "Std. Error"]
              p_value <- summary_lm$coefficients[2, "Pr(>|t|)"]

              slope_frame <- data.frame(
                bin = b,
                slope = round(slope, 3),
                se = round(se_slope, 3),
                p_value = round(p_value, 5)) %>%
                mutate(sig = case_when(
                  .default = "",
                  p_value < 0.05 & p_value >= 0.01 ~ "*",
                  p_value < 0.01 & p_value >= 0.001 ~ "**",
                  p_value < 0.001 ~ "***"
                )) %>%
                unique()

              slope_temp <- bind_rows(slope_temp, slope_frame)

              ci_bin_temp <- predict(lm_bin_temp, interval = 'confidence')
              ci_bin_temp <- data.frame(ci = ci_bin_temp)
              ci_temp <- bind_rows(ci_temp, ci_bin_temp)
            }

            temp_dat <- cbind(temp_dat, ci_temp)

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = pred)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
              geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
              stat_smooth(
                aes(colour = 'Linear Fit\n(w/ 95% CI)', linetype = 'Linear Fit\n(w/ 95% CI)', group = bin),
                method = "lm", se = FALSE, formula = y ~ x, size = 1
              ) +
              stat_smooth(
                aes(colour = 'Loess Fit\n(w/ SE)', linetype = 'Loess Fit\n(w/ SE)'),
                method = "loess", se = TRUE, formula = y ~ x, size = 1
              ) +
              theme_minimal() +
              scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = ifelse(output$parameters$outcome_type == 'Continuous', 'Prediction\n', paste0('p(', output$parameters$outcome, '= 1)\n') ),
                color = 'Fit Type',
                linetype = 'Fit Type'
              ) +
              theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 16, face = 'bold'),
                panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
                panel.grid = element_blank(),
                legend.title.align = 0.5,
                legend.text.align = 0.25,
                legend.title = element_blank(),
                legend.text = element_text(size = 15, color = "gray5"),
                legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
                legend.position = "bottom",
                strip.text = element_text(size = 14, face = "bold"),
                strip.background = element_rect(fill = "gray", color = "gray5"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 15),
                plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))

            push_output$figures$static[[variables[var_index]]] <<- temp_figure_stand_alone
            push_output$slope_tables$static[[variables[var_index]]] <<- slope_temp

          }, error = function(e) {
            message(paste("Skipping", variables[var_index], "due to error:", e$message))
          })
        }

        process_static_bins(var)
      } # Static Bins

      for (var in 1:length(variables)){

        process_rolling_bins <- function(var_index) {
          tryCatch({
            temp_dat <- data.frame(push_data[[variables[var_index]]])
            temp_dat <- temp_dat[, c(1, 2)]
            names(temp_dat) <- c('steps', 'pred')

            temp_dat$steps <- as.numeric(temp_dat$steps)
            num_bins <- bins
            temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

            temp_dat <- temp_dat %>%
              rowwise() %>%
              mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                                  gsub(".*\\,", "", gsub("\\]", "", bin)),
                                  0)) %>%
              mutate(cut = as.numeric(cut)) %>%
              group_by(cut) %>%
              mutate(bin_id = cur_group_id()) %>%
              ungroup()

            # Create overlapping rolling bins (bin_cut_id)
            t <- data.frame()
            bin_cut_id <- 1
            max_bin <- max(temp_dat$bin_id)

            for (i in unique(temp_dat$bin_id)) {
              bin_1 <- i
              bin_2 <- ifelse(i == max_bin, i, i + 1)

              bin_1_dat <- temp_dat %>% filter(bin_id == bin_1)
              bin_2_dat <- temp_dat %>% filter(bin_id == bin_2)

              if (i == max_bin) {
                temp_bin_dat <- bin_1_dat
              } else {
                temp_bin_dat <- bind_rows(bin_1_dat, bin_2_dat)
              }

              temp_bin_dat$bin_cut_id <- bin_cut_id
              t <- bind_rows(t, temp_bin_dat)

              bin_cut_id <- bin_cut_id + 1
            }

            # Prepare bin_cut matching labels
            bin_cut_match <- t %>%
              select(steps, bin_cut_id) %>%
              group_by(bin_cut_id) %>%
              mutate(bin_range = paste0("(", round(min(steps), 3), ", ", round(max(steps), 3), "]")) %>%
              mutate(bin_group = paste0(bin_range, ' Bin (', bin_cut_id, ') \n Slope = ')) %>%
              ungroup()

            ci_temp <- data.frame()
            slope_temp <- data.frame()
            unique_bins <- unique(t$bin_cut_id)

            for (b in unique_bins) {
              temp_bin <- t[t$bin_cut_id == b, ]
              lm_bin_temp <- suppressWarnings(lm(pred ~ steps, data = temp_bin))

              summary_lm <- suppressWarnings(summary(lm_bin_temp))
              slope <- coef(summary_lm)[2]
              se_slope <- summary_lm$coefficients[2, "Std. Error"]
              p_value <- summary_lm$coefficients[2, "Pr(>|t|)"]

              slope_frame <- data.frame(
                bin = bin_cut_match$bin_range[match(b, bin_cut_match$bin_cut_id)],
                slope = round(slope, 3),
                se = round(se_slope, 3),
                p_value = round(p_value, 5)
              ) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 & p_value >= 0.001 ~ "**",
                  p_value < 0.05 & p_value >= 0.01 ~ "*",
                  TRUE ~ ""
                )) %>%
                unique()

              slope_temp <- bind_rows(slope_temp, slope_frame)

              ci_bin_temp <- predict(lm_bin_temp, interval = 'confidence')
              ci_bin_temp <- data.frame(ci = ci_bin_temp)
              ci_temp <- bind_rows(ci_temp, ci_bin_temp)
            }

            temp_dat <- cbind(t, ci_temp)

            temp_dat$bin_cut_group <- bin_cut_match$bin_group[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
            temp_dat$bin_range <- bin_cut_match$bin_range[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
            temp_dat$slope <- slope_temp$slope[match(temp_dat$bin_range, slope_temp$bin)]
            temp_dat$sig <- slope_temp$sig[match(temp_dat$bin_range, slope_temp$bin)]
            temp_dat$bin_cut_group <- paste0(temp_dat$bin_cut_group, temp_dat$slope, temp_dat$sig)
            temp_dat$bin_cut_group <- factor(temp_dat$bin_cut_group, levels = unique(temp_dat$bin_cut_group))

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = pred)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              stat_smooth(
                aes(
                  colour = 'Linear Fit\n(w/ 95% CI)',
                  linetype = 'Linear Fit\n(w/ 95% CI)',
                  group = bin_cut_id), method = "lm",  se = TRUE, formula = y~x, size = 1) +
              geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
              stat_smooth(aes(
                  colour = 'Loess Fit\n(w/ SE)',
                  linetype = 'Loess Fit\n(w/ SE)'), method = "loess",  se = TRUE, formula = y~x, size = 1) +
              facet_wrap(~bin_cut_group, scales = 'free') +
              geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
              theme_minimal() +
              scale_color_manual(values = c(
                'Linear Fit\n(w/ 95% CI)' = 'gray5',
                'Loess Fit\n(w/ SE)' = 'red' )) +
              scale_linetype_manual(values = c(
                'Linear Fit\n(w/ 95% CI)' = 'solid',
                'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = ifelse(output$parameters$outcome_type == 'Continuous', 'Prediction\n', paste0('p(', output$parameters$outcome, '= 1)\n') ),
                color = 'Fit Type',
                linetype = 'Fit Type'
              ) +
              theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 16, face = 'bold'),
                panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
                panel.grid = element_blank(),
                legend.title.align = 0.5,
                legend.text.align = 0.25,
                legend.title = element_blank(),
                legend.text = element_text(size = 15, color = "gray5"),
                legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
                legend.position = "bottom",
                strip.text = element_text(size = 14, face = "bold"),
                strip.background = element_rect(fill = "gray", color = "gray5"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 15),
                plot.caption = element_text(size = 12, hjust = 0, face = 'italic')
              )

            push_output$figures$rolling[[variables[var_index]]] <<- temp_figure_stand_alone
            push_output$slope_tables$rolling[[variables[var_index]]] <<- slope_temp

          }, error = function(e) {
            message(paste("Skipping", variables[var_index], "due to error:", e$message))
          })
        }

        process_rolling_bins(var)

      } # Rolling Bins

      for (var in 1:length(variables)){

        process_rolling_extended_bins <- function(var_index) {
          tryCatch({
            temp_dat <- data.frame(push_data[[variables[var_index]]])
            temp_dat <- temp_dat[, c(1, 2)]
            names(temp_dat) <- c('steps', 'pred')

            temp_dat$steps <- as.numeric(temp_dat$steps)
            num_bins <- bins
            temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

            temp_dat <- temp_dat %>%
              rowwise() %>%
              mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                                  gsub(".*\\,", "", gsub("\\]", "", bin)),
                                  0)) %>%
              mutate(cut = as.numeric(cut)) %>%
              group_by(cut) %>%
              mutate(bin_id = cur_group_id())

            # Prepare rolling bins by grouping bin_ids with window of bin_cut size
            t <- data.frame()
            bin_cut_id <- 1
            max_bin <- max(temp_dat$bin_id)
            for (i in unique(temp_dat$bin_id)) {
              bin_1 <- i
              bin_2 <- ifelse(bin_1 + bin_cut <= max_bin, bin_1 + bin_cut, max_bin)
              bins_collect <- c(bin_1:bin_2)

              temp_bin_dat <- temp_dat %>%
                filter(bin_id %in% bins_collect) %>%
                mutate(bin_cut_id = bin_cut_id)

              t <- bind_rows(t, temp_bin_dat)

              bin_cut_id <- bin_cut_id + 1
            }

            # Summarize bin ranges for labeling
            bin_cut_match <- t %>%
              select(steps, bin_cut_id) %>%
              group_by(bin_cut_id) %>%
              mutate(bin_range = paste0("(", round(min(steps), 3), ", ", round(max(steps), 3), "]")) %>%
              mutate(bin_group = paste0(bin_range, ' Bin (', bin_cut_id, ') \n Slope = '))

            # Initialize dataframes for confidence intervals and slopes
            ci_temp <- data.frame()
            slope_temp <- data.frame()
            unique_bins <- unique(t$bin_cut_id)

            for (b in unique_bins) {
              temp_bin <- t[t$bin_cut_id == b,]
              lm_bin_temp <- suppressWarnings(lm(pred ~ steps, data = temp_bin))

              summary_lm <- suppressWarnings(summary(lm_bin_temp))
              slope <- coef(summary_lm)[2]
              se_slope <- summary_lm$coefficients[2, "Std. Error"]
              p_value <- summary_lm$coefficients[2, "Pr(>|t|)"]

              slope_frame <- data.frame(
                bin = bin_cut_match$bin_range[match(b, bin_cut_match$bin_cut_id)],
                slope = round(slope, 3),
                se = round(se_slope, 3),
                p_value = round(p_value, 5)) %>%
                mutate(sig = case_when(
                  .default = "",
                  p_value < 0.05 & p_value >= 0.01 ~ "*",
                  p_value < 0.01 & p_value >= 0.001 ~ '**',
                  p_value < 0.001 ~ '***'
                )) %>%
                unique()

              slope_temp <- bind_rows(slope_temp, slope_frame)

              ci_bin_temp <- predict(lm_bin_temp, interval = 'confidence')
              ci_bin_temp <- data.frame(ci = ci_bin_temp)

              ci_temp <- bind_rows(ci_temp, ci_bin_temp)
            }

            temp_dat <- cbind(t, ci_temp)

            temp_dat$bin_cut_group <- bin_cut_match$bin_group[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
            temp_dat$bin_range <- bin_cut_match$bin_range[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
            temp_dat$slope <- slope_temp$slope[match(temp_dat$bin_range, slope_temp$bin)]
            temp_dat$sig <- slope_temp$sig[match(temp_dat$bin_range, slope_temp$bin)]
            temp_dat$bin_cut_group <- paste0(temp_dat$bin_cut_group, temp_dat$slope, temp_dat$sig)
            temp_dat$bin_cut_group <- factor(temp_dat$bin_cut_group, levels = unique(temp_dat$bin_cut_group))

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = pred)) +
              geom_point(colour = 'gray5') +
              stat_smooth(
                aes(
                  colour = 'Linear Fit\n(w/ 95% CI)',
                  linetype = 'Linear Fit\n(w/ 95% CI)',
                  group = bin_cut_id
                ),  data = temp_dat, method = "lm", se = FALSE, formula = y~x, size = 1) +
              geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
              stat_smooth(
                aes(
                  colour = 'Loess Fit\n(w/ SE)',
                  linetype = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, formula = y~x,  size = 1) +
              facet_wrap(~bin_cut_group, scales = 'free_y') +
              geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
              theme_minimal() +
              scale_color_manual(values = c(
                'Linear Fit\n(w/ 95% CI)' = 'gray5',
                'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c(
                'Linear Fit\n(w/ 95% CI)' = 'solid',
                'Loess Fit\n(w/ SE)' = 'dashed' )) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = ifelse(output$parameters$outcome_type == 'Continuous', 'Prediction\n', paste0('p(', output$parameters$outcome, '= 1)\n')),
                color = 'Fit Type',
                linetype = 'Fit Type' ) +
              theme(
                axis.text = element_text(size = 14),
                axis.title = element_text(size = 16, face = 'bold'),
                panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
                panel.grid = element_blank(),
                legend.title.align = 0.5,
                legend.text.align = 0.25,
                legend.title = element_blank(),
                legend.text = element_text(size = 15, color = "gray5"),
                legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
                legend.position = "bottom",
                strip.text = element_text(size = 14, face = "bold"),
                strip.background = element_rect(fill = "gray", color = "gray5"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 15),
                plot.caption = element_text(size = 12, hjust = 0, face = 'italic')
              )


            push_output$figures$rolling_extended[[variables[var_index]]] <<- temp_figure_stand_alone
            push_output$slope_tables$rolling_extended[[variables[var_index]]] <<- slope_temp

          }, error = function(e) {
            message(paste("Skipping", variables[var_index], "due to error:", e$message))
          })
        }

        process_rolling_extended_bins(var)

      } # Rolling Extended Bins

      return(push_output)


    }


    push_output <- push_recovery(push_data = output$push, variables = names(output$push))

    diagnostics[['push']] <- push_output

  } # Push Runs

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

        {

          temp_var_bootstrap_data <- bootstrap_accuracies[, c(temp_var, 'accuracy', 'bootstrap_id')] %>%
            group_by(.data[[var]]) %>%
            summarise(mean_accuracy = mean(accuracy),
                      lower_ci = mean_accuracy - 1.96 * sd(accuracy) / sqrt(n()),
                      upper_ci = mean_accuracy + 1.96 * sd(accuracy) / sqrt(n()))

        } # Recover Var Data

        {

          temp_var_bootstrap_figure <- ggplot(temp_var_bootstrap_data, aes(x = .data[[temp_var]], y = mean_accuracy)) +
            geom_segment(aes(x = .data[[temp_var]], y = lower_ci, yend = upper_ci)) +
            geom_ribbon(aes(x = .data[[temp_var]], ymin = lower_ci, ymax = upper_ci, fill = "95% Confidence\nIntervals", group = 1), alpha = 0.05) +
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

        } # Compile Figure


        diagnostics$bootstrap$variable_figures[[var]] <- temp_var_bootstrap_figure

      }



    } # Confidence Intervals by Var


  } # Bootstraps

  {

    summary_diagnostics <- list()

    summary_diagnostics[['Performance Metrics']] <- data.frame(output$declared_model$results)


    importance <- tryCatch({
      varImp(output$declared_model)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })

    # Check if importance was successfully retrieved
    if (!is.null(importance)) {
      importance <- as.data.frame(importance$importance[1])
      names(importance) <- 'Importance'
      summary_diagnostics[['Variable Importance']] <- importance
    }

    predictions <- predict(output$declared_model, newdata = output$parameters$test_set)

    if (output$parameters$outcome_type == 'Binomial'){
      true_labels <- data.frame(output$parameters$test_set)[[output$parameters$outcome]]
      true_labels <- as.factor(true_labels)
      predictions <- factor(predictions, levels = levels(true_labels))
      conf_matrix <- confusionMatrix(predictions, true_labels)
      summary_diagnostics[['Confusion Matrix']] <- conf_matrix
      summary_diagnostics['Precision'] <- conf_matrix$byClass["Precision"]
      summary_diagnostics['Recall'] <-  conf_matrix$byClass["Recall"]
      summary_diagnostics['F1'] <-  conf_matrix$byClass["F1"]

      #final_model <- output$declared_model
      #train_data <- data.frame(output$parameters$train_set, check.names = F)
      #trainPred <- predict(final_model, newdata = train_data)
      #in_sample_accuracy <- sum(trainPred == train_data$direction) / nrow(train_data)
      #summary_diagnostics['In-Sample Accuracy'] <- in_sample_accuracy


    } else {

      mse <- mean((data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions)^2)
      rmse <- sqrt(mse)
      mae <- mean(abs(data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions))

      summary_diagnostics[['Mean Squared Error (MSE)']] <- mse
      summary_diagnostics[['Root Mean Squared Error (RMSE)']] <- rmse
      summary_diagnostics[['Mean Absolute Error (MAE)']] <- mae
    }

    diagnostics[['summary_diagnostics']] <- summary_diagnostics


  } # Summary


  return(diagnostics)

} # Diagnostic Plots & Stats (Updated 7/1/25)

