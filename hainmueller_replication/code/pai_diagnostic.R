#' PAI Diagnostic Retrieval Function
#'
#' @param output Output object from `pai()`
#' @param diagnostic Diagnostic element to return. Options: `placebo`, `push`, `bootstrap`, or `summary`
#' @param type Diagnostic subset to return. Options `data`, `figure`, or `distribution` (`bootstrap` ONLY)
#' @param variables Subset of variables to return (`figure` ONLY)
#' @param combine_plots Return `figure`(s) as combined plot using `cowplot` (CRAN). Defaults to `FALSE`
#'
#' @return
#' @export
#'
#' @examples
#' # placebo <- pai_diagnostic_retrieval(output = output, diagnostic = 'all')
#' # push <- pai_diagnostic_retrieval(output = output, diagnostic = 'push', type = 'figure', variables = c('var2', 'var4'), combine_plots = TRUE)
#' # bootstrap <- pai_diagnostic_retrieval(output = output, diagnostic = 'bootstrap', type = 'figure', combine_plots = TRUE)
#' # bootstrap_distribution <- pai_diagnostic_retrieval(output = output, diagnostic = 'bootstrap', type = 'distribution')
pai_diagnostic_retrieval <- function(output,
                                     diagnostic_type,
                                     type,
                                     variables = NULL,
                                     combine_plots = FALSE,
                                     rename_y_title = NULL,
                                     rename_x_title = NULL,
                                     rename_x_labels = NULL,
                                     rename_y_labels = NULL,
                                     rename_title = NULL,
                                     rename_subtitle = NULL,
                                     rename_caption = NULL
){

  diagnostic_output <- output$diagnostics[[diagnostic_type]] #diagnostic = 'placebo', 'push', 'bootstrap'
  diagnostic_retrieved <- list()

  if (diagnostic_type == 'placebo'){

    if(output$parameters$outcome_type == 'Binomial'){
      title_label = ' Predictive Accuracy '
    } else {
      title_label = ' RMSE '
    }

    if (type == 'all'){

      full <- diagnostic_output$placebo_full
      x_label = full$labels$x
      x_label = gsub('\\n', '', x_label)
      y_label = full$labels$y
      y_label = gsub('\\n', '', y_label)
      CI <- diagnostic_output$placebo_CI
      bootstrap <- diagnostic_output$bootstrap_omit_information
      #title_label = full$labels$y
      #title_label = gsub('\\n', '', title_label)
      full$labels$x = NULL
      CI$labels$y = NULL
      CI$labels$x = NULL
      bootstrap$labels$x = NULL
      bootstrap$labels$y = NULL

      if (is.null(variables)){
        full = full
        CI = CI
        bootstrap = bootstrap
      } else {
        full = full %+% subset(full$data, var %in% variables)
        CI = CI %+% subset(CI$data, var %in% variables)
        bootstrap = bootstrap %+% subset(bootstrap$data, dropped_var %in% variables)
      } # If Variable(s) Declared

      full_figure <- list(full + theme(legend.position = 'top'))
      other_figures <- list(CI  + labs(title = paste0('Distribution of ', title_label, '\nFrom Placebo Shuffling')) + theme(plot.title = element_text(hjust = 0.5)),
                            bootstrap  + labs(title = paste0('Distribution of ', title_label, '\nAfter Dropping Information')) + theme(plot.title = element_text(hjust = 0.5)))
      temp_figure <- cowplot::plot_grid(plotlist = full_figure, align = 'v')
      other_figure <- cowplot::plot_grid(plotlist = other_figures, ncol = 2, align = 'v')

      diagnostic_retrieved <- cowplot::plot_grid(temp_figure, other_figure, ncol = 1, rel_heights = c(2.5, 2))

      return(diagnostic_retrieved)

    } else if (type == 'figure'){

      if (is.null(variables)){
        diagnostic_retrieved[[1]] <- diagnostic_output$placebo_full #Returns Full Placebo Figure
      } else {
        diagnostic_retrieved[[1]] <- diagnostic_output$placebo_full %+% subset(diagnostic_output$placebo_full$data, var %in% variables) #Returns Placebo Figure Filtered by Var(s)
      } # If Variable(s) Declared

    } else if (type == 'bootstrap_drop_vars'){

      if (is.null(variables)){
        diagnostic_retrieved[[1]] <- diagnostic_output$bootstrap_omit_information #Returns Full Placebo Figure
      } else {
        diagnostic_retrieved[[1]] <- diagnostic_output$bootstrap_omit_information %+% subset(diagnostic_output$bootstrap_omit_information$data, var %in% variables) #Returns Placebo Figure Filtered by Var(s)
      } # If Variable(s) Declared


    } else if (type == 'placebo_confidence'){

      if (is.null(variables)){
        diagnostic_retrieved[[1]] <- diagnostic_output$placebo_CI #Returns Full Placebo Figure
      } else {
        diagnostic_retrieved[[1]] <- diagnostic_output$placebo_CI %+% subset(diagnostic_output$placebo_CI$data, var %in% variables) #Returns Placebo Figure Filtered by Var(s)
      } # If Variable(s) Declared

    } else {

      diagnostic_retrieved <- diagnostic_output$data #Returns Placebo Fit Data
      return(diagnostic_retrieved)

    }



  } #If Diagnostic is 'Placebo' # all, figure, placebo_confidence, bootstrap_drop_var

  if (diagnostic_type == 'push'){

    push_recovery <-  function(push_data = NULL,
                               variables = NULL,
                               bins = 10,
                               bin_cut = NULL){

      diagnostic_list <- list() # Empty List to Store Output

      {


        if (is.null(bins)){
          bins = 5
        }

        if (is.null(bin_cut)){
          bin_cut = 5
        }

      } #Set Parameters

      if (is.null(variables)){
        variables <- names(push_data)
      }

      for (var in 1:length(variables)) {

        process_variable <- function(var_index) {
          tryCatch({
            temp_dat <- data.frame(push_data[[variables[var_index]]])
            temp_dat <- temp_dat[, c(1, 3)] # Step & ACC
            names(temp_dat) <- c('steps', 'accuracy')
            temp_dat$steps <- as.numeric(temp_dat$steps)

            lm_temp <- lm(accuracy ~ steps, data = temp_dat)
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

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)'), method = "lm", se = FALSE, linetype = 'solid', size = 1) +
              geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, colour = 'gray5') +
              stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
              theme_minimal() +
              scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = 'Accuracy\n',
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

            diagnostic_list$figures$linear[[variables[var_index]]] <<- temp_figure_stand_alone
            diagnostic_list$slope_tables$linear[[variables[var_index]]] <<- slope_frame
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
            temp_dat <- temp_dat[, c(1, 3)]
            names(temp_dat) <- c('steps', 'accuracy')

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
              lm_bin_temp <- suppressWarnings(lm(accuracy ~ steps, data = temp_bin))

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

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
              stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin), method = "lm", se = FALSE, linetype = 'solid', size = 1) +
              geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
              stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
              theme_minimal() +
              scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = 'Accuracy\n',
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

            diagnostic_list$figures$static[[variables[var_index]]] <<- temp_figure_stand_alone
            diagnostic_list$slope_tables$static[[variables[var_index]]] <<- slope_temp

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
            temp_dat <- temp_dat[, c(1, 3)]
            names(temp_dat) <- c('steps', 'accuracy')

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
              lm_bin_temp <- suppressWarnings(lm(accuracy ~ steps, data = temp_bin))

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

            temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
              geom_point(colour = 'gray5', alpha = 1/4) +
              stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
              geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
              stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
              facet_wrap(~bin_cut_group, scales = 'free_x') +
              geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
              theme_minimal() +
              scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
              scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
              labs(
                x = paste0('\n', as.character(variables[var_index])),
                y = 'Accuracy\n',
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

            diagnostic_list$figures$rolling[[variables[var_index]]] <<- temp_figure_stand_alone
            diagnostic_list$slope_tables$rolling[[variables[var_index]]] <<- slope_temp

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
              temp_dat <- temp_dat[, c(1, 3)]
              names(temp_dat) <- c('steps', 'accuracy')

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
                lm_bin_temp <- suppressWarnings(lm(accuracy ~ steps, data = temp_bin))

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

              # Join slope info for labels and factors
              temp_dat$bin_cut_group <- bin_cut_match$bin_group[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
              temp_dat$bin_range <- bin_cut_match$bin_range[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
              temp_dat$slope <- slope_temp$slope[match(temp_dat$bin_range, slope_temp$bin)]
              temp_dat$sig <- slope_temp$sig[match(temp_dat$bin_range, slope_temp$bin)]
              temp_dat$bin_cut_group <- paste0(temp_dat$bin_cut_group, temp_dat$slope, temp_dat$sig)
              temp_dat$bin_cut_group <- factor(temp_dat$bin_cut_group, levels = unique(temp_dat$bin_cut_group))

              # Plot
              temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
                geom_point(colour = 'gray5') +
                stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
                geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
                stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
                facet_wrap(~bin_cut_group, scales = 'free_x') +
                geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
                theme_minimal() +
                scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
                scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
                labs(
                  x = paste0('\n', as.character(variables[var_index])),
                  y = 'Accuracy\n',
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

              diagnostic_list$figures$rolling_extended[[variables[var_index]]] <<- temp_figure_stand_alone
              diagnostic_list$slope_tables$rolling_extended[[variables[var_index]]] <<- slope_temp

            }, error = function(e) {
              message(paste("Skipping", variables[var_index], "due to error:", e$message))
            })
          }

          process_rolling_extended_bins(var)

          } # Rolling Extended Bins

      return(diagnostic_list)


    }


    diagnostic_retrieved <- push_recovery(push_data = output$push,
                                          variables = variables)



  } #If Diagnostic is 'Push'

  if (diagnostic_type == 'bootstrap'){

    if (type == 'data'){
      diagnostic_retrieved <- diagnostic_output$bootstrap_output
      return(diagnostic_retrieved)
    } #If Type is Bootstrap Data Output

    if (type == 'distribution'){
      diagnostic_retrieved[[1]] <- diagnostic_output$bootstrap_distribution
    } # If Type is Distribution Figure

    if (type == 'figure'){

      if (is.null(variables)){
        bootstrap_vars = names(diagnostic_output$bootstrap_output[!names(diagnostic_output$bootstrap_output) %in% c('accuracy', 'bootstrap_id')])
      } else {
        bootstrap_vars = variables
      } # If Variables = NULL, Get Vars from Boostrap Output Data

      diagnostic_retrieved <- list() # Initialize Empty List for Figures

      for (var in 1:length(bootstrap_vars)){
        diagnostic_retrieved[[bootstrap_vars[var]]] <- diagnostic_output$variable_figures[[bootstrap_vars[var]]]

      } #Collect Figures by Var

    } #If Type is Var Figure

  } #If Diagnostic = 'bootstrap'

  if (diagnostic_type == 'summary'){

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


    diagnostic_retrieved <- summary_diagnostics

    return(diagnostic_retrieved)

  }

  for (i in 1:length(diagnostic_retrieved)){

    temp_diagnostic_plot <- diagnostic_retrieved[[i]]

    {

      if (!is.null(rename_y_labels)){
        temp_diagnostic_plot <- temp_diagnostic_plot +
          scale_x_continuous(labels = rename_x_labels)
      } # Yaxis Labels

      if (!is.null(rename_x_labels)){
        temp_diagnostic_plot <- temp_diagnostic_plot +
          scale_x_discrete(labels = rename_x_labels)
      } # Xaxis Labels

      if (!is.null(rename_y_title)){
        temp_diagnostic_plot$labels$y = rename_y_title

      } #Yaxis Title

      if (!is.null(rename_x_title)){
        temp_diagnostic_plot$labels$x = rename_x_title

      } #Xaxis Title

      if (!is.null(rename_title)){
        temp_diagnostic_plot <- temp_diagnostic_plot +
          ggtitle(rename_title)
      } # Main Title

      if (!is.null(rename_subtitle)){
        temp_diagnostic_plot <- temp_diagnostic_plot +
          labs(subtitle = rename_subtitle)
      } # SubTitle

      if (!is.null(rename_caption)){
        temp_diagnostic_plot <- temp_diagnostic_plot +
          labs(caption = rename_caption)
      } # SubTitle

    } # Plot Label Additions/Fixes

    diagnostic_retrieved[[i]] <- temp_diagnostic_plot

  } # Plot Label Additions/Fixtures (only if figure diagnostics)

  if (combine_plots == TRUE){

    titles_vector = names(diagnostic_retrieved)

    diagnostic_figure_retrieved <- diagnostic_retrieved

    diagnostic_retrieved <- cowplot::plot_grid(plotlist = diagnostic_figure_retrieved,
                                               ncol = round(length(variables)/2, 0),
                                               scale = 0.9) # Arrange plots on a grid


    #diagnostic_retrieved <- diagnostic_retrieved +
    #draw_label("Step", x=0.5, y=  0, vjust=-1, angle= 0) +
    #draw_label("Predicted Accuracy", x=  0, y=0.5, vjust= 1, angle=90)

  } #If combine_plots = TRUE, Combine Them to Single Plot and Return Plot (Else, Return List of Figures)


  return(diagnostic_retrieved)


} # Updated 5/27/25



