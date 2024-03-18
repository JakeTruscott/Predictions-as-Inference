#' PAI Diagnostic Tool
#'
#' @param pai_object A compiled PAI object obtained using pai_main().
#' @param variables A defined list of variables to analyze.
#' @param bins Numeric value of bins to assess predicted response across variance in the distribution of a variable; Default = 5
#' @param bin_cut Numeric value of cut point between bins; Default = 5
#'
#' @return
#' @export
#'
#' @examples
pai_diagnostic <- function(pai_object = NULL,
                           variables = NULL,
                           bins = NULL,
                           bin_cut = NULL){

  diagnostic_list <- list()
  combined_diagnostic_list <- list()

  {

    if (is.null(variables)){
      variables = unique(pai_object$var)
      variables = variables[!grepl('\\:', variables)]
    }

    if (is.null(bins)){
      bins = 5
    }

    if (is.null(bin_cut)){
      bin_cut = 5
    }


  } #Set Parameters

  {

    temp <- pai_object$drop_acc.ch %>%
      mutate(var_numeric = 1:nrow(pai_object$drop_acc.ch)) %>%
      mutate(var = ifelse(grepl("\\*", var), gsub("\\*", " x\n", var), var))


    temp_figure <- ggplot(data = temp, aes(x = var_numeric, y = fit_change)) +
      geom_rect(aes(xmin = var_numeric - 0.15, xmax = var_numeric + 0.15,
                    ymin = min_change, ymax = max_change, fill = 'Range of Predicted Accuracy from Placebos'),
                color = 'black') +
      geom_point(aes(color = 'Prediction from Model Fit After Dropping Information'), size = 2.5) +
      geom_hline(yintercept = 0, linetype = 2) +
      scale_fill_manual(values = 'gray', name = NULL) +
      scale_color_manual(values = 'gray5', name = NULL) +
      scale_x_continuous(breaks = seq(min(temp$var_numeric), max(temp$var_numeric), 1), labels = temp$var) +
      theme_minimal() +
      theme_test(base_size = 14) +
      labs(
        x = '\n',
        y = 'Change in Predicted Accuracy\nWith All True Data\n') +
      theme(
        axis.text = element_text(size = 14),
        panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
        legend.title.align = 0.5,
        legend.text.align = 0.25,
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "gray5"),
        legend.position = "bottom",
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "gray", color = "gray5"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))

    diagnostic_list$figures[['placebo']] <- temp_figure


  } #Placebo

  {

    data <- pai_object$push

    for (var in names(data)){

      var <- gsub('as\\.factor\\(', '', gsub('\\)', '', var))

      if (!var %in% names(data)){
        next
      }

      temp_dat <- data.frame(data[[var]])
      temp_dat <- temp_dat[,c(1,3)]
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
          p_value < 0.01 & p_value >= 0.001 ~ '**',
          p_value < 0.001 ~ '***'
        )) %>%
        unique()


      {
        temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(colour = 'gray5') +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)'), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = var,
            x = paste("\nSteps\n(", expression("\U00B1 2 \U03C3"), ")"),
            y = 'Accuracy\n',
            color = 'Fit Type',
            linetype = 'Fit Type'
          ) +
          theme(
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
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
        } #Stand Alone

      {
        temp_figure_combined <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(shape = 1, colour = 'gray5', fill = NA, alpha = 1/2) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)'), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = 'Linear Fit',
            x = "\n",
            y = '\n',
            color = 'Fit Type',
            linetype = 'Fit Type'
          ) +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = 'bold'),
            panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
            panel.grid = element_blank(),
            legend.title.align = 0.5,
            legend.text.align = 0.25,
            legend.title = element_blank(),
            legend.text = element_text(size = 15, color = "gray5"),
            legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
            legend.position = "none",
            strip.text = element_text(size = 14, face = "bold"),
            strip.background = element_rect(fill = "gray", color = "gray5"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 15),
            plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))


        temp_figure

      } #Combined Diagnostic

      diagnostic_list$figures$linear[[var]] <- temp_figure_stand_alone
      diagnostic_list$slope_tables$linear[[var]] <- slope_frame
      combined_diagnostic_list$figures$linear[[var]] <- temp_figure_combined

    }


  } #Full Linear Fit

  {

    data <- pai_object$push

    for (var in names(data)){

      var <- gsub('as\\.factor\\(', '', gsub('\\)', '', var))

      if (!var %in% names(data)){
        next
      }

      temp_dat <- data.frame(data[[var]])
      temp_dat <- temp_dat[,c(1,3)]
      names(temp_dat) <- c('steps', 'accuracy')

      temp_dat$steps <- as.numeric(temp_dat$steps)
      num_bins = bins
      temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

      temp_dat <- temp_dat %>%
        rowwise() %>%
        mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                            gsub(".*\\,", "", gsub("\\]", "", bin)),
                            0)) %>%
        mutate(cut = as.numeric(cut))

      {
        ci_temp <- data.frame()
        slope_temp <- data.frame()
        unique_bins <- unique(temp_dat$bin)

        for (b in unique_bins){

          temp_bin <- temp_dat[temp_dat$bin == b,]
          lm_bin_temp <- lm(accuracy ~ steps, data = temp_bin)

          summary_lm <- summary(lm_bin_temp)
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
              p_value < 0.01 & p_value >= 0.001 ~ '**',
              p_value < 0.001 ~ '***'
            )) %>%
            unique()

          slope_temp <- bind_rows(slope_temp, slope_frame)

          ci_bin_temp <- predict(lm_bin_temp, interval = 'confidence')
          ci_bin_temp <- data.frame(ci = ci_bin_temp)


          ci_temp <- bind_rows(ci_temp, ci_bin_temp)

        }

        temp_dat <- cbind(temp_dat, ci_temp)

        } #Slopes

      {

        temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(colour = 'gray5') +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = var,
            x = paste("\nSteps\n(", expression("\U00B1 2 \U03C3"), ")"),
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


      } #Stand Alone Figure

      {
        temp_figure_combined <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(shape = 1, colour = 'gray5', fill = NA, alpha = 1/2) +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = 'Static Bins',
            x = "\n",
            y = "\n",
            color = 'Fit Type',
            linetype = 'Fit Type'
          ) +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = 'bold'),
            panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
            panel.grid = element_blank(),
            legend.title.align = 0.5,
            legend.text.align = 0.25,
            legend.title = element_blank(),
            legend.text = element_text(size = 15, color = "gray5"),
            legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
            legend.position = "none",
            strip.text = element_text(size = 10, face = "bold"),
            strip.background = element_rect(fill = "gray", color = "gray5"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 15),
            plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))


      } #Combined

      diagnostic_list$figures$static[[var]] <- temp_figure_stand_alone
      combined_diagnostic_list$figures$static[[var]] <- temp_figure_combined
      diagnostic_list$slope_tables$static[[var]] <- slope_temp

    }


  } #Static Bins

  {

    data <- pai_object$push

    for (var in names(data)){

      var <- gsub('as\\.factor\\(', '', gsub('\\)', '', var))

      if (!var %in% names(data)){
        next
      }

      temp_dat <- data.frame(data[[var]])
      temp_dat <- temp_dat[,c(1,3)]
      names(temp_dat) <- c('steps', 'accuracy')

      temp_dat$steps <- as.numeric(temp_dat$steps)
      num_bins = bins
      temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

      temp_dat <- temp_dat %>%
        rowwise() %>%
        mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                            gsub(".*\\,", "", gsub("\\]", "", bin)),
                            0)) %>%
        mutate(cut = as.numeric(cut)) %>%
        group_by(cut) %>%
        mutate(bin_id = cur_group_id())

      {
        t <- data.frame()
        bin_cut_id <- 1
        for (i in unique(temp_dat$bin_id)){
          max_bin <- max(temp_dat$bin_id)
          bin_1 <- i
          bin_2 <- ifelse(i == max_bin, i, bin_1 + 1)
          bin_1_dat <- temp_dat %>%
            filter(bin_id == bin_1)
          bin_2_dat <- temp_dat %>%
            filter(bin_id == bin_2)

          if (i == max_bin){
            temp_bin_dat <- bin_1_dat
            temp_bin_dat$bin_cut_id <- bin_cut_id
          } else {
            temp_bin_dat <- bind_rows(bin_1_dat, bin_2_dat)
            temp_bin_dat$bin_cut_id <- bin_cut_id
          }

          t <- bind_rows(t, temp_bin_dat)

          bin_cut_id <- bin_cut_id + 1

        }

        bin_cut_match <- t %>%
          select(steps, bin_cut_id) %>%
          group_by(bin_cut_id) %>%
          mutate(bin_range = paste0("(", round(min(steps), 3), ", ", round(max(steps), 3), "]")) %>%
          mutate(bin_group = paste0(bin_range, ' Bin (', bin_cut_id, ') \n Slope = '))

        ci_temp <- data.frame()
        slope_temp <- data.frame()
        unique_bins <- unique(t$bin_cut_id)

        for (b in unique_bins){

          temp_bin <- t[t$bin_cut_id == b,]
          lm_bin_temp <- lm(accuracy ~ steps, data = temp_bin)

          summary_lm <- summary(lm_bin_temp)
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
        } #Slopes

      temp_dat$bin_cut_group <- bin_cut_match$bin_group[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
      temp_dat$bin_range <- bin_cut_match$bin_range[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
      temp_dat$slope <- slope_temp$slope[match(temp_dat$bin_range, slope_temp$bin)]
      temp_dat$sig <- slope_temp$sig[match(temp_dat$bin_range, slope_temp$bin)]
      temp_dat$bin_cut_group <- paste0(temp_dat$bin_cut_group, temp_dat$slope, temp_dat$sig )
      temp_dat$bin_cut_group <- factor(temp_dat$bin_cut_group, levels = unique(temp_dat$bin_cut_group))

      {
        temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(colour = 'gray5') +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          facet_wrap(~bin_cut_group) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = var,
            x = paste("\nSteps\n(", expression("\U00B1 2 \U03C3"), ")"),
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


      } #Stand Alone

      {
        temp_figure_combined <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(shape = 1, colour = 'gray5', fill = NA, alpha = 1/2) +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          facet_wrap(~bin_cut_group) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = 'Rolling Bins',
            x = "\n",
            y = '\n',
            color = 'Fit Type',
            linetype = 'Fit Type'
          ) +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = 'bold'),
            panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
            panel.grid = element_blank(),
            legend.title.align = 0.5,
            legend.text.align = 0.25,
            legend.title = element_blank(),
            legend.text = element_text(size = 15, color = "gray5"),
            legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
            legend.position = "none",
            strip.text = element_text(size = 8, face = "bold"),
            strip.background = element_rect(fill = "gray", color = "gray5"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 15),
            plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))

      } #Combined


      diagnostic_list$figures$rolling[[var]] <- temp_figure_stand_alone
      combined_diagnostic_list$figures$rolling[[var]] <- temp_figure_combined
      diagnostic_list$slope_tables$rolling[[var]] <- slope_temp

    }

  } # Rolling Bins

  {

    data <- pai_object$push

    for (var in names(data)){

      var <- gsub('as\\.factor\\(', '', gsub('\\)', '', var))

      if (!var %in% names(data)){
        next
      }

      temp_dat <- data.frame(data[[var]])
      temp_dat <- temp_dat[,c(1,3)]
      names(temp_dat) <- c('steps', 'accuracy')

      temp_dat$steps <- as.numeric(temp_dat$steps)
      num_bins = bins
      temp_dat$bin <- cut_interval(temp_dat$steps, n = num_bins)

      temp_dat <- temp_dat %>%
        rowwise() %>%
        mutate(cut = ifelse(is.numeric(as.numeric(gsub(".*\\,", "", gsub("\\]", "", bin)))),
                            gsub(".*\\,", "", gsub("\\]", "", bin)),
                            0)) %>%
        mutate(cut = as.numeric(cut)) %>%
        group_by(cut) %>%
        mutate(bin_id = cur_group_id())


      {
        t <- data.frame()
        bin_cut_id <- 1
        for (i in unique(temp_dat$bin_id)){
          max_bin <- max(temp_dat$bin_id)
          bin_1 <- i
          bin_2 = ifelse(bin_1 + bin_cut <= max_bin, bin_1 + bin_cut, max_bin)

          bins_collect <- c(bin_1:bin_2)

          temp_bin_dat <- temp_dat %>%
            filter(bin_id %in% bins_collect) %>%
            mutate(bin_cut_id = bin_cut_id)

          t <- bind_rows(t, temp_bin_dat)

          bin_cut_id <- bin_cut_id + 1

        }

        bin_cut_match <- t %>%
          select(steps, bin_cut_id) %>%
          group_by(bin_cut_id) %>%
          mutate(bin_range = paste0("(", round(min(steps), 3), ", ", round(max(steps), 3), "]")) %>%
          mutate(bin_group = paste0(bin_range, ' Bin (', bin_cut_id, ') \n Slope = '))

        ci_temp <- data.frame()
        slope_temp <- data.frame()
        unique_bins <- unique(t$bin_cut_id)

        for (b in unique_bins){

          temp_bin <- t[t$bin_cut_id == b,]
          lm_bin_temp <- lm(accuracy ~ steps, data = temp_bin)

          summary_lm <- summary(lm_bin_temp)
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
        } #Slopes

      temp_dat$bin_cut_group <- bin_cut_match$bin_group[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
      temp_dat$bin_range <- bin_cut_match$bin_range[match(temp_dat$bin_cut_id, bin_cut_match$bin_cut_id)]
      temp_dat$slope <- slope_temp$slope[match(temp_dat$bin_range, slope_temp$bin)]
      temp_dat$sig <- slope_temp$sig[match(temp_dat$bin_range, slope_temp$bin)]
      temp_dat$bin_cut_group <- paste0(temp_dat$bin_cut_group, temp_dat$slope, temp_dat$sig )
      temp_dat$bin_cut_group <- factor(temp_dat$bin_cut_group, levels = unique(temp_dat$bin_cut_group))

      {
        temp_figure_stand_alone <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(colour = 'gray5') +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          facet_wrap(~bin_cut_group) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = var,
            x = paste("\nSteps\n(", expression("\U00B1 2 \U03C3"), ")"),
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


      } #Stand Alone

      {
        temp_figure_combined <- ggplot(temp_dat, aes(x = steps, y = accuracy)) +
          geom_point(shape = 1, colour = 'gray5', fill = NA, alpha = 1/2) +
          geom_vline(aes(xintercept = cut), linetype = 2, alpha = 1/3) +
          stat_smooth(aes(colour = 'Linear Fit\n(w/ 95% CI)', group = bin_cut_id), data = temp_dat, method = "lm", se = FALSE, linetype = 'solid', size = 1) +
          geom_errorbar(aes(ymin = ci.lwr, ymax = ci.upr), width = 0, colour = 'gray5') +
          stat_smooth(aes(colour = 'Loess Fit\n(w/ SE)'), method = "loess", se = TRUE, linetype = 'dashed', size = 1) +
          facet_wrap(~bin_cut_group) +
          theme_minimal() +
          scale_color_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'gray5', 'Loess Fit\n(w/ SE)' = 'red')) +
          scale_linetype_manual(values = c('Linear Fit\n(w/ 95% CI)' = 'solid', 'Loess Fit\n(w/ SE)' = 'dashed')) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5', alpha = 1/4) +
          labs(
            title = 'Rolling Bins (Extended)',
            color = 'Fit Type',
            linetype = 'Fit Type',
            x = "\n",
            y = "\n"
          ) +
          theme(
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 10, face = 'bold'),
            panel.border = element_rect(linewidth = 1, color = "gray5", fill = NA),
            panel.grid = element_blank(),
            legend.title.align = 0.5,
            legend.text.align = 0.25,
            legend.title = element_blank(),
            legend.text = element_text(size = 15, color = "gray5"),
            legend.box.background = element_rect(size = 1, color = 'gray5', fill = NA),
            legend.position = "none",
            strip.text = element_text(size = 8, face = "bold"),
            strip.background = element_rect(fill = "gray", color = "gray5"),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 15),
            plot.caption = element_text(size = 12, hjust = 0, face = 'italic'))

        temp_figure_combined
      } #Combined


      diagnostic_list$figures$rolling_extended[[var]] <- temp_figure_stand_alone
      combined_diagnostic_list$figures$rolling_extended[[var]] <- temp_figure_combined
      diagnostic_list$slope_tables$rolling_extended[[var]] <- slope_temp

    }

  } #Rolling Extended Bins

  {

    diagnostic_list$combined_diagnostics <- list()

    for (var in names(data)){

      var <- gsub('as\\.factor\\(', '', gsub('\\)', '', var))

      if (!var %in% names(data)){
        next
      }

      arranged_temp <- suppressMessages(cowplot::ggdraw() +
                                          draw_plot(combined_diagnostic_list$figures$rolling[[var]], 0, 0, 0.5, 0.5) +
                                          draw_plot(combined_diagnostic_list$figures$rolling_extended[[var]], 0.5, 0, 0.5, 0.5) +
                                          draw_plot(combined_diagnostic_list$figures$linear[[var]], 0, 0.5, 0.5, 0.5) +
                                          draw_plot(combined_diagnostic_list$figures$static[[var]], 0.5, 0.5, 0.5, 0.5))

      diagnostic_list$combined_diagnostics[[var]] <- arranged_temp
    }


  } #Combined Diagnostic


  return(diagnostic_list)


}
