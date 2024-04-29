pai_diagnostic <- function(output){

  diagnostics <- list()

  {

      placebo_vars <- output$placebo$var
      {

        placebo_figure <- pai_object$fit_change %>%
          filter(var %in% placebo_vars) %>%
          mutate(var_numeric = 1:nrow(.)) %>%
          mutate(var = ifelse(grepl("\\*", var), gsub("\\*", " x\n", var), var)) %>%
          ggplot(aes(x = factor(var))) +
          geom_hline(yintercept = 0, linetype = 2, linewidth = 1.1) +
          geom_point(aes(y = fit_change, colour = 'Prediction from Model Fit\nAfter Dropping Information'), size = 3, shape = 10) +
          geom_rect(aes(xmin = var_numeric - 0.15, xmax = var_numeric + 0.15,
                        ymin = min_change, ymax = max_change, fill = 'Range of Predicted\nAccuracy from Placebos'), colour = 'gray5') +
          geom_rect(aes(xmin = var_numeric - 0.15, xmax = var_numeric + 0.15,
                        ymin = lower_bound, ymax = upper_bound, fill = '95% Confidence\nInterval'), colour = 'gray5') +
          labs(y = ' ',
               x = '\nVariable\n',
               fill = ' ',
               colour = ' ') +
          scale_fill_manual(values = c("gray", "gray50")) +
          scale_colour_manual(values = 'black') +
          theme_minimal() +
          theme(legend.position = 'bottom',
                panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
                axis.text = element_text(size = 10, colour = 'black'),
                axis.title = element_text(size = 12, colour = 'black'))

      } #Render Placebo Figure

      diagnostics[['placebo']] <- placebo_figure #Add Placebo Figure to Diagnostic


  } #Placebo Iterations

  {

      linear_vars <- output$fit_change$var

      push_output <- output$push #Grab Push Output

      diagnostic_push <- list() #Create Empty List for Output

      for (var in linear_vars){
        diagnostic_push[[var]] <- list()
      } #Create Var-Level List for diagnostic_push

      for (var in linear_vars){

        temp_dat <- push_output[[var]] #Grab Temp Var
        base_plot <- ggplot(data = temp_dat, aes(x = step, y = acc)) +
          geom_point() #Generate Base Plot

        scott_info <- hist(data[[var]], breaks = "scott", plot = FALSE) #Get # Bins from Scot's Normal Reference Rule
        breakpoints <- length(scott_info$breaks)
        temp_dat$bin <- cut_interval(as.numeric(temp_dat$step), n = breakpoints) #Assign Bins

        for (temp_bin in 1:length(unique(temp_dat$bin))){

          bin_label <- unique(temp_dat$bin)[temp_bin]

          temp_bin_dat <- temp_dat %>%
            filter(bin == unique(temp_dat$bin)[temp_bin])
          lm_bin_temp <- lm(acc ~ step, data = temp_bin_dat)

          diagnostic_push[[var]][['linear_fit']][[as.character(bin_label)]] <- broom::tidy(lm_bin_temp) %>%
            mutate(sig = case_when(
              .default = '',
              p.value <= 0.05 & p.value > 0.01 ~ '*',
              p.value <=0.01 & p.value > 0.001 ~ '**',
              p.value <= 0.001 ~ '***'
            ))

          base_plot <- base_plot +
            geom_smooth(method = 'lm', formula = y ~ x, data = temp_bin_dat)


        } #Calculate LM by Bins & Append to Base Plot

        diagnostic_push[[var]][['linear_plot']] <- base_plot +
          theme_minimal() +
          labs(x = '\nStep\n',
               y = '\nAccuracy\n') +
          theme(
            panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 12, colour = 'black')
          )

      } #By Var - Calculate Linear Fit & Plot

    diagnostics[['push']] <- diagnostic_push

  } #Linear Fit Across Bins

  {

  } #Confidence Intervals

  return(diagnostics)

} #Linear Fit by Bins


