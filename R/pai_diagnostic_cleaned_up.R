pai_diagnostic <- function(pai_object, #PAI Output Object
                       plot_type = NULL, #Plot Type to Render
                       variables= NULL, #Variables to Render (Default = ALL)
                       bins = NULL, #Bins (Defaults to NULL if Placebo; 5 if Other)
                       add_aes = NULL #Add ggplot aesthetics (as list)
                       ){

  diagnostics <- list()

  if (plot_type %in% c('Placebo', 'placebo')){

    if (is.null(variables)){
      placebo_vars <- pai_object$fit_change$var
    } else {
      placebo_vars <- variables
    } # Based on Vars Declared, Declare Variables

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


  }

}
