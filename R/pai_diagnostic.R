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
#' Placebo Example
#' placebo <- pai_diagnostic_retrieval(output = output, diagnostic = 'placebo')
#' Push Example with combined plots and subset of var2 and var4
#' push <- pai_diagnostic_retrieval(output = output, diagnostic = 'push', type = 'figure', variables = c('var2', 'var4'), combine_plots = TRUE)
#' Bootstrap Example with variable-level plots
#' boostrap <- pai_diagnostic_retrieval(output = output, diagnostic = 'boostrap', type = 'figure', combine_plots = TRUE)
#' Boostrap Distribution Example
#' boostrap_distribution <- pai_diagnostic_retrieval(output = output, diagnostic = 'boostrap', type = 'distribution')
pai_diagnostic_retrieval <- function(output,
                                     diagnostic,
                                     type,
                                     variables = NULL,
                                     combine_plots = FALSE
){

  diagnostic_output <- output$diagnostics[[diagnostic]] #diagnostic = 'placebo', 'push', 'bootstrap'

  if (diagnostic == 'placebo'){

    if (type == 'figure'){

      if (is.null(variables)){
        diagnostic_retrieved <- diagnostic_output #Returns Full Placebo Figure
      } else {
        diagnostic_retrieved <- diagnostic_output %+% subset(diagnostic_output$data, var %in% variables) #Returns Placebo Figure Filtered by Var(s)
      } # If Variable(s) Declared


    } else {

      diagnostic_retrieved <- diagnostic_output$data #Returns Placebo Fit Data

    } #If Type is Figure vs. Data

  } #If Diagnostic = 'Placebo'

  if (diagnostic == 'push'){

    diagnostic_retrieved = list() #Initialize Empty List

    if (!is.null(variables)){

      for (var in 1:length(variables)){

        temp_var = variables[var]

        retrieval_type = ifelse(type == 'data', 'linear_fit', 'linear_plot')

        temp_var = variables[var]
        temp_retrieval <- diagnostic_output[[temp_var]][[retrieval_type]]
        diagnostic_retrieved[[as.character(temp_var)]] <- temp_retrieval

      }

    } else {

      variables <- names(diagnostic_output)
      vars <- names(diagnostic_output) #Get Var Names from Diagnostic[['push']]

      for (var in 1:length(vars)){

        temp_var = vars[var]

        retrieval_type = ifelse(type == 'linear_fit', 'linear_fit', 'linear_plot')

        temp_retrieval <- diagnostic_output[[temp_var]][[retrieval_type]]
        diagnostic_retrieved[[as.character(temp_var)]] <- temp_retrieval

      } # Grab Figure or Fit for Each Var
    }  #If No Variables Declared - Return All

    if (combine_plots == TRUE){

      titles_vector = names(diagnostic_retrieved)

      diagnostic_figure_retrieved <- diagnostic_retrieved

      for (i in 1:length(diagnostic_retrieved)){
        diagnostic_figure_retrieved[[i]]$labels$x = ''
        diagnostic_figure_retrieved[[i]]$labels$y = ''

      } #Remove Legends

      for (i in seq_along(diagnostic_figure_retrieved)) {
        diagnostic_figure_retrieved[[i]] <- arrangeGrob(
          diagnostic_figure_retrieved[[i]],
          top = textGrob(titles_vector[i], gp = gpar(fontsize = 14))
        )
      } # Add Var-Level Titles


      diagnostic_retrieved <- cowplot::plot_grid(plotlist = diagnostic_figure_retrieved,
                                                 ncol = round(length(variables)/2, 0),
                                                 scale = 0.9) # Arrange plots on a grid


      diagnostic_retrieved <- diagnostic_retrieved +
        draw_label("Step", x=0.5, y=  0, vjust=-1, angle= 0) +
        draw_label("Predicted Accuracy", x=  0, y=0.5, vjust= 1, angle=90)

    } #If combine_plots = TRUE, Combine Them to Single Plot and Return Plot (Else, Return List of Figures)


  } #If Diagnostic is 'Push'

  if (diagnostic == 'bootstrap'){

    if (type == 'data'){
      diagnostic_retrieved <- diagnostic_output$bootstrap_output
    } #If Type is Bootstrap Data Output

    if (type == 'distribution'){
      diagnostic_retrieved <- diagnostic_output$bootstrap_distribution
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

      if (combine_plots == TRUE){

        g_legend <- function(a.gplot) {
          tmp <- ggplot_gtable(ggplot_build(a.gplot))
          leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
          legend <- tmp$grobs[[leg]]
          return(legend)
        } #Get Legend Function

        legend <- g_legend(diagnostic_retrieved[[1]]) #Get Legend from First Plot

        for (i in 1:length(diagnostic_retrieved)){
          temp_figure <-  diagnostic_retrieved[[i]] +  guides(shape = 'none', fill = 'none', colour = 'none')
          diagnostic_retrieved[[i]] <- temp_figure
        } #Remove Legends

        {

          x_label = diagnostic_retrieved[[1]]$labels$x
          y_label = diagnostic_retrieved[[1]]$labels$y

        } #Grab Labels

        temp_figure <- cowplot::plot_grid(plotlist = diagnostic_retrieved,
                                          label_x = x_label,
                                          label_y = y_label)

        legend_below <- cowplot::plot_grid(NULL, legend, nrow = 2, align = 'v')

        diagnostic_retrieved <- cowplot::plot_grid(temp_figure, legend_below, ncol = 1, rel_heights = c(4, 1))



      } #If combine_plots = TRUE, Combine Them to Single Plot and Return Plot (Else, Return List of Figures)

    } #If Type is Var Figure

  } #If Diagnostic = 'bootstrap'

  if (diagnostic == 'summary'){

    summary_diagnostics <- list()

    summary_diagnostics[['Out of Sample Accuracy']] <- data.frame(output$declared_model$results)
    summary_diagnostics[['Variable Importance']] <- data.frame(varImp(output$declared_model)[1])

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

    } else {

      mse <- mean((data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions)^2)
      rmse <- sqrt(mse)
      mae <- mean(abs(data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions))

      summary_diagnostics[['Mean Squared Error (MSE)']] <- mse
      summary_diagnostics[['Root Mean Squared Error (RMSE)']] <- rmse
      summary_diagnostics[['Mean Absolute Error (MAE)']] <- mae
    }


    diagnostic_retrieved <- summary_diagnostics

  }

  return(diagnostic_retrieved)


}


pai_diagnostic_retrieval(output = temp_pai,
               diagnostic = 'push',
               type = 'figure',
               variables = c('traditional_electricity_share', 'renewablecapacity_3yr_average'),
               combine_plots = T)
