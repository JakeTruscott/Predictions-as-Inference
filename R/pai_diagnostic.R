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
#' placebo <- pai_diagnostic_retrieval(output = output, diagnostic = 'all')
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
                                     combine_plots = FALSE,
                                     rename_y_title = NULL,
                                     rename_x_title = NULL,
                                     rename_x_labels = NULL,
                                     rename_y_labels = NULL,
                                     rename_title = NULL,
                                     rename_subtitle = NULL,
                                     rename_caption = NULL
){

  diagnostic_output <- output$diagnostics[[diagnostic]] #diagnostic = 'placebo', 'push', 'bootstrap'
  diagnostic_retrieved <- list()

  if (diagnostic == 'placebo'){

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


  } #If Diagnostic is 'Push'

  if (diagnostic == 'bootstrap'){

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

  if (diagnostic == 'summary'){

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


} # Updated 6/23



