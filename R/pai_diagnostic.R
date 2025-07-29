#' Title
#'
#' @param pai_object PAI Output Object
#' @param output_directory  Folder directory -- Will only return object to global environment if NULL
#'
#' @return
#' @export
#'
#' @examples
pai_diagnostic_retrieval <- function(pai_object,
                                     output_directory = NULL){


  check_and_install_packages <- function(){

    packages <- c('caret', 'dplyr', 'stringr', 'doParallel', 'broom', 'cowplot',
                  'grid', 'gridExtra', 'doSNOW', 'gridtext', 'patchwork',
                  'randomForest', 'rlist', 'xgboost', 'ggplot2', 'ggridges', 'tidyr')

    missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

    if (length(missing_packages) > 0) {
      message("The following necessary packages are missing: ", paste(missing_packages, collapse = ", "))

      response <- readline("Would you like to install them? (Y/N): ")
      if (tolower(response) %in% c("yes", "y")) {
        install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
        message("Packages installed successfully.")
      } else {
        stop("Packages must be installed prior to compilation. Please install and try again.")
      }
    }

    invisible(lapply(packages, function(pkg) {
      suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
    }))

    message('All Packages Installed and Deployed Correctly')
  }

  check_and_install_packages() # Deploy



  temp_diagnostic <- list() # Empty List to Store Output

  if (!is.null(output_directory)){
    if (!dir.exists(output_directory)){
      dir.create(output_directory)
    }
  } # If  Output Dir Doesn't Exist -- Create iT

  message('Beginning Diagnostic Recovery...')

  {

    temp_diagnostic[['placebo']] <- suppressMessages(
      suppressWarnings(
        pai_object$diagnostics$placebo
      )
    )


    if (!is.null(output_directory)){

      placebo_output_directory <- file.path(output_directory, 'placebo')

      if (!dir.exists(placebo_output_directory)){
        dir.create(placebo_output_directory)
      }

      for (i in 1:length(temp_diagnostic[['placebo']])){

        temp_figure <- temp_diagnostic[['placebo']][[i]]
        temp_figure_name <- names(temp_diagnostic[['placebo']][i])

        suppressMessages(
          suppressWarnings(
            ggsave(temp_figure,
                   filename = file.path(placebo_output_directory, paste0(temp_figure_name, '.png')),
                   width = 10,
                   height = 8,
                   units = 'in',
                   bg = 'white')
          )
        )




      } # Push Each to Placebo Folder

    } # If Output = TRUE


  } # Placebo Output

  message(' --- Completed Placebo')

  {

    temp_diagnostic[['push_predictions']] <- pai_object$diagnostics$push

    if (!is.null(output_directory)){
      if (!dir.exists(file.path(output_directory, 'push_predictions'))){
        dir.create(file.path(output_directory, 'push_predictions'))
      }

      for (type in 1:length(pai_object$diagnostics$push$figures)){

        temp_type <- pai_object$diagnostics$push$figures[[type]]
        temp_type_name <- names(pai_object$diagnostics$push$figures[type])

        if (!dir.exists(file.path(output_directory, 'push_predictions', temp_type_name))){
          dir.create(file.path(output_directory, 'push_predictions', temp_type_name))
        }

        for (var in 1:length(temp_type)){
          temp_var <- names(temp_type[var])
          temp_var_figure <- temp_type[[var]]
          temp_output_path <- file.path(filename = file.path(output_directory, 'push_predictions', temp_type_name, paste0(temp_var, '.png')))
          suppressWarnings(ggsave(temp_var_figure,
                                  filename = temp_output_path,
                                  width = 10,
                                  height = 8,
                                  units = 'in',
                                  bg = 'white'))

        }

      }


    }




  } # Push Figures

  message(' --- Completed Push')

  {


    temp_diagnostic[['bootstraps']] <- list()
    temp_diagnostic[['bootstraps']][['temp_bootstrap_data']] <- pai_object$diagnostics$bootstrap$bootstrap_pai_object
    temp_diagnostic[['bootstraps']][['temp_bootstrap_distribution']] <- pai_object$diagnostics$bootstrap$bootstrap_distribution

    if (!is.null(output_directory)){
      if (!dir.exists(file.path(output_directory, 'bootstraps'))){
        dir.create(file.path(output_directory, 'bootstraps'))
      }

      suppressWarnings(ggsave(pai_object$diagnostics$bootstrap$bootstrap_distribution,
                              filename = file.path(output_directory, 'bootstraps', 'bootstrap_distribution.png'),
                              width = 10,
                              height = 8,
                              units = 'in',
                              bg = 'white'))

    }



    temp_diagnostic[['bootstraps']][['variable_figures']] <- list()

    temp_vars <- names(pai_object$diagnostics$bootstrap$variable_figures)

    for (var in 1:length(temp_vars)){

      temp_diagnostic[['bootstraps']][['variable_figures']][[as.character(temp_vars[var])]] <- pai_object$diagnostics$bootstrap$variable_figures[[as.character(temp_vars[var])]]


      if (!is.null(output_directory)){

        temp_pai_object_path <- file.path(output_directory, 'bootstraps', paste0(temp_vars[var], '.png'))

        suppressWarnings(ggsave(pai_object$diagnostics$bootstrap$variable_figures[[as.character(temp_vars[var])]],
                                filename = temp_pai_object_path,
                                width = 10,
                                height = 8,
                                units = 'in',
                                bg = 'white'))


      }


    }


  } # Bootstrap Figures

  message(' --- Completed Bootstraps')

  {

    summary_diagnostics <- list()

    summary_diagnostics[['Performance Metrics']] <- data.frame(pai_object$declared_model$results)


    importance <- tryCatch({
      varImp(pai_object$declared_model)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })

    # Check if importance was successfully retrieved
    if (!is.null(importance)) {
      importance <- as.data.frame(importance$importance[1])
      names(importance) <- 'Importance'
      summary_diagnostics[['Variable Importance']] <- importance
    }

    predictions <- predict(pai_object$declared_model, newdata = pai_object$parameters$test_set)

    if (pai_object$parameters$outcome_type == 'Binomial'){
      true_labels <- data.frame(pai_object$parameters$test_set)[[pai_object$parameters$outcome]]
      true_labels <- as.factor(true_labels)
      predictions <- factor(predictions, levels = levels(true_labels))
      conf_matrix <- confusionMatrix(predictions, true_labels)
      summary_diagnostics[['Confusion Matrix']] <- conf_matrix
      summary_diagnostics['Precision'] <- conf_matrix$byClass["Precision"]
      summary_diagnostics['Recall'] <-  conf_matrix$byClass["Recall"]
      summary_diagnostics['F1'] <-  conf_matrix$byClass["F1"]

      #final_model <- pai_object$declared_model
      #train_data <- data.frame(pai_object$parameters$train_set, check.names = F)
      #trainPred <- predict(final_model, newdata = train_data)
      #in_sample_accuracy <- sum(trainPred == train_data$direction) / nrow(train_data)
      #summary_diagnostics['In-Sample Accuracy'] <- in_sample_accuracy


    } else {

      mse <- mean((data.frame(pai_object$parameters$test_set)[[pai_object$parameters$outcome]] - predictions)^2)
      rmse <- sqrt(mse)
      mae <- mean(abs(data.frame(pai_object$parameters$test_set)[[pai_object$parameters$outcome]] - predictions))

      summary_diagnostics[['Mean Squared Error (MSE)']] <- mse
      summary_diagnostics[['Root Mean Squared Error (RMSE)']] <- rmse
      summary_diagnostics[['Mean Absolute Error (MAE)']] <- mae
    }


    temp_diagnostic[['summary']] <- summary_diagnostics

    if (!is.null(output_directory)){

      if (!dir.exists(file.path(output_directory, 'summary_diagnostics'))){
        dir.create(file.path(output_directory, 'summary_diagnostics'))
      }

      save(summary_diagnostics, file = file.path(output_directory, 'summary_diagnostics', 'summary_diagnostics.rdata'))
      output_text <- capture.output(print(summary_diagnostics))
      writeLines(as.character(output_text),  file.path(output_directory, 'summary_diagnostics', 'summary_diagnostics.txt'))


    }


  } # Summary

  message(' --- Completed Summary')

  return(temp_diagnostic) # Return When Done

}
