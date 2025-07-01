declared_model <- function(parameters){


  if (parameters$outcome_type == 'Binomial') {
    outcome_var <- all.vars(stats::as.formula(parameters$base_formula))[1]
    parameters$train_set[[outcome_var]] <- factor(parameters$train_set[[outcome_var]])
    rhs <- as.character(stats::as.formula(parameters$base_formula))[3]
    parameters$base_formula <- paste0("factor(", outcome_var, ") ~ ", rhs) # Modify formula to wrap DV in factor() if not already wrapped
  }

  declared_model <- suppressMessages(suppressWarnings(caret::train(form = stats::as.formula(parameters$base_formula),
                                                                   data = parameters$train_set,
                                                                   metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
                                                                   method = as.character(parameters$model),
                                                                   trControl = parameters$train_control,
                                                                   localImp = TRUE)))

  return(declared_model)

} #Run Declared ML Model
