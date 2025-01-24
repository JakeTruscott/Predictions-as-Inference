declared_model <- function(parameters){

  declared_model <- suppressMessages(suppressWarnings(caret::train(form = as.formula(parameters$base_formula),
                                                                   data = parameters$train_set,
                                                                   metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
                                                                   method = as.character(parameters$model),
                                                                   trControl = parameters$train_control,
                                                                   localImp = TRUE)))

  return(declared_model)

} #Run Declared ML Model
