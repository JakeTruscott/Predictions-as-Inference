push <- function(output, parameters){

  output <- output

  predictors <- parameters$predictors
  interactions <- parameters$interactions
  if (!is.null(interactions)){
    variables <- c(predictors, interactions)
  } else {
    variables <- predictors
  } #Get Variables - Include Interactions (If Declared) and Assign Factor Labels (If Needed)

  push_output <- list() #Initialize List for Output

  for (variable in 1:length(variables)){
    temp_var = variables[variable] #Get Var
    data = data.frame(parameters$test_set, check.names = F) #Get Data (Test Data)
    temp_var_distribution = unique(data[,temp_var]) #Get Distribution of Temp Var

    factor_vars <- output$parameters$factors
    is_factor = TRUE # Default is_factor = T

    tryCatch({
      sd_var <- sd(data[, temp_var])
      steps <- seq(-2 * sd_var, 2 * sd_var, (4 * sd_var) / 100)
      is_factor = FALSE #Test to see if not actually factor/categorical distribution
    }, error = function(e){
      is_factor = TRUE # If it is Factor, Keep it True
    })

    if (is_factor == TRUE || temp_var %in% factor_vars){
      next
    }


    var_push_predictions <- data.frame() #Initialize Empty DF for Push Predictions

    for (step in steps){

      temp_step = step

      temp_pred <- push_pred(mod = output$declared_model,
                             var = temp_var,
                             stepper = temp_step,
                             Z = data,
                             outcome_type = parameters$outcome_type,
                             outcome_var = parameters$outcome,
                             parameters = parameters) #Calculate for Step[step]

      temp_pred <- data.frame(
        step = temp_step,
        onecount = temp_pred[1],
        acc = temp_pred[2]) #Put Output in Temp DF

      var_push_predictions <- dplyr::bind_rows(var_push_predictions, temp_pred)

    }

    push_output[[temp_var]] <- var_push_predictions

    message("\033[37m           Completed Push Protocol For \033[0m", temp_var) #Print Update

  } #For Variable in Variables

  return(push_output)

} # Push + push_pred from Stepper

push_pred <- function(mod, var, stepper, Z, outcome_type, outcome_var, parameters){

  if (outcome_type == 'Continuous'){
    Z[[var]] <- Z[[var]]  + stepper
    pred <- predict(mod, Z)
    true <- Z[[outcome_var]]
    dif <- pred - true
    return(dif)
  } else {

    Z[[var]] <- Z[[var]]  + stepper
    pred <- predict(mod, Z)
    true <- Z[[outcome_var]]
    onecount <- length(which(pred=='1'))/length(true)
    acc <- length(which(pred==true))/length(true)
    return(c(onecount, acc))
  }

  preds <- predict(mod, Z)
  return(length(which(trues==preds))/length(preds))


} #Predictive Accuracy from Steps
