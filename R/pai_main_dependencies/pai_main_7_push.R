push <- function(output, parameters){

  output <- output

  variables <- parameters$predictors # Get Variables

  push_output <- list() #Initialize List for Output

  for (variable in 1:length(variables)){
    temp_var = variables[variable] #Get Var
    data = parameters$test_set #Get Data (Test Data)
    temp_var_distribution = unique(data[,temp_var]) #Get Distribution of Temp Var

    factor_vars <- output$parameters$factors
    is_factor = TRUE # Default is_factor = T

    tryCatch({
      temp_min = min(data[,temp_var])
      temp_max = max(data[,temp_var])
      steps <- seq(temp_min, temp_max, by = (temp_max-temp_min)/100) # Steps = Min -> Max by Distance/100
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
                             outcome_type = parameters$outcome_type)

      temp_pred <- data.frame(
        step = temp_step,
        prediction = temp_pred) #Put Output in Temp DF

      var_push_predictions <- dplyr::bind_rows(var_push_predictions, temp_pred)

    }

    push_output[[temp_var]] <- var_push_predictions

    message("\033[37m           Completed Push Protocol For \033[0m", temp_var) #Print Update

  } #For Variable in Variables

  return(push_output)

} # Push + push_pred from Stepper
