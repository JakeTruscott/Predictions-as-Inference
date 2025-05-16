dropping_vars <- function(parameters, output){

  fit_change <- data.frame() #Initialize Empty DF to Store Fit Changes from Dropping Vars
  bootstrap_drop_var <- list() #Initialize Empty List to Store Bootstrap Output
  omitting_variables_models_output <- list() # Initialize Empty List to Store Drop Var Model Outputs (Will Store if Param = TRUE)
  vars_to_drop <- c() #Initialize Empty Object for Vars to Drop (Based on Params Declaration)

  if (parameters$list_drop_vars == 'FALSE'){
    {
      combinations <- data.frame() # Create Empty DF for Combinations of Drop Vars
      vars_to_drop <- parameters$predictors

      for (drop_var in 1:length(vars_to_drop)){

        current_var <- vars_to_drop[drop_var]
        temp_drop_var <- vars_to_drop[drop_var]
        other_vars <- vars_to_drop[!vars_to_drop %in% unlist(temp_drop_var)]


        other_vars <- other_vars[!grepl('(\\*|\\:)', other_vars)]
        other_vars <- ifelse(other_vars %in% unlist(parameters$factors), paste0('factor(', other_vars, ')'), other_vars)
        other_interactions <- vars_to_drop[!vars_to_drop %in% unlist(temp_drop_var)]
        other_interactions <- other_interactions[grepl('(\\*|\\:)', other_interactions)]
        other_cleaned_interactions <- c()
        if(!is.null(other_interactions)){
          for (interaction in other_interactions){
            temp_interaction <- c()
            terms <- unlist(stringr::str_split(interaction, pattern = '(\\*|\\:)'))
            interaction_pattern <- ifelse(grepl('\\*', interaction), '*', ':')
            for (term in terms){
              term <- ifelse(term %in% unlist(parameters$factors), paste0('factor(', term, ')'), term)
              temp_interaction <- c(temp_interaction, term)
            } # For Each Term in Interaction
            temp_interaction <- paste(temp_interaction, collapse = interaction_pattern)
            other_cleaned_interactions <- c(other_cleaned_interactions, temp_interaction)
          } #For Each Interaction - Separate + Add if factor() if Factor & Recombine
        } #If 'other_interactions' isn't empty
        other_vars <- c(other_vars, other_cleaned_interactions) #Combine All Vars Back Into Single Object

        if (length(other_vars) == 0) {
          message("\033[37m           Insufficient Variables in Group: \033[0m", current_var, '...Moving On') # Print Update
          next
        }

        outcome_variable <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', parameters$outcome, ')'), parameters$outcome) # Add factor() to outcome_var if Binomially Distributed

        temp_combination <- paste0(outcome_variable, '~', paste(other_vars, collapse = '+'))

        temp_combination <- data.frame(
          temp_combination = temp_combination,
          dropped_var = temp_drop_var) #Collapse Into Single Temp DF

        combinations <- dplyr::bind_rows(combinations, temp_combination) #Add to Combinations Frame

      }


    } # Create Combinations (If Non-List Drop Vars)

  } else {
    {

      combinations <- data.frame()
      vars_to_drop <- parameters$drop_vars

      for (drop_var in 1:length(vars_to_drop)){

        current_var <- names(vars_to_drop[drop_var])
        temp_drop_var <- parameters$drop_vars[[drop_var]] # Retrieve Temporary Var
        other_vars <- parameters$predictors[!parameters$predictors %in% unlist(temp_drop_var)] #Retrieve Other Vars
        other_vars <- other_vars[!grepl(paste(temp_drop_var, collapse = "|"), other_vars)] #Further Remove if Temp Var in Interaction
        other_vars <- ifelse(other_vars %in% unlist(parameters$factors), paste0('factor(', other_vars, ')'), other_vars)
        other_interactions <- vars_to_drop[!vars_to_drop %in% unlist(temp_drop_var)]
        other_interactions <- other_interactions[grepl('(\\*|\\:)', other_interactions)]
        other_cleaned_interactions <- c()
        if(!is.null(other_interactions)){
          for (interaction in other_interactions){
            temp_interaction <- c()
            terms <- unlist(stringr::str_split(interaction, pattern = '(\\*|\\:)'))
            interaction_pattern <- ifelse(grepl('\\*', interaction), '*', ':')
            for (term in terms){
              term <- ifelse(term %in% unlist(parameters$factors), paste0('factor(', term, ')'), term)
              temp_interaction <- c(temp_interaction, term)
            } # For Each Term in Interaction
            temp_interaction <- paste(temp_interaction, collapse = interaction_pattern)
            other_cleaned_interactions <- c(other_cleaned_interactions, temp_interaction)
          } #For Each Interaction - Separate + Add if factor() if Factor & Recombine
        } #If 'other_interactions' isn't empty
        other_vars <- unique(c(other_vars, other_cleaned_interactions)) #Combine All Vars Back Into Single Object

        if (length(other_vars) == 0) {
          message("\033[37m           Insufficient Variables in Group: \033[0m", current_var, '...Moving On') # Print Update
          next
        }

        outcome_variable <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', parameters$outcome, ')'), parameters$outcome) # Add factor() to outcome_var if Binomially Distributed

        temp_combination <- paste0(outcome_variable, '~', paste(other_vars, collapse = '+'))

        temp_combination <- data.frame(
          temp_combination = temp_combination,
          dropped_var = names(vars_to_drop[drop_var])) #Collapse Into Single Temp DF

        combinations <- dplyr::bind_rows(combinations, temp_combination) #Add to Combinations Frame

      }

    } #Create Combinations (If List Drop Vars)

  } # Create Combinations by parameters$list_drop_vars

  {
    for (combination in 1:nrow(combinations)){
      temp_combination_row <- combinations[combination,] #Get Temp Row
      temp_dropped_var <- temp_combination_row$dropped_var

      suppressWarnings(suppress_message({
        temp_drop_var_declared_model <- train(
          form = as.formula(temp_combination_row$temp_combination),
          data = parameters$train_set,
          metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
          method = as.character(parameters$model),
          trControl = parameters$train_control,
          localImp = TRUE
        )
      }))  #Re-Run Model w/ Omitted Variable

      if (parameters$save_drop_var_models == TRUE){
        omitting_variables_models_output[[as.character(temp_dropped_var)]] <- temp_drop_var_declared_model
      } # If save_drop_var_models == TRUE ~ Store Output to omitting_variables_model_output

      test_data <- parameters$test_set

      if (parameters$outcome_type == 'Binomial'){
        original_predictions <- predict(output$declared_model, test_data, na.action = na.pass)
        original_accuracy = length(which(original_predictions==test_data[[parameters$outcome]]))/length(original_predictions)
      } else {
        original_predictions <- predict(output$declared_model, test_data, na.action = na.pass)
        original_accuracy <- sqrt(mean((original_predictions - test_data[[parameters$outcome]])^2)) # If DV is Continuous...
      } # Get Original Predictions & Accuracy

      if (parameters$outcome_type == 'Binomial'){
        drop_var_predictions <- predict(temp_drop_var_declared_model, test_data, na.action = na.pass)
        drop_var_accuracy = length(which(drop_var_predictions==test_data[[parameters$outcome]]))/length(original_predictions)
      } else {
        drop_var_predictions <- predict(temp_drop_var_declared_model, test_data, na.action = na.pass)
        drop_var_accuracy <- sqrt(mean((drop_var_predictions - test_data[[parameters$outcome]])^2)) # If DV is Continuous...
      } # Get Drop Var Predictions & Accuracy


      change_temp <- data.frame(var = temp_dropped_var,
                                fit_change = (drop_var_accuracy - original_accuracy)) #Get Temp Frame for Fit Change

      fit_change <- dplyr::bind_rows(fit_change, change_temp) #Append to fit_change

      {

        test_data = data.frame(output$parameters$test_set, check.names = F) #Grab Test Data

        for (var in 1:ncol(test_data)){
          temp_column <- data.frame(test_data[,var])
          names(temp_column)[1] <- names(test_data[var])
          colon_check <- gsub('\\.', ':', names(test_data[var]))
          asterisk_check <- gsub('\\.', '*', names(test_data[var]))
          if (colon_check %in% unlist(parameters$interactions)){
            names(test_data)[var] <- colon_check
          }
          if (asterisk_check %in% unlist(parameters$interactions)){
            names(test_data)[var] = asterisk_check
          }
        } #Fix bootstrap Column Names

        outcome_variable = parameters[['outcome']] #Set Outcome Var

        predictions <- predict(temp_drop_var_declared_model, newdata = test_data) #Get Base Predictions from Dropped Var Model
        comparison_set <- parameters$test_set[outcome_variable][,1] #Set Real Data

        if (parameters[['outcome_type']] == 'Binomial'){
          accuracy <- mean(predictions == comparison_set) #Get Predictive Accuracy from Predictions v. Real Data if Binomial DV
        } else {
          accuracy <- sqrt(mean((predictions - comparison_set)^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
        }


        bootstrap_output <- list() #Create Empty List to Store Bootstrap Outputs

        for (i in 1:50) {
          bootstrap_indices <- sample(nrow(test_data), replace = TRUE) #Generate Bootstrap Sample
          bootstrap_test_data <- test_data[bootstrap_indices, ] #Subset Test Data by Sample Indeces
          bootstrap_test_data <- bootstrap_test_data[, !names(bootstrap_test_data) == parameters$outcome, drop = FALSE] #Remove DV
          bootstrap_predictions <- predict(output$declared_model, newdata = bootstrap_test_data) #Predict on bootstrap Sample

          if (parameters[['outcome_type']] == 'Binomial') {
            bootstrap_accuracy <- mean(as.numeric(as.character(bootstrap_predictions)) - as.numeric(as.character(predictions)))
          } else {
            bootstrap_accuracy <- mean(bootstrap_predictions - predictions)
          }



          bootstrap_output[[i]] <- list(bootstrap_data = bootstrap_test_data,
                                        bootstrap_accuracies = bootstrap_accuracy) #Append to List

        } #Compile Predictions from Bootstrapped Samples of Test Data




      } #Bootstrap

      bootstrap_drop_var[[as.character(temp_combination_row$dropped_var)]] <- bootstrap_output

      message("\033[37m           Completed Variable Omission For \033[0m", temp_combination_row$dropped_var) #Print Update

    }
  } #Assess Fit Change from Omitting Vars (If List Drop Vars)

  if (parameters$save_drop_var_models == TRUE){
    omitting_vars <- list(fit_change = fit_change,
                          bootstrap_drop_var = bootstrap_drop_var,
                          omitting_variables_models_output = omitting_variables_models_output)
  } else {
    omitting_vars <- list(fit_change = fit_change,
                          bootstrap_drop_var = bootstrap_drop_var)
  } # If save_drop_var_models == TRUE - Return Model Outputs (If FALSE - Don't)



  return(omitting_vars)

} #Omitting Variables
