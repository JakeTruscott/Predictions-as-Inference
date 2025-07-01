bootstrap_predictions_ci <- function(output, parameters){

  test_data = parameters$test_set #Grab Test Data
  outcome_variable = parameters[['outcome']] #Set Outcome Var

  original_predictions <- predict(output$declared_model, newdata = test_data) #Get Base Predictions
  comparison_set <- parameters$test_set[outcome_variable][,1] #Set Real Data

  if (parameters[['outcome_type']] == 'Binomial'){
    accuracy = length(which(original_predictions==test_data[[parameters$outcome]]))/length(original_predictions) #Get Predictive Accuracy from Predictions v. Real Data if Binomial DV
  } else {
    accuracy <- sqrt(mean((original_predictions - comparison_set)^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
  }


  bootstrap_output <- list() #Create Empty List to Store Bootstrap Outputs

  for (i in 1:100) {
    bootstrap_indices <- sample(nrow(test_data), replace = TRUE) #Generate Bootstrap Sample
    bootstrap_test_data <- test_data[bootstrap_indices, ] #Subset Test Data by Sample Indeces
    bootstrap_test_data <- bootstrap_test_data[, !names(bootstrap_test_data) == parameters$outcome, drop = FALSE] #Remove DV
    bootstrap_predictions <- predict(output$declared_model, newdata = bootstrap_test_data) #Predict on bootstrap Sample

    if (parameters[['outcome_type']] == 'Binomial'){
      actual_outcomes <- test_data[[parameters$outcome]][bootstrap_indices]
      bootstrap_accuracy <- length(which(bootstrap_predictions == actual_outcomes)) / length(actual_outcomes)

    } else {
      actual_outcomes <- test_data[[parameters$outcome]][bootstrap_indices]
      bootstrap_accuracy <- sqrt(mean((bootstrap_predictions - actual_outcomes)^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
    }

    bootstrap_output[[i]] <- list(bootstrap_data = bootstrap_test_data,
                                  bootstrap_accuracies = bootstrap_accuracy) #Append to List

  } #Compile Predictions from Bootstrapped Samples of Test Data

  return(bootstrap_output) #Return Summary



} #Bootstrap CIs
