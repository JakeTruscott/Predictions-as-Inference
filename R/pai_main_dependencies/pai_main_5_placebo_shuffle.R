placebo_shuffle <- function(declared_model, parameters, output){

  placebos <- data.frame() # Initialize Empty DF

  if(is.null(parameters$interactions)){
    variables <- c(unlist(parameters$predictors))
  } else {
    variables <- c(unlist(parameters$predictors), unlist(parameters$interactions))
  }  # Get Variables

  true_values <- parameters$test_set[parameters$outcome][,1] #Set Real Data
  test_data <- parameters$test_set

  if (parameters$outcome_type == 'Binomial'){
    original_predictions <- predict(output$declared_model, parameters$test_set, na.action = na.pass)
    original_accuracy = length(which(original_predictions==test_data[[parameters$outcome]]))/length(original_predictions)
  } else {
    original_predictions <- predict(output$declared_model, parameters$test_set, na.action = na.pass)
    original_accuracy <- sqrt(mean((original_predictions - test_data[[parameters$outcome]])^2)) # If DV is Continuous...
  }

  set.seed(parameters$seed)
  random_seeds <- runif(parameters$placebo_iterations, min = 100, max = 10000) # Create Vector of Seeds -- Will Draw from Same Pool via Seed Declaration

  for (rep in 1:as.numeric(parameters$placebo_iterations)){

    set.seed(random_seeds[rep]) # Select Seed to Draw From

    if (parameters$list_drop_vars == TRUE){

      variables <- parameters$drop_vars

      for (drop_group in 1:length(variables)){

        shuffle_data <- parameters$test_set #Get Test Data

        temp_drop_group <- unlist(unname(variables[drop_group]))

        for (var_name in temp_drop_group){
          shuffle_data[[var_name]] <- sample(shuffle_data[[var_name]])
        } #Shuffle Each of the Vars

        interaction_terms_in_original <- names(shuffle_data[grepl('(\\*|\\:)', names(shuffle_data))]) # Check if Interaction Terms in Original Data
        if (!length(interaction_terms_in_original) == 0){
          for (interaction in 1:length(interaction_terms_in_original)){
            temp_interaction <- c(stringr::str_split(interaction_terms_in_original[interaction], pattern = c('\\*', '\\:')))[[1]]
            shuffle_data[interaction_terms_in_original[interaction]] <- apply(shuffle_data[, temp_interaction], 1, prod)
          } # For Each Interaction Term - Reconfigure

        } #If Not Null Interactions

        shuffled_predictions <- predict(declared_model, newdata = shuffle_data, na.action = na.pass) # Predict using the shuffled data

        if (parameters[['outcome_type']] == 'Binomial'){

          shuffled_accuracy = length(which(shuffled_predictions==test_data[[parameters$outcome]]))/length(original_predictions)
          accuracy_change <- shuffled_accuracy - original_accuracy #Get Pred Accuracy Change if Binomial DV

        } else {
          shuffled_accuracy <- sqrt(mean((shuffled_predictions - test_data[[parameters$outcome]])^2)) # If DV is Continuous...
          accuracy_change <- shuffled_predictions - original_predictions #Get Root Mean Squared Error Change in Continuous DV
        }

        placebo_temp <- data.frame(rep_count = rep, variable = names(variables[drop_group]), shuffled_accuracy = shuffled_accuracy) # Store the accuracy change
        placebos <- dplyr::bind_rows(placebos, placebo_temp)


      } #If List Drop Vars = TRUE

    } else {

      for (var in variables){

        shuffle_data <- parameters$test_set #Get Test Data

        vars_to_shuffle <- c(var, unlist(stringr::str_split(var, pattern = '(\\*|\\:)'))) #Get All Vars (Including Interactions w/ Vars)
        vars_to_shuffle <- vars_to_shuffle[!grepl('(\\*|\\:)', vars_to_shuffle)] #Shuffle Stand-Alone Vars -- Recompile Interactions
        interaction_vars <- variables[grepl('(\\*|\\:)', variables)] #Grab Interaction Terms from variables vector

        for (var_name in vars_to_shuffle) {
          shuffle_data[[var_name]] <- sample(shuffle_data[[var_name]])
        } #Shuffle Any & All Vars Included in Shuffle

        if (!length(interaction_vars) == 0) {
          if (!is.null(interaction_vars)){
            for (interaction in 1:length(interaction_vars)){
              t <- interaction_vars[[interaction]][[1]]
              t <- stringr::str_split(t, pattern = '(\\:|\\*)')[[1]]
              shuffle_data[interaction_vars[interaction]] <- apply(shuffle_data[, t], 1, prod)
            }
          }
        } # For Interaction Vars, Reconfigure After Shuffling Stand-Alone Terms

        shuffled_predictions <- predict(output$declared_model, newdata = shuffle_data, na.action = na.pass) # Predict using the shuffled data

        if (parameters[['outcome_type']] == 'Binomial'){

          shuffled_accuracy = length(which(shuffled_predictions==test_data[[parameters$outcome]]))/length(shuffled_predictions)
          accuracy_change <- shuffled_accuracy - original_accuracy #Get Pred Accuracy Change if Binomial DV

        } else {
          shuffled_predictions <- sqrt(mean((shuffled_predictions - test_data[[parameters$outcome]])^2)) # If DV is Continuous...
          accuracy_change <- shuffled_predictions - original_predictions #Get Root Mean Squared Error Change in Continuous DV
        }


        placebo_temp <- data.frame(rep_count = rep, variable = names(variables[drop_group]), shuffled_accuracy = shuffled_accuracy) # Store the accuracy change
        placebos <- dplyr::bind_rows(placebos, placebo_temp) # Store



      } #For Var in Variables

    }  #If List Drop Vars = F

    if (rep %% 5 == 0) {
      message("\033[37m           Completed Placebo Shuffling Iteration \033[0m", rep) #Print Update

    }

  } #For Rep in Placebo Iterations

  placebos_all_returned <- placebos

  pred <- list()

  for (unique_var in unique(placebos$variable)){
    # Subset the data for the current variable
    temp_data <- placebos %>%
      filter(variable == unique_var) %>%
      select(shuffled_accuracy)

    pred[unique_var] <- temp_data

  }

  pred <- as.data.frame(pred)


  preds_list <- list(preds = pred,
                     base = original_accuracy,
                     pred.num = length(original_predictions),
                     means = apply(pred, 2, mean),
                     mins = apply(pred, 2, min),
                     upper = apply(pred, 2, function(x) quantile(x, 0.95)))

  placebos_summary <- data.frame()

  for (i in 1:length(preds_list$preds)){

    temp_group_name <- names(preds_list$preds)[i]
    temp_base <- preds_list$base[1]
    temp_mean <- preds_list$means[i]
    temp_min <- preds_list$mins[i]
    temp_max <- preds_list$upper[i]

    temp_combined <- data.frame(variable = temp_group_name,
                                var_id = i,
                                base = temp_base,
                                base_zero = 0,
                                mean = temp_mean,
                                lwr = temp_min,
                                upr = temp_max)

    placebos_summary <- bind_rows(placebos_summary, temp_combined)

  }


  placebos_final <- data.frame()

  for (i in 1:nrow(placebos_summary)){

    temp_row <- placebos_summary[i,]
    temp_row <- temp_row %>%
      mutate(variable = variable,
             var_id = var_id,
             base = base,
             mean = mean - base,
             lwr = lwr - base,
             upr = upr - base)

    placebos_final <- bind_rows(placebos_final, temp_row)

  }

  placebos_final <- placebos_final %>%
    rename(var = variable)
  placebos_all_returned <- placebos_all_returned %>%
    rename(var = variable)


  placebo <- list()
  placebo[['placebo_summary']] <- placebos_final
  placebo[['all_returned']] <- placebos_all_returned

  return(placebo)

} # Placebo Protocol
