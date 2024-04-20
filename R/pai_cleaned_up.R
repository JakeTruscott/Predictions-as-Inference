# To Do:
'
1) Combine variable declarations:
    - Combine Predictors, Factors, and Interactions
    - Allow "assign factors" - but default to NULL (if change to not null or TRUE, then use it)
    - Allow list_drop_vars
    - Allow drop_vars declaration
    - clean up ML declaration

'

require(caret)
require(dplyr)
require(stringr)
require(doParallel)

sandbox_data <- data.frame(
  var1 = sample(0:1, 100, replace = TRUE),
  var2 = sample(0:50, 100, replace = TRUE),
  var3 = c(sample(0:1, 99, replace = TRUE), 2),
  var4 = sample(0:50, 100, replace = TRUE),
  var5 = sample(0:1, 100, replace = TRUE),
  var6 = sample(0:50, 100, replace = TRUE)

)


test <- pai(data = sandbox_data,
            model = 'parRF',
            outcome = 'var1',
            predictors = NULL,
            interactions = c('var4*var5'),
            cores = 1)

#To Do: Add Diagnostic Stuff to Output

data = sandbox_data #Data
model = 'parRF'  #Caret Model
outcome = 'var1'  #DV
predictors = NULL  #IVs
interactions = NULL  #Interactive Terms
drop_vars = NULL  #Defaults to All
cores = NULL  #Defaults to 1
placebo_iterations = NULL  #Defaults to 10
folds = NULL  #Defaults to 5
train_split = 0.8  #Defaults to 80/20
custom_tc = FALSE  #Defaults to Basic TC (3 Repeats  Assigned K-Folds  etc.)
assign_factors = 3  #Defaults to 3 - Change to Any Number
list_drop_vars = FALSE  #Defaults to FALSE
seed = 1234


pai <- function(data, #Data
                model = NULL, #Caret Model
                outcome = NULL, #DV
                predictors = NULL, #IVs
                interactions = NULL, #Interactive Terms
                drop_vars = NULL, #Defaults to All
                cores = NULL, #Defaults to 1
                placebo_iterations = NULL, #Defaults to 10
                folds = NULL, #Defaults to 5
                train_split = 0.8, #Defaults to 80/20
                custom_tc = FALSE, #Defaults to Basic TC (3 Repeats, Assigned K-Folds, etc.)
                assign_factors = 3, #Defaults to 3 - Change to Any Number
                list_drop_vars = FALSE, #Defaults to FALSE
                seed = 1234 #Defaults to 1234
                ){


  start_time <- Sys.time() #Start Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m---------------------- Beginning PAI Process ----------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Start Message

  output <- list() #Create Empty List to Store Output, Params, etc.

  set.seed(seed) #Set Random Seed (Defaults to 1234)

  message("Initializing Parallel Environment with \033[37m", cores, " Core(s)") #Print Update

  parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed) #Compile Parameters from Input Declarations

  doParallel::registerDoParallel(as.numeric(parameters$cores)) #Register Parallel Environment

  #foreach(i = 1:cores, .export = c("declared_model", "dropping_vars", "pai_params_wrapper", "placebo_shuffle", "print_parameters", "push", "push_pred", "sparse_variable_check", "suppress_message", "parameters")) %dopar% {


    print_parameters(parameters) #Print Parameters

    message('Beginning ', parameters$model) #Start Message for Declared Model

    declared_model <- declared_model(parameters) #Return Declared ML Model
    output[['declared_model']] <- declared_model #Add to

    message('Beginning Placebo Iterations') #Start Message for Placebo Iterations

    placebo <- placebo_shuffle(declared_model, parameters) #Run Placebo Iterations
    output[['placebo']] <- placebo #Append to Output

    message('Beginning Variable Omissions') #Start Message for Placebo Iterations

    omitting_variables <- dropping_vars(parameters, output) #Run Omitting Vars
    output[['omitting_variables']] <- omitting_variables #Append to Output

    fit_change <- left_join(placebo, omitting_variables, by = 'var') #Create Fit Change Frame
    output[['fit_change']] <- fit_change #Append to Output

    message('Compiling Bootstrapped Confidence Intervals') #Start Message for Placebo Iterations

    bootstrap_cis <- bootstrap_predictions_ci(output, parameters) #Compile Bootstrapped CIs from Predictions
    output[['bootstrap_predictions_CI']] <- bootstrap_cis #Append to Output

    message('Beginning Push Protocol') #Start Message for Placebo Iterations

    pusher <- push(parameters, output) #Push Protocol
    output[['push']] <- pusher #Append to Output

    output[['parameters']] <- parameters #Append Parameters to Output

  #}

  end_time <- Sys.time() #End Time
  completion_time_minutes<- as.numeric((difftime(end_time, start_time, units = "secs")/60)) #Completion Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m-------------------------- PAI  Complete --------------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Completion Message
  message('\033[32mCompletion Time = ', round(completion_time_minutes,2), ' Minutes \033[0m') #Print Completion Time

  return(output) #Return Output Object

} #Predictions as Inference Main Function


pai_params_wrapper <- function(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed){

  {

    parameters <- list()

    if(is.null(data)){
      message("\033[31m Error: No Data Declared! \033[0m")
      stop()
    }

    if (is.null(model)){
      parameters[['model']] <- 'parRF'
    } else {
      parameters[['model']] <- model
    } #Declare Model from Caret

    if (is.null(outcome)){
      stop('No Outcome Variable Declared \n Try Again')
    } else {
      parameters[['outcome']] <- outcome
    } # Declare Outcome (DV)

    if (is.null(predictors)){
      parameters[['predictors']] <- c(names(data)[!names(data) %in% parameters[['outcome']]])
    } else {
      parameters[['predictors']] <- list(predictors)
    } # Declare Predictors

    if (!is.null(interactions)){
      parameters[['interactions']] <- c(interaction)
      } #Declare Interaction Terms

    if (is.null(drop_vars)){
      parameters[['drop_vars']] <- c(parameters[['predictors']], parameters[['interactions']])
    } else {
      parameters[['drop_vars']] <- c(drop_vars)
    } # Declare Variables to Drop

    if (is.null(cores)){
      parameters[['cores']] <- 1
    } else {
      parameters[['cores']] <- as.numeric(cores)
    } #Cores

    if (is.null(placebo_iterations)){
      parameters[['placebo_iterations']] <- 10
    } else {
      parameters[['placebo_iterations']] <- as.numeric(placebo_iterations)
    } #Placebo Iterations

    if (is.null(folds)){
      parameters[['folds']] <- 5
    } else {
      parameters[['folds']] <- as.numeric(folds)
    } #K-Folds

    if (is.null(train_split)){
      parameters[['train_split']] <- 80
    } else {
      parameters[['train_split']] <- as.numeric(train_split)
    } # Train/Test Split

    if (custom_tc == FALSE){
      parameters[['custom_tc']] <- 'FALSE'
    } else {
      parameters[['custom_tc']] <- 'TRUE'
    } #Custom Train Control

    if (is.null(assign_factors)){
      parameters[['assign_factors']] <- 4
    } else {
      parameters[['assign_factors']] <- as.numeric(assign_factors)
    } #Assign Factor Floor (Default to 4)


    if (list_drop_vars == FALSE){
      parameters[['list_drop_vars']] <- 'FALSE'
    } else {
      parameters[['list_drop_vars']] <- 'TRUE'
    } # Drop Vars Grouped in List of Objects

    if (seed == 1234){
      parameters[['seed']] <- 1234
    } else {
      parameters[['seed']] <- as.numeric(seed)
    } #Seed

  } #Parameter Declaration

  {

    {

      if (parameters$custom_tc == 'FALSE'){
        parameters[['train_control']] <- trainControl(method = 'repeatedcv',
                                                      number = 5,
                                                      repeats = 3,
                                                      savePredictions = TRUE,
                                                      verbose = F)
      } else {

        custom_params <- data.frame(custom_declare = strsplit(custom_tc, ', ')[[1]])

        tc <- list()

        tc_names <- names(trainControl())

        custom_params <- custom_params %>%
          mutate(param = gsub('\\=.*', '', custom_declare),
                 value = gsub('.*\\=', '', custom_declare)) %>%
          mutate(param = gsub('\\s+', '', param),
                 value = gsub('\\s+', '', value)) %>%
          select(param, value) %>%
          filter(param %in% tc_names)

        tc_params <- list()

        for (i in 1:nrow(custom_params)) {
          param_name <- custom_params$param[i]
          param_value <- eval(parse(text = custom_params$value[i]))
          tc_params[[param_name]] <- param_value
        }

        parameters[['train_control']] <- do.call(trainControl, tc_params)


      }


    } # Train Parameters - Custom (If Declared)

    {

      outcome_type <- data[parameters[['outcome']]]
      outcome_levels <- if(length(unique(outcome_type)) > 2){
        outcome_type = 'Continuous'
      } else {
        outcome_type = 'Binomial'
      }
      parameters[['outcome_type']] = outcome_type


    } #Assign DV Type (Binomial or Continuous)

  } # Other Params (Custom Train Control) + Assingn DV Type

  {

    {

      if (is.null(parameters$interactions)){
        combined_vars <- unique(parameters$predictors)
      } else {
        combined_vars <- unique(c(unique(unlist(stringr::str_split(parameters$interactions, pattern = "\\*|\\:"))), unique(parameters$predictors)))
      } #Get All Vars

      full_data <-  data %>%
        dplyr::select(parameters$outcome, any_of(unlist(combined_vars))) #Get Full Data

      if (!is.null(parameters$interactions)){
        for (interaction in interactive_terms){
          temp_term <- interaction
          vars <- strsplit(temp_term, "([*]|[:])")[[1]]
          full_data <- full_data %>%
            mutate(!!temp_term := !!as.symbol(vars[1]) * !!as.symbol(vars[2]))
        }
      }

      parameters['full_data'] <- list(full_data)

    } #Create 'full_data'

    {

      train_index <- createDataPartition(y = full_data[[parameters[['outcome']]]], p = 0.8, list = F) #Create Partition Index

      train_set <- full_data[train_index, ] #Split Train
      test_set <- full_data[-train_index, ] #Split Test

      parameters[['train_index']] <- train_index
      parameters[['train_set']] <- list(train_set)
      parameters[['test_set']] <- list(test_set)

    } # Test-Train Split

    {

      formula_vars <- c() #Create Empty Object for Formula Vars

      sparse_check <- sparse_variable_check(parameters) #Check Sparse Nature & Assign Factor

      non_factors <- unname(unlist(sparse_check['non-factors'])) #Get Non-Factors
      factors <- unname(unlist(sparse_check['factors'])) #Get Factors
      dv <- unname(sparse_check['outcome'][1]) #Get DV
      sparse_factors <- unname(sparse_check['sparse_factors']) #Get Sparse Factors
      factors <- factors[!factors %in% dv] #Remove DV

      parameters['non_factors'] <- list(non_factors) #Put Non-Factors in parameters
      parameters['factors'] <- list(factors) #Same for factors
      parameters['sparse_factors'] <- list(sparse_factors)

      factors <- paste0('factor(', factors, ')') #Add 'as.factor' to factors

      formula_vars <- c(non_factors, factors) #Create Single Formula Vars (No Interactions Yet...)

      {
        if (is.null(parameters$interactions)) {
          formula_vars <- formula_vars #If No Interactions, Skip
        } else {

          formula_interactions <- c() #Create Empty Object to Store Formula Interaction Terms (IF Declared)

          interactions <- unlist(parameters$interactions) #Get Interactive Terms

          for (i in 1:length(interactions)){

            temp_interaction <- interactions[i]
            temp_interaction_vars <- unlist(stringr::str_split(temp_interaction, pattern = "[*|:]"))

            if (any(temp_interaction_vars %in% sparse_check$sparse_factors)) {
              sparse_check$sparse_factors <- c(sparse_check$sparse_factors, temp_interaction)
              next
            } #Check if Interaction Var is Sparse (If Yes, Toss and Move oN)

            temp_interaction_checked <- c()

            for (interaction_var in temp_interaction_vars){

              if (interaction_var %in% sparse_check$factors){
                checked_interaction_var <- paste0('factor(', interaction_var, ')')
                temp_interaction_checked <- c(temp_interaction_checked, checked_interaction_var)
              } else {
                temp_interaction_checked <- c(temp_interaction_checked, interaction_var)
              }

            }

            temp_interaction_checked <- paste(temp_interaction_checked, collapse = ":")

            formula_interactions <- c(formula_interactions, temp_interaction_checked)

          }

          if(is.null(formula_interactions)){
            formula_vars <- formula_vars
          } else {
            formula_vars <- c(formula_vars, formula_interactions)
          }


        } #Check if Interactions Are Sparse or Factor

      } #Checking if Interactions Contain Sparse Factors...

      if (!is.null(sparse_check$sparse_factors)) {
        parameters$sparse_factors <- sparse_check$sparse_factors
      } #Update Sparse Factors (If Needed)


      parameters[['predictors']] <- parameters$predictors[!parameters$predictors %in% unlist(parameters$sparse_factors)] #Toss Sparse Predicors
      parameters[['interactions']] <- parameters$interactions[!parameters$interactions %in% unlist(parameters$sparse_factors)] #Toss Interactions w/ Sparse Variables

      parameters[['drop_vars']] <- parameters$drop_vars[!parameters$drop_vars %in% unlist(parameters$sparse_factors)] #Toss Sparse Factors from Drop Vars


    } # Get Formula Vars

    {

      dv <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', dv, ')'), dv) #Get DV

      parameters[['base_formula']] <- paste0(dv, '~', paste(formula_vars, collapse = "+")) #Create Formula


    } #Create Formula (+ Message for What Was Tossed b/c Sparse)



  } #Create Data, Test/Train Split & Formula -- Remove Sparse Vars

  return(parameters)

}

sparse_variable_check <- function(parameters){

  data = parameters$full_data
  dv = parameters$outcome
  variables = unlist(parameters$predictors)
  test_data = data.frame(parameters$test_set)
  train_data = data.frame(parameters$train_set)
  factor_level_min = parameters$assign_factors

  factor_variables <- c() #Initialize Empty Object for Factor Vars

  for (var in c(variables, dv)){
    temp_variable <- c(data[[var]])
    temp_levels <- as.numeric(length(unique(temp_variable)))

    if (temp_levels <= as.numeric(factor_level_min)){
      factor_variables <- c(factor_variables, var)
    } else {
      next
    }

  } #Check If Factor (4 or Less Unique Values)

  sparse_factors <- c()

  for (factor in factor_variables){

    levels_test <- test_data[[factor]] #Grab Variable from Test
    levels_test <- sort(unique(levels_test)) #Get Unique Vars
    levels_train <- train_data[[factor]] #Same for Train
    levels_train <- sort(unique(levels_train))

    if (all(levels_test %in% levels_train) && all(levels_train %in% levels_test)){
      next
    } else {
      sparse_factors <- c(sparse_factors, factor)
    }

  } #Check if any factors are Sparse

  remaining_factors <- factor_variables[!factor_variables %in% c(sparse_factors)]

  sparse_check <- list()
  sparse_check[['outcome']] <- dv
  sparse_check[['non-factors']] <- c(variables[!variables %in% factor_variables])
  sparse_check[['factors']] <- c(remaining_factors)
  if (is.null(sparse_factors)){
    sparse_check[['sparse_factors']] <- NULL
  } else {
    sparse_check['sparse_factors'] <- c(sparse_factors)
  }

  return(sparse_check) #Return List w/ Factors & Sparse Factors

} #Check Sparse Factors & Assign as.factor() to Factors for Formula

print_parameters <- function(parameters){

  message("\033[32m Model: \033[0m", "\033[33m", parameters$model, "\033[0m \n",
          "\033[32m Outcome Variable: \033[0m", "\033[33m", parameters$outcome , "\033[0m \n",
          "\033[32m Predictors: \033[0m", "\033[33m " , ifelse(length(unique(parameters$predictors) < 10), paste(parameters$predictors, collapse = " "), length(unique(parameters$predictors))), " \033[0m \n",
          "\033[32m Interaction(s): \033[0m", "\033[33m", ifelse(is.null(parameters[['interactions']]), "None", paste(paste0('(', unlist(parameters$interactions), ')'), collapse = " ")), "\033[0m")

  if (parameters$list_drop_vars == 'FALSE'){
    message("\033[32m Variables to Drop: \033[0m", "\033[33m", ifelse(all(parameters$predictors %in% parameters$drop_vars), "All Predictors", unlist(parameters$drop_vars)), " \033[0m")
  } else {
    message("\033[32m Variables to Drop: ", paste(paste0('(', unlist(parameters$list_drop_vars), ')'), collapse = " ") ," \033[0m")
  }

    message("\033[32m Train/Test Split: \033[0m", "\033[33m", (parameters$train_split)*100, "/", 100-((parameters$train_split)*100), "\033[0m \n",
    "\033[32m Vars Dropped Due to Sparse Variance in Train/Test: \033[0m", "\033[31m", ifelse(is.null(parameters$sparse_factors), 'None', paste(unlist(paste0('(', parameters$sparse_factors, ')')), collapse = " ")), "\033[0m \n")


} #Print Parameters

declared_model <- function(parameters){

  declared_model <- train(form = as.formula(parameters$base_formula),
                    data = data.frame(parameters$train_set),
                    metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
                    method = as.character(parameters$model),
                    trControl = parameters$train_control,
                    localImp = TRUE)

  return(declared_model)

} #Run Declared ML Model

placebo_shuffle <- function(declared_model, parameters){

  placebos <- data.frame() # Initialize Empty DF

  if(is.null(parameters$interactions)){
    variables <- c(unlist(parameters$predictors))
  } else {
    variables <- c(unlist(parameters$predictors), unlist(parameters$interactions))
  }  # Get Variables

  original_predictions <- predict(declared_model, data.frame(parameters$test_set), na.action = na.pass)

  for (rep in 1:as.numeric(parameters$placebo_iterations)){

    for (var in variables){

      shuffle_data <- data.frame(parameters$test_set) #Get Test Data
      shuffle_data[[var]] <- sample(shuffle_data[[var]]) # Shuffle the variable
      shuffled_predictions <- predict(declared_model, newdata = shuffle_data, na.action = na.pass) # Predict using the shuffled data
      accuracy_change <- mean(original_predictions != shuffled_predictions) # Calculate accuracy change
      placebo_temp <- data.frame(rep_count = rep, variable = var, accuracy_change = accuracy_change) # Store the accuracy change
      placebos <- bind_rows(placebos, placebo_temp)

    } #For Var in Variables

    if (rep %% 5 == 0) {
      message("\033[37m           Completed Placebo Shuffling Iteration \033[0m", rep) #Print Update

    }

  } #For Rep in Placebo Iterations

  placebo <- placebos %>%
    select(-rep_count) %>%
    rename(var = variable) %>%
    group_by(var) %>%
    summarize(mean_change = mean(accuracy_change),
              sd_change = sd(accuracy_change),
              lower_bound = mean_change - qt(0.975, n() - 1) * (sd_change / sqrt(n())),
              upper_bound = mean_change + qt(0.975, n() - 1) * (sd_change / sqrt(n())))

  return(placebo)

} # Placebo Protocol

# Problem with Placebo Shuffle -- Needs to Shuffle Interactions x Base Terms if One or Other is Declared...

dropping_vars <- function(parameters, output){

  fit_change <- data.frame() #Initialize Empty DF to Store Fit Changes from Dropping Vars

  vars_to_drop <- c() #Initialize Empty Object for Vars to Drop (Based on Params Declaration)

  if (parameters$list_drop_vars == 'FALSE'){
    vars_to_drop <- unlist(parameters$drop_vars)
  } #Indicate Source of Drop Vars (List Objects or Declared/Assigned Vars)

  {
    combinations <- data.frame() # Create Empty DF for Combinations of Drop Vars

    for (drop_var in 1:length(vars_to_drop)){
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

      outcome_variable <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', parameters$outcome, ')'), parameters$outcome) # Add factor() to outcome_var if Binomially Distributed

      temp_combination <- paste0(outcome_variable, '~', paste(other_vars, collapse = '+'))

      temp_combination <- data.frame(
        temp_combination = temp_combination,
        dropped_var = temp_drop_var) #Collapse Into Single Temp DF

      combinations <- bind_rows(combinations, temp_combination) #Add to Combinations Frame

    }


  } # Create Combinations

  {

    for (combination in 1:nrow(combinations)){
      temp_combination_row <- combinations[combination,] #Get Temp Row
      temp_dropped_var <- temp_combination_row$dropped_var

      suppressWarnings(suppress_message({
        temp_drop_var_declared_model <- train(
          form = as.formula(temp_combination$temp_combination),
          data = data.frame(parameters$train_set),
          metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
          method = as.character(parameters$model),
          trControl = parameters$train_control,
          localImp = TRUE
        )
      }))  #Re-Run Model w/ Omitted Variable

      if (parameters$outcome_type == 'Continuous'){
        fit_drop_var <- mean(temp_drop_var_declared_model$results$RMSE)
        fit_original <- mean(output$declared_model$results$RMSE)
      } else {
        fit_drop_var <- mean(temp_drop_var_declared_model$results$Accuracy)
        fit_original <- mean(output$declared_model$results$Accuracy)
      } # Get Fit -- Exception by Data Type

      change_temp <- data.frame(var = temp_dropped_var,
                                fit_change = (fit_original - fit_drop_var)) #Get Temp Frame for Fit Change

      fit_change <- bind_rows(fit_change, change_temp) #Append to fit_change

      message("\033[37m           Completed Variable Omission For \033[0m", temp_combination_row$dropped_var) #Print Update

    }

  } # Assess Fit Change from Omitting Vars

  return(fit_change)

} #Omitting Variables

push <- function(parameters, output){

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
    data = parameters$full_data #Get Data (Test Data)
    temp_var_distribution = unique(data[,temp_var]) #Get Distribution of Temp Var

    if (length(temp_var_distribution) <= 4){
      is_factor = TRUE #If factor TRUE
      steps = unique(temp_var_distribution)
    } else {
      is_factor = FALSE #If NOT Factor (FALSE)
      sd_var <- sd(data[,temp_var]) #Get Standard Deviation
      steps <- seq(-2*sd_var, 2*sd_var, (4*sd_var)/100) #Calculate Steps (+/- 2 Sds)
    } #If Factor (Steps = Levels); Else (Steps = +/- 2 Sds)


    var_push_predictions <- data.frame() #Initialize Empty DF for Push Predictions

    for (step in 1:length(steps)){

      temp_step = steps[step]

      temp_pred <- push_pred(mod = output$declared_model,
                             var = ifelse(is_factor == TRUE, factor(temp_var), temp_var),
                             stepper = temp_step,
                             Z = data,
                             outcome_type = parameters$outcome_type) #Calculate for Step[step]

      temp_pred <- data.frame(
        step = temp_step,
        onecount = temp_pred[1],
        acc = temp_pred[2]) #Put Output in Temp DF

      var_push_predictions <- bind_rows(var_push_predictions, temp_pred)

    }

    push_output[[temp_var]] <- var_push_predictions

    message("\033[37m           Completed Push Protocol For \033[0m", temp_var) #Print Update

  } #For Variable in Variables



} # Push + push_pred from Stepper

push_pred <- function(mod, var, stepper, Z, outcome_type){

  if (outcome_type == 'Continuous'){
    Z[[var]] <- Z[[var]] + stepper
    pred <- predict(mod, Z)
    true <- Z$y
    dif <- pred - true
    return(dif)
  } else {
    Z[[var]] <- Z[[var]] + stepper
    pred <- predict(mod, Z)
    true <- Z[,parameters[['outcome']]]
    onecount <- length(which(pred=='1'))/length(true)
    acc <- length(which(pred==true))/length(true)
    return(c(onecount, acc))
  }



} #Predictive Accuracy from Steps

suppress_message <- function(expr){
  sink(tempfile())
  result <- tryCatch(expr, finally = {
    sink()
    file.remove(tempfile())
  })
  invisible(result)
} #Special Function to Suppress Messages from Caret (R)

bootstrap_predictions_ci <- function(output, parameters){

  test_data = data.frame(parameters$test_set) #Grab Test Data
  outcome_variable = parameters[['outcome']] #Set Outcome Var

  predictions <- predict(output$declared_model, newdata = test_data) #Get Base Predictions
  comparison_set <- data.frame(parameters$test_set)[outcome_variable][,1] #Set Real Data
  accuracy <- mean(predictions == comparison_set) #Get Predictive Accuracy from Predictions v. Real Data

  bootstrap_accuracies <- numeric() #Create Empty List to Store Bootstrap Accuracies

  for (i in 1:100) {
    bootstrap_indices <- sample(nrow(test_data), replace = TRUE) #Generate Bootstrap Sample
    bootstrap_test_data <- test_data[bootstrap_indices, ] #Subset Test Data by Sample Indeces
    bootstrap_test_data <- bootstrap_test_data[, !names(bootstrap_test_data) == parameters$outcome, drop = FALSE] #Remove DV
    bootstrap_predictions <- predict(output$declared_model, newdata = bootstrap_test_data) #Predict on Boostrap Sample
    bootstrap_accuracy <- mean(bootstrap_predictions == test_data[parameters[['outcome']]][bootstrap_indices,]) #Calculate Accuracy
    bootstrap_accuracies[i] <- bootstrap_accuracy
  } #Compile Predictions from Bootstrapped Samples of Test Data

  bootstrap_summary <- data.frame(
    median = median(bootstrap_accuracies), #Median
    mean = mean(bootstrap_accuracies), #Mean
    conf_lower = as.numeric(quantile(bootstrap_accuracies, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[1]), #2.5%
    conf_higher = as.numeric(quantile(bootstrap_accuracies, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[2]), #97.5
    max = max(bootstrap_accuracies), #Max
    min = min(bootstrap_accuracies), #Min,
    boostraps = length(bootstrap_accuracies) #Count
  ) #Compile Into Single DF

  return(bootstrap_summary) #Return Summary



}


