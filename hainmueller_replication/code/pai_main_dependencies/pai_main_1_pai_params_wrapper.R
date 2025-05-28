pai_params_wrapper <- function(data, model, factors, outcome, predictors, drop_vars, save_drop_var_models, cores, placebo_iterations, folds, train_split, drop_sparse_vars, sparse_variable_threshold, custom_tc, outcome_type, assign_factors, list_drop_vars, seed){

  parameters <- list()

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
      parameters[['predictors']] <- c(predictors)
    } # Declare Predictors

    if (is.null(drop_vars)){
      parameters[['drop_vars']] <- c(parameters[['predictors']])
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

    if (is.null(factors)){
      parameters[['factors']] <- NULL
    } else {
      parameters[['factors']] <- c(factors)
    } # Assign Initial Factors

    if (is.null(assign_factors)){
      parameters[['assign_factors']] <- NULL
    } else {
      parameters[['assign_factors']] <- as.numeric(assign_factors)
    } #Assign Factor Floor (Default to 4)

    if(drop_sparse_vars == FALSE){
      parameters[['drop_sparse_vars']] <- FALSE
    } else {
      parameters[['drop_sparse_vars']] <- TRUE
    }

    parameters[['save_drop_var_models']] <- save_drop_var_models # Save Drop Var Models (True/False -- Defaults to FALSE)

    if (is.null(sparse_variable_threshold)){
      parameters[['sparse_variable_threshold']] <- NULL
    } else {
      parameters[['sparse_variable_threshold']] <- sparse_variable_threshold
    }

    parameters[['list_drop_vars']] <- list_drop_vars

    if (list_drop_vars == FALSE){
      parameters[['drop_vars']] <- c(parameters$drop_vars)
    } else {
      parameters[['drop_vars']] <- drop_vars
    } #Drop Vars - List vs. Identified

    if (is.null(seed)){
      parameters[['seed']] <- 1234
    } else {
      parameters[['seed']] <- as.numeric(seed)
    } #Seed

  } #Parameter Declaration

  {

    {

      if (parameters$custom_tc == 'FALSE'){
        parameters[['train_control']] <- trainControl(method = 'repeatedcv',
                                                      number = 10,
                                                      repeats = 3,
                                                      savePredictions = TRUE,
                                                      verbose = F)
      } else {

        parameters[['train_control']] <- custom_tc

      }


    } # Train Parameters - Custom (If Declared)

    {

      if (is.null(outcome_type)){
        temp_outcome_type <- unlist(c(data[parameters[['outcome']]]))
        outcome_levels <- if(length(unique(temp_outcome_type)) > 5){
          temp_outcome_type = 'Continuous'
        } else {
          temp_outcome_type = 'Binomial'
        }
      } else {
        temp_outcome_type = ifelse(outcome_type %in% c('Binomial', 'Dichotomous', 'Categorical', 'binomial', 'dichotomous', 'categorical'), 'Binomial', 'Continuous')
      }


      parameters[['outcome_type']] = temp_outcome_type


    } #Assign DV Type (Binomial or Continuous)

  } # Other Params (Custom Train Control) + Assign DV Type

  {

    full_data <-  data

  } # Create Full Data

  {

    factors <- parameters[['factors']]

    if (!is.null(assign_factors)){

      for (i in ncol(full_data)){

        temp_column <- full_data[,i]
        temp_column_name <- names(full_data[i])
        unique_values <- length(unique(temp_column))
        if (unique_values <= assign_factors){
          factors <- c(factors, temp_column_name)
        }
      }


    }

  } # Assign Factors

  {

    if (is.null(parameters$sparse_variable_threshold)){
      full_data <- full_data

    } else {

      vars_to_check <- c(factors, names(full_data)[sapply(full_data, function(x) is.character(x) || is.factor(x))])

      sparse_values_check <- function(data){
        sparse_values_list <- list()

        for (var in vars_to_check){

          freq_table <- table(data[[var]])
          rare_values <- names(freq_table[freq_table < sparse_variable_threshold])

          if (length(rare_values) > 0){
            sparse_values_list[[var]] <- rare_values
          }

        }

        return(sparse_values_list)

      } # Function to Check Sparse Factor Variables & Fix
      rare_values <- sparse_values_check(full_data) # Plug Full Data
      for (var in names(full_data)) {

        if (!var %in% names(rare_values)) {
          next
        }

        values_to_amend <- rare_values[[var]]

        if (is.factor(full_data[[var]])) {

          full_data[[var]] <- as.character(full_data[[var]])  # Convert factor to character

          full_data[[var]][full_data[[var]] %in% values_to_amend] <- 888  # Replace values

          full_data[[var]] <- factor(full_data[[var]])  # Convert back to factor
        } else {

          full_data[[var]][full_data[[var]] %in% values_to_amend] <- 888 # Replace values in character or numeric vectors
        }
      } # Fix Sparse Factors -- Replace with '888'

    }



  } # Sparse Variable Threshold

  {

    parameters['full_data'] <- list(full_data)

  } # Create Full Data

  {

    train_index <- createDataPartition(y = full_data[[parameters[['outcome']]]], p = 0.8, list = FALSE)[, 1]

    train_set <- full_data[train_index, ] #Split Train
    test_set <- full_data[-train_index, ] #Split Test
    parameters[['train_index']] <- train_index
    parameters[['train_set']] <- list(train_set)
    parameters[['test_set']] <- list(test_set)

  } # Test-Train Split

  {

    formula_vars <- c() #Create Empty Object for Formula Vars

    sparse_check <- sparse_variable_check(parameters) #Check Sparse Nature & Assign Factor

    if (!is.null(sparse_check$sparse_factors)) {
      parameters$sparse_factors <- sparse_check$sparse_factors
    } #Update Sparse Factors (If Needed)

    non_factors <- unname(unlist(sparse_check['non-factors'])) #Get Non-Factors

    if (!is.null(sparse_check['factors'])){
      factors <- c(factors, unname(unlist(sparse_check['factors'])))
    }

    dv <- unlist(unname(sparse_check['outcome'][1])) #Get DV
    sparse_factors <- unlist(unname(sparse_check['sparse_factors'])) #Get Sparse Factors
    factors <- factors[!factors %in% dv] #Remove DV
    if (parameters$drop_sparse_vars == TRUE){
      factors <- factors[!factors %in% unlist(sparse_factors)]
    }
    non_factors <- non_factors[!non_factors %in% c(unlist(sparse_factors), factors)]

    parameters[['non_factors']] <- non_factors #Put Non-Factors in parameters
    parameters[['factors']] <- factors #Same for factors
    parameters[['sparse_factors']] <- sparse_factors

    if (!is.null(parameters[['factors']])){

      for (factor_term in 1:length(parameters[['factors']])){
        temp_factor_term <- parameters[['factors']][factor_term]

        train_set[[as.character(temp_factor_term)]] <- factor(train_set[[as.character(temp_factor_term)]])
        test_set[[as.character(temp_factor_term)]] <- factor(test_set[[as.character(temp_factor_term)]])
        full_data[[as.character(temp_factor_term)]] <- factor(full_data[[as.character(temp_factor_term)]])

      }

    } # Convert Factors to Factor in Data

    if (!is.null(parameters[['sparse_factors']])){

      for (factor_term in 1:length(parameters[['sparse_factors']])){
        temp_factor_term <- parameters[['sparse_factors']][factor_term]

        train_set[[as.character(temp_factor_term)]] <- factor(train_set[[as.character(temp_factor_term)]])
        test_set[[as.character(temp_factor_term)]] <- factor(test_set[[as.character(temp_factor_term)]])
        full_data[[as.character(temp_factor_term)]] <- factor(full_data[[as.character(temp_factor_term)]])

      }

    } # Convert Sparse Factors to Factor in Data


    formula_vars <- c(non_factors, factors) #Create Single Formula Vars (No Interactions Yet...)
    formula_vars <- formula_vars[!formula_vars %in% dv]
    formula_vars <- unique(formula_vars)

  } # Assign Formula Vars

  {

    if (parameters[['list_drop_vars']] == TRUE){

      for (list in 1:length(parameters[['drop_vars']])){
        temp_list <- parameters[['drop_vars']][list]
        temp_list_name <- names(parameters[['drop_vars']][list])
        temp_list <- unlist(temp_list)[!unlist(temp_list) %in% parameters$sparse_factors]
        parameters[['drop_vars']][[as.character(temp_list_name)]] <- c(unname(temp_list))
      }

    } else {

      parameters[['drop_vars']] <- parameters$drop_vars[!parameters$drop_vars %in% unlist(parameters$sparse_factors)] #Toss Sparse Factors from Drop Vars

    }


  } # Assign Drop Vars

  {

    test_set <- data.frame(parameters$test_set, check.names = F)
    train_set <- data.frame(parameters$train_set, check.names = F)
    full_data <- data.frame(parameters$full_data, check.names = F)

    if (parameters$drop_sparse_vars == TRUE){
      test_set <- test_set[!names(test_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
      train_set <- train_set[!names(train_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
      full_data <- full_data[!names(full_data) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars

    }
    parameters[['test_set']] <- test_set #Resend to Params
    parameters[['train_set']] <- train_set #Resend to Params
    parameters[['full_data']] <- full_data #Resend to Params

  } # Create Train/Test sets

  {

    dv <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', outcome, ')'), outcome) #Get DV

    parameters[['base_formula']] <- paste0(dv, '~', paste(formula_vars[!is.na(formula_vars)], collapse = "+")) #Create Formula


  } #Create Formula (+ Message for What Was Tossed b/c Sparse)

  return(parameters)

}
