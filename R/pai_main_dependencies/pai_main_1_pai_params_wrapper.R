pai_params_wrapper <- function(data, model, factors, outcome, predictors, drop_vars, save_drop_var_models, cores, placebo_iterations, folds, train_split, drop_sparse_vars, sparse_variable_threshold, custom_tc, outcome_type, assign_factors, list_drop_vars, seed){

  {

    parameters <- list()
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # --- Data Check ---
    if (is.null(data)) {
      message("\033[31mError: No Data Declared!\033[0m")
      stop()
    }

    # --- Model ---
    parameters[['model']] <- if (is.null(model)) 'parRF' else model

    # --- Outcome Variable ---
    if (is.null(outcome)) stop('No Outcome Variable Declared\nTry Again')
    parameters[['outcome']] <- outcome

    # --- Predictors ---
    parameters[['predictors']] <- if (is.null(predictors)) {
      setdiff(names(data), outcome)
    } else {
      predictors
    }

    # --- Drop Variables ---
    parameters[['drop_vars']] <- if (is.null(drop_vars)) {
      parameters[['predictors']]
    } else {
      drop_vars
    }

    # --- Cores ---
    parameters[['cores']] <- if (is.null(cores)) 1 else as.numeric(cores)

    # --- Placebo Iterations ---
    parameters[['placebo_iterations']] <- if (is.null(placebo_iterations)) 10 else as.numeric(placebo_iterations)

    # --- K-Folds ---
    parameters[['folds']] <- if (is.null(folds)) 5 else as.numeric(folds)

    # --- Train/Test Split ---
    parameters[['train_split']] <- if (is.null(train_split)) 80 else as.numeric(train_split)

    # --- Custom Train Control ---
    parameters[['custom_tc']] <- as.character(custom_tc %||% FALSE)

    # --- Factor Variables ---
    parameters[['factors']] <- if (is.null(factors)) NULL else factors
    parameters[['assign_factors']] <- ifelse(assign_factors == TRUE, TRUE, FALSE)

    # --- Sparse Variable Handling ---
    parameters[['drop_sparse_vars']] <- isTRUE(drop_sparse_vars)
    parameters[['save_drop_var_models']] <- isTRUE(save_drop_var_models)
    parameters[['sparse_variable_threshold']] <- sparse_variable_threshold %||% NULL

    # --- Drop Vars List Option ---
    parameters[['list_drop_vars']] <- isTRUE(list_drop_vars)
    if (isTRUE(list_drop_vars)) {
      parameters[['drop_vars']] <- drop_vars
    }

    # --- Seed ---
    parameters[['seed']] <- if (is.null(seed)) 1234 else as.numeric(seed)

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

    full_data_vars <- unlist(Filter(function(x) !is.null(x) && !all(is.na(x)),
                                    list(parameters$outcome, parameters$factors, parameters$predictors)))
    parameters[['full_data']] <- data[, names(data) %in% full_data_vars]


  } # Create Full Data

  {

    if (assign_factors == TRUE){

      temp_additional_factors <-

      for (i in 1:ncol(parameters[['full_data']])){

        temp_column <- parameters[['full_data']][,i]
        temp_column_name <- names(parameters[['full_data']][i])
        if (is.factor(temp_column) || is.integer(temp_column)) {
          parameters[['factors']] <- c(parameters[['factors']], temp_column_name)
        }
      }

      parameters[['factors']] <- unique(parameters[['factors']])


    }


  } # Assign Factors

  {

    if (is.null(parameters$sparse_variable_threshold)){
      parameters[['full_data']] <- parameters[['full_data']]

    } else {

      vars_to_check <- c(
        parameters[['factors']],
        names(parameters[['full_data']])[sapply(parameters[['full_data']], function(x) is.character(x) || is.factor(x))]
      )

      vars_to_check <- setdiff(vars_to_check, parameters[['outcome']]) # Remove DV

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
      rare_values <- sparse_values_check(parameters[['full_data']]) # Plug Full Data
      for (var in names(parameters[['full_data']])) {

        if (!var %in% names(rare_values)) {
          next
        }

        values_to_amend <- rare_values[[var]]

        if (is.factor(parameters[['full_data']][[var]])) {

          parameters[['full_data']][[var]] <- as.character(parameters[['full_data']][[var]])  # Convert factor to character

          parameters[['full_data']][[var]][parameters[['full_data']][[var]] %in% values_to_amend] <- 888  # Replace values

          parameters[['full_data']][[var]] <- factor(parameters[['full_data']][[var]])  # Convert back to factor
        } else {

          parameters[['full_data']][[var]][parameters[['full_data']][[var]] %in% values_to_amend] <- 888 # Replace values in character or numeric vectors
        }
      } # Fix Sparse Factors -- Replace with '888'

    }



  } # Sparse Variable Threshold

  {

    if (!is.null(parameters[['factors']])){
      parameters[['full_data']] <- parameters[['full_data']] %>%
        mutate(across(any_of(parameters[['factors']]), as.factor)) # Convert Factors to Factor in full_data
    }


  } # Convert Factors to Factor in Full Data --> Then Train/test Split

  {

    train_index <- createDataPartition(y = parameters[['full_data']][[parameters[['outcome']]]], p = 0.8, list = FALSE)[, 1]

    train_set <- parameters[['full_data']][train_index, ] #Split Train
    test_set <- parameters[['full_data']][-train_index, ] #Split Test
    parameters[['train_index']] <- train_index
    parameters[['train_set']] <- list(train_set)
    parameters[['test_set']] <- list(test_set)

  } # Test-Train Split

  {

    formula_vars <- c() #Create Empty Object for Formula Vars

    sparse_check <- sparse_variable_check(parameters) #Check Sparse Nature & Assign Factor

    if (!is.null(sparse_check$sparse_factors)) {
      parameters[['sparse_factors']] <- sparse_check$sparse_factors
    } #Update Sparse Factors (If Needed)

    parameters[['non_factors']] <- unname(unlist(sparse_check['non-factors'])) #Get Non-Factors

    if (!is.null(sparse_check['factors'])){
      parameters[['sparse_factors']] <- c(parameters[['sparse_factors']], unname(unlist(sparse_check['factors'])))
      parameters[['sparse_factors']] <- unique(parameters[['sparse_factors']])
      parameters[['sparse_factors']] <- setdiff(parameters[['sparse_factors']], parameters[['outcome']])
      if (length(parameters[['sparse_factors']]) == 0){
        parameters[['sparse_factors']] <- NULL
      }
    }

    dv <- unlist(unname(sparse_check['outcome'][1])) #Get DV
    sparse_factors <- unlist(unname(sparse_check['sparse_factors'])) #Get Sparse Factors
    if (parameters$drop_sparse_vars == TRUE){
      parameters[['sparse_factors']] <- parameters[['sparse_factors']][!parameters[['sparse_factors']]%in% unlist(sparse_factors)]
    }
    parameters[['non_factors']] <- parameters[['non_factors']][!parameters[['non_factors']] %in% c(parameters$sparse_factors, parameters[['factors']])]


    formula_vars <- c(parameters[['factors']], parameters[['non_factors']]) #Create Single Formula Vars
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

      parameters[['drop_vars']] <- parameters$drop_vars[
        !parameters$drop_vars %in% c(unlist(parameters$sparse_factors), parameters$outcome)
      ]
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

    parameters[['base_formula']] <- paste0(outcome, '~', paste(formula_vars[!is.na(formula_vars)], collapse = "+")) #Create Formula


  } #Create Formula (+ Message for What Was Tossed b/c Sparse)

  {

    parameters[['train_test_variable_lengths']] <- list()
    variable_lengths <- data.frame()

    for (i in 1:ncol(parameters$train_set)){
      temp_column <- parameters$train_set[,i]
      temp_length <- length(unique(temp_column))
      temp_length <- data.frame(variable = names(parameters$train_set)[i],
                                length = temp_length,
                                source = 'train_set')
      variable_lengths <- bind_rows(variable_lengths, temp_length)
    }

    for (i in 1:ncol(parameters$test_set)){
      temp_column <- parameters$test_set[,i]
      temp_length <- length(unique(temp_column))
      temp_length <- data.frame(variable = names(parameters$test_set)[i],
                                length = temp_length,
                                source = 'test_set')
      variable_lengths <- bind_rows(variable_lengths, temp_length)
    }

    parameters[['train_test_variable_lengths']] <- variable_lengths

  } # Sparsity Check (Makes Sure There's No Single-Level Variables in Train or Test)


  return(parameters)

}
