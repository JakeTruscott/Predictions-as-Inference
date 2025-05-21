pai_params_wrapper <- function(data, model, outcome, predictors, interactions, drop_vars, save_drop_var_models, cores, placebo_iterations, folds, train_split, drop_sparse_vars, sparse_variable_threshold, custom_tc, assign_factors, list_drop_vars, seed){

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

    if (!is.null(interactions)){
      parameters[['interactions']] <- c(interactions)
    } else {
      parameters[['interactions']] <- NULL
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

    if (seed == 1234){
      parameters[['seed']] <- 1234
    } else {
      parameters[['seed']] <- as.numeric(seed)
    } #Seed

  } #Parameter Declaration

  message('1')

  {

    {

      if (parameters$custom_tc == 'FALSE'){
        parameters[['train_control']] <- trainControl(method = 'repeatedcv',
                                                      number = 10,
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

        tc_params <- trainControl(method = 'repeatedcv',
                                  number = 10,
                                  repeats = 3,
                                  savePredictions = TRUE,
                                  verbose = F)

        for (i in 1:nrow(custom_params)) {
          param_name <- custom_params$param[i]
          param_value <- eval(parse(text = custom_params$value[i]))
          tc_params[[param_name]] <- param_value
        }

        parameters[['train_control']] <- suppressWarnings(do.call(trainControl, tc_params))


      }


    } # Train Parameters - Custom (If Declared)

    {

      outcome_type <- unlist(c(data[parameters[['outcome']]]))
      outcome_levels <- if(length(unique(outcome_type)) > 2){
        outcome_type = 'Continuous'
      } else {
        outcome_type = 'Binomial'
      }
      parameters[['outcome_type']] = outcome_type


    } #Assign DV Type (Binomial or Continuous)

  } # Other Params (Custom Train Control) + Assign DV Type

  message('2')

  {

    {

      if (is.null(parameters$interactions)){
        combined_vars <- unique(parameters$predictors)
      } else {
        combined_vars <- unique(c(unique(unlist(stringr::str_split(parameters$interactions, pattern = "\\*|\\:"))), unique(parameters$predictors)))
      } #Get All Vars

      full_data <-  data %>%
        select(parameters$outcome, any_of(unlist(combined_vars))) #Get Full Data

      {

        if (is.null(parameters$sparse_variable_threshold)){
          full_data <- full_data
        } else {

          sparse_values_check <- function(data){
            sparse_values_list <- list()

            for (var in names(data)){

              if (!is.factor(data[[var]])){
                next
              }

              freq_table <- table(data[[var]])
              rare_values <- names(freq_table[freq_table < 50])

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

      if (!is.null(parameters$interactions)){
        for (interaction in unlist(parameters$interactions)){
          temp_term <- interaction
          vars <- strsplit(temp_term, "([*]|[:])")[[1]]
          full_data <- full_data %>%
            mutate(!!temp_term := !!as.symbol(vars[1]) * !!as.symbol(vars[2]))
        }
      }

      parameters['full_data'] <- list(full_data)

    } #Create 'full_data' (Fix Sparse Factors if Threshold !is.null())

    message('a')

    {

      train_index <- createDataPartition(y = full_data[[parameters[['outcome']]]], p = 0.8, list = FALSE)[, 1]

      train_set <- full_data[train_index, ] #Split Train
      test_set <- full_data[-train_index, ] #Split Test
      parameters[['train_index']] <- train_index
      parameters[['train_set']] <- list(train_set)
      parameters[['test_set']] <- list(test_set)

    } # Test-Train Split

    message('b')

    {

      formula_vars <- c() #Create Empty Object for Formula Vars

      sparse_check <- sparse_variable_check(parameters) #Check Sparse Nature & Assign Factor

      non_factors <- unname(unlist(sparse_check['non-factors'])) #Get Non-Factors
      factors <- unname(unlist(sparse_check['factors'])) #Get Factors
      dv <- unname(sparse_check['outcome'][1]) #Get DV
      sparse_factors <- unname(sparse_check['sparse_factors']) #Get Sparse Factors
      factors <- factors[!factors %in% dv] #Remove DV

      if (parameters$drop_sparse_vars == TRUE){
        factors <- factors[!factors %in% unlist(sparse_factors)]
      }

      message('i')

      non_factors <- non_factors[!non_factors %in% unlist(sparse_factors)]

      parameters[['non_factors']] <- non_factors #Put Non-Factors in parameters
      parameters[['factors']] <- factors #Same for factors
      parameters[['sparse_factors']] <- sparse_factors

      if (is.null(assign_factors) & !is.null(factors)){
        factors <- c(factors)
      } else {
        factors_adj <- paste0('factor(', factors, ')') #Add 'as.factor' to factors
        factors <- ifelse(factors_adj == 'factor()', factors, factors_adj)

      }

      message('ii')

      if(drop_sparse_vars == FALSE){
        factors <- c(factors, unlist(sparse_factors))
      }

      message('iii')

      formula_vars <- c(non_factors, factors) #Create Single Formula Vars (No Interactions Yet...)
      formula_vars <- formula_vars[!formula_vars %in% dv]

      message('vi')

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

      message('v')

      if (!is.null(sparse_check$sparse_factors)) {
        parameters$sparse_factors <- sparse_check$sparse_factors
      } #Update Sparse Factors (If Needed)

      message('vi')

      if (parameters[['drop_sparse_vars']] == TRUE){

        parameters[['predictors']] <- parameters$predictors[!parameters$predictors %in% unlist(parameters$sparse_factors)] #Toss Sparse Predicors
        parameters[['interactions']] <- parameters$interactions[!parameters$interactions %in% unlist(parameters$sparse_factors)] #Toss Interactions w/ Sparse Variables

      } else {
        parameters[['predictors']]  <- parameters$predictors # Don't Toss Sparse Vars
        parameters[['interactions']] <- parameters$interactions # Don't Toss Sparse Vars

      }

      message('vii')


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

      message('viii')

      test_set <- data.frame(parameters$test_set, check.names = F)
      train_set <- data.frame(parameters$train_set, check.names = F)
      full_data <- data.frame(parameters$full_data, check.names = F)

      message('ix')

      if (parameters$drop_sparse_vars == TRUE){
        test_set <- test_set[!names(test_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
        train_set <- train_set[!names(train_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
        full_data <- full_data[!names(full_data) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars

      }

      message('x')

      parameters[['test_set']] <- test_set #Resend to Params
      parameters[['train_set']] <- train_set #Resend to Params
      parameters[['full_data']] <- full_data #Resend to Params

    } # Get Formula Vars

    message('c')

    {

      dv <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', dv, ')'), dv) #Get DV

      parameters[['base_formula']] <- paste0(dv, '~', paste(formula_vars[!is.na(formula_vars)], collapse = "+")) #Create Formula


    } #Create Formula (+ Message for What Was Tossed b/c Sparse)

    message('d')


    {

      parameters$test_set = data.frame(parameters$test_set, check.names = F)
      parameters$train_set = data.frame(parameters$train_set, check.names = F)

    } # Final Fix/Setting of Test/Train Data

  } #Create Data, Test/Train Split & Formula -- Remove Sparse Vars

  return(parameters)

}
