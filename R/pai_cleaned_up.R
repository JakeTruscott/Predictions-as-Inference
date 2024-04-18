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

sandbox_data <- data.frame(
  var1 = sample(0:1, 100, replace = TRUE),
  var2 = runif(100, min = 0, max = 1),
  var3 = c(sample(0:1, 99, replace = TRUE), 2),
  var4 = runif(100, min = 0, max = 1),
  var5 = sample(0:1, 100, replace = TRUE),
  var6 = runif(100, min = 0, max = 1)

)

data = sandbox_data
outcome = 'var1'
predictors = NULL
model = 'parRF'
interactions = c('var2:var3', 'var4*var5')
drop_vars = NULL
cores = 1
placebo_iterations = 10
folds = NULL
train_split = 0.8
custom_tc = FALSE
assign_factors = NULL
list_drop_vars = FALSE
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
                custom_tc = FALSE,
                assign_factors = 3, #Defaults to 3 - Change to Any Number
                list_drop_vars = FALSE, #Defaults to FALSE
                seed = 1234 #Defaults to 1234
                ){


  start_time <- Sys.time() #Start Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m---------------------- Beginning PAI Process ----------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Start Message

  parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed)





  end_time <- Sys.time() #End Time
  completion_time_minutes<- as.numeric((difftime(end_time, start_time, units = "secs")/60)) #Completion Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m-------------------------- PAI  Complete --------------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Completion Message
  message('\033[32mCompletion Time = ', round(completion_time_minutes,2), ' Minutes \033[0m') #Print Completion Time




}



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

    if (is.null(interactions)){
      parameters[['interactions']] <- 'None'
    } else {
      parameters[['interactions']] <- c(interactions)
    } #Declare Interaction Terms

    if (is.null(drop_vars)){
      parameters[['drop_vars']] <- parameters[['predictors']]
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
                                savePredictions = TRUE)
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

      if (parameters$interactions[1] == 'None'){
        combined_vars <- unique(parameters$predictors)
      } else {
        combined_vars <- unique(c(unique(unlist(stringr::str_split(parameters$interactions, pattern = "\\*|\\:"))), unique(parameters$predictors)))
      } #Get All Vars

      full_data <-  data %>%
        dplyr::select(parameters$outcome, any_of(unlist(combined_vars))) #Get Full Data

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
      factors <- factors[!factors %in% dv] #Remove DV

      parameters['non_factors'] <- list(non_factors) #Put Non-Factors in parameters
      parameters['factors'] <- list(factors) #Same for factors
      parameters['sparse_factors'] <- list(sparse_factors)

      factors <- paste0('factor(', factors, ')') #Add 'as.factor' to factors

      formula_vars <- c(non_factors, factors) #Create Single Formula Vars (No Interactions Yet...)

      if (parameters['interactions'] == 'None') {
          formula_vars <- formula_vars #If No Interactions, Skip
        } else {

        formula_interactions <- c()

        interactions <- unlist(parameters$interactions)

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

        formula_vars <- c(formula_vars, formula_interactions)

      } #Check if Interactions Are Sparse or Factor

      if (!is.null(sparse_check$sparse_factors)) {
        parameters$sparse_factors <- sparse_check$sparse_factors
      } #Update Sparse Factors (If Needed)







    } # Get Formula Vars

    {


    formula <- paste0(dv, '~', paste(formula_vars, collapse = "+"))

    parameters[['base_formula']] <- formula


    } #Create Formula (+ Message for What Was Tossed b/c Sparse)



  } #Create Data, Test/Train Split & Formula -- Remove Sparse Vars

  return(parameters)

}

sparse_variable_check <- function(parameters){

  data = parameters$full_data
  dv = parameters$outcome
  variables = unlist(parameters$predictors)
  test_data = parameters$test_set[[1]]
  train_data = parameters$train_set[[1]]
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
          "\033[32m Interaction(s): \033[0m", "\033[33m", ifelse(is.null(parameters[['interactions']]), "None", paste(paste0('(', unlist(parameters$interactions), ')'), collapse = " ")), "\033[0m \n",
    "\033[32m Variables to Drop: \033[0m", "\033[33m", ifelse(all(parameters$predictors %in% parameters$drop_vars), "All Predictors", unlist(parameters$drop_vars)), " \033[0m \n",
    "\033[32m Train/Test Split: \033[0m", "\033[33m", (parameters$train_split)*100, "/", 100-((parameters$train_split)*100), "\033[0m \n",
    "\033[32m Vars Dropped Due to Sparse Variance in Train/Test: \033[0m", "\033[33m", ifelse(is.null(parameters$sparse_factors), 'None', paste(unlist(paste0('(', parameters$sparse_factors, ')')), collapse = " ")), "\033[0m \n")


}

declared_model <- function(parameters){


}








