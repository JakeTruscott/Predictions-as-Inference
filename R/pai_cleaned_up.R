# To Do:
'
1) Combine variable declarations:
    - Combine Predictors, Factors, and Interactions
    - Allow "assign factors" - but default to NULL (if change to not null or TRUE, then use it)
    - Allow list_drop_vars
    - Allow drop_vars declaration
    - clean up ML declaration

'

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
                assign_factors = FALSE, #Defaults to FALSE - Change to TRUE (4) or Any Number
                list_drop_vars = FALSE, #Defaults to FALSE
                seed = 1234 #Defaults to 1234
                ){

  start_time <- Sys.time()

  message("-------------------------------------------------------------------")
  cat("---------------------- Beginning PAI Process ----------------------\n")
  message("-------------------------------------------------------------------")



predictors <- c('var1', 'var2', 'var3', 'var4', 'var5', 'var6')
parameters['predictors'] <- list(predictors)
interactions <- c('var1:var2')
parameters['interactions'] <- list(interactions)

  end_time <- Sys.time()
  completion_time_seconds <- difftime(end_time, start_time, units = "secs")
  completion_time_minutes <- as.numeric(completion_time_seconds) / 60

  message("-------------------------------------------------------------------")
  cat("-------------------------- PAI  Complete --------------------------\n")
  message("-------------------------------------------------------------------")
  message('Completion Time = ', completion_time_minutes, ' Minutes')




}

data <- data.frame(
  var1 = sample(0:1, 100, replace = TRUE),
  var2 = runif(100, min = 0, max = 1),
  var3 = sample(0:1, 100, replace = TRUE),
  var4 = runif(100, min = 0, max = 1),
  var5 = sample(0:1, 100, replace = TRUE),
  var6 = runif(100, min = 0, max = 1)

)


pai_params_wrapper <- function(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed){

  {

    parameters <- list()

    if (is.null(model)){
      parameters['model'] <- 'parRF'
    } else {
      parameters['model'] <- model
    } #Declare Model from Caret

    if (is.null(outcome)){
      stop('No Outcome Variable Declared \n Try Again')
    } else {
      parameters['outcome'] <- outcome
    } # Declare Outcome (DV)

    if (is.null(predictors)){
      parameters['predictors'] <- c(names(data)[!names(data) %in% parameters['outcome']])
    } else {
      parameters['predictors'] <- list(predictors)
    } # Declare Predictors

    if (is.null(interactions)){
      parameters['interactions'] <- 'None'
    } else {
      parameters['interactions'] <- list(interactions)
    } #Declare Interaction Terms

    if (is.null(drop_vars)){
      parameters['drop_vars'] <- parameters[['predictors']]
    } else {
      parameters['drop_vars'] <- list(drop_vars)
    } # Declare Variables to Drop

    if (is.null(cores)){
      parameters['cores'] <- 1
    } else {
      parameters['cores'] <- as.numeric(cores)
    } #Cores

    if (is.null(placebo_iterations)){
      parameters['placebo_iterations'] <- 10
    } else {
      parameters['placebo_iterations'] <- as.numeric(placebo_iterations)
    } #Placebo Iterations

    if (is.null(folds)){
      paramters['folds'] <- 5
    } else {
      paramters['folds'] <- as.numeric(folds)
    } #K-Folds

    if (is.null(train_split)){
      parameters['train_split'] <- 80
    } else {
      parameters['train_split'] <- as.numeric(train_split)
    } # Train/Test Split

    if (custom_tc == FALSE){
      parameters['custom_tc'] <- 'FALSE'
    } else {
      parameters['custom_tc'] <- 'TRUE'
    } #Custom Train Control

    if (assign_factors == FALSE){
      paramters['assign_factors'] <- 'FALSE'
    } else {
      parameters['assign_factors'] <- as.numeric(assign_factors)
    } #Assign Factor Breaks (Highly Recommend...)

    if (list_drop_vars == FALSE){
      parameters['list_drop_vars'] <- 'FALSE'
    } else {
      parameters['list_drop_vars'] <- 'TRUE'
    } # Drop Vars Grouped in List of Objects

    if (seed == 1234){
      parameters['seed'] <- 1234
    } else {
      parameters['seed'] <- as.numeric(seed)
    } #Seed

  } #Parameter Declaration

  {
    {

      if (parameters$custom_tc == 'FALSE'){
        parameters['train_control'] <- trainControl(method = 'repeatedcv',
                                number = cv_folds,
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

        parameters['train_control'] <- do.call(trainControl, tc_params)


      }


    } # Train Parameters - Custom (If Declared)

  } # Other Params (Custom Train Control)

  {

    {

      if (parameters$interactions == 'None'){
        combined_vars <- unique(parameters$predictors)
      } else {
        combined_vars <- unique(c(unique(unlist(stringr::str_split(parameters$interactions, pattern = "\\*|\\:"))), unique(parameters$predictors)))
      } #Get All Vars

      full_data <-  data %>%
        dplyr::select(any_of(unlist(combined_vars))) #Get Full Data

      parameters['full_data'] <- list(full_data)

    } #Create 'full_data'

    {

      train_index <- createDataPartition(y = full_data[[outcome]], p = 0.8, list = F) #Create Partition Index

      train_set <- full_data[train_index, ] #Split Train
      test_set <- full_data[-train_index, ] #Split Test

      parameters['train_index'] <- train_index
      parameters['train_set'] <- list(train_set)
      parameters['test_set'] <- list(test_set)

    } # Test-Train Split

    {

      formula_vars <- c()

      if (parameters$interactions == 'None'){
        formula_vars <- formula_vars
      } else {
        formula_vars <- c(formula_vars, unlist(parameters$interactions))
      }





    } #Create Formula


  } #Create Data, Test/Train Split & Formula -- Remove Sparse Vars



}




test <- "y ~ var1 + var2 + var1:var2"
as.formula(test)






interactions <- 'var1:var2'






