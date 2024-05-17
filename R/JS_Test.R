

library(caret); library(dplyr); library(stringr); library(doParallel); library(broom); library(cowplot); library(grid); library(gridExtra); library(doSNOW); library(gridtext); library(patchwork); library(randomForest); library(rlist); library(xgboost); library(ggplot2); library(ggridges)


perms <- readRDS("C:/Users/Jake Truscott/OneDrive - purdue.edu/Active Research/SJT_R_Package/JS_Book_Replication/perms.rds")

js <- perms[[3]]
js <- data.frame(js)

moods <- grep('mood', names(js))
sals <- unique(c(grep('salience', names(js)), grep('CLR', names(js)), grep('sal.', names(js)), grep('Sal.', names(js))))
lc <- grep('lcD', names(js))
jr <- grep('judRev', names(js))
lats <- grep('lat', names(js))
issues <- which(names(js) %in% c('issueArea', "issuecluster", "issueFactor",
                                 "iA.2", "iA.1", "iA.7", "iA.8", "adminAction", "adminActionState"))
amicis <- which(names(js) %in% c("amaff", "amrev", 'totam', 'wlf', 'chamber', 'sg', 'aclu'))
ideo <- which(names(js) %in% c('scmed', 'mqmed', 'mqmean'))
groups <- list(moods=moods, sals=sals, lc=lc, jr=jr, lats=lats, issues=issues, amicis=amicis, ideo=ideo)

drop_var_list <- list()

for (i in 1:length(groups)){

  temp_group <- groups[[i]]
  temp_group_name <- names(groups[i])

  temp_group_list <- c()

  for (g in temp_group){

    colname = names(js[g])
    temp_group_list <- c(temp_group_list, colname)
  }

  drop_var_list[[as.character(temp_group_name)]] <- temp_group_list

}


data = js #Data
model = 'parRF' #Caret Model
outcome = 'direction' #DV
predictors = NULL #IVs
interactions = NULL #Interactive Terms
drop_vars = drop_var_list #Defaults to All
cores = 10 #Defaults to 1
placebo_iterations = 5 #Defaults to 10
folds = NULL #Defaults to 5
train_split = 0.8 #Defaults to 80/20
custom_tc = FALSE #Defaults to Basic TC (3 Repeats Assigned K-Folds etc.)
assign_factors = 5 #Defaults to 3 - Change to Any Number
list_drop_vars = TRUE #Defaults to FALSE
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

  set.seed(seed) #Set Random Seed (Defaults to 1234)

  output <- list()

  parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed) #Compile Parameters from Input Declarations
  output[['parameters']] <- parameters #Add Parameters to Output Object

  print_parameters(parameters) #Print Parameters

  message("Initializing Parallel Environment with \033[37m", cores, " Core(s) \033[0m") #Print Update

  cl <- makeCluster(as.numeric(parameters$cores)) #Allocate Cores
  registerDoParallel(cl) #Register Parallel Environment

  message("Beginning ", parameters$model) #Print Update

  declared_model <- declared_model(parameters) #Return Declared ML Model
  output[['declared_model']] <- declared_model #Add to

  message('Beginning Placebo Iterations') #Start Message for Placebo Iterations

  placebo <- placebo_shuffle(declared_model, parameters, output) #Run Placebo Iterations
  output[['placebo']] <- placebo$placebo_summary #Append to Output
  output[['placebo_all']] <- placebo$all_returned #Append to Output

  message('Beginning Variable Omissions') #Start Message for Placebo Iterations

  omitting_variables <- dropping_vars(parameters, output) #Run Omitting Vars
  output[['omitting_variables']] <- omitting_variables$fit_change #Append to Output
  output[['omitting_variables_bootstrap']] <- omitting_variables$bootstrap_drop_var

  fit_change <- left_join(placebo$placebo_summary, omitting_variables$fit_change, by = 'var') #Create Fit Change Frame
  output[['fit_change']] <- fit_change #Append to Output

  message('Compiling Bootstrapped Confidence Intervals') #Start Message for Placebo Iterations

  bootstrap_cis <- bootstrap_predictions_ci(output, parameters) #Compile Bootstrapped CIs from Predictions
  output[['bootstrap_predictions_CI']] <- bootstrap_cis #Append to Output

  message('Beginning Push Protocol') #Start Message for Placebo Iterations

  pusher <- push(output, parameters) #Push Protocol
  output[['push']] <- pusher #Append to Output

  diagnostics <- pai_diagnostic(output) #Diagnostic Figures & Tables
  output[['diagnostics']] <- diagnostics

  output[['parameters']] <- parameters #Append Parameters to Output

  stopCluster(cl) #Stop Cluster
  registerDoSEQ() #Register Sequential Backend

  end_time <- Sys.time() #End Time
  completion_time_minutes<- as.numeric((difftime(end_time, start_time, units = "secs")/60)) #Completion Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m-------------------------- PAI  Complete --------------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Completion Message
  message('\033[32mCompletion Time = ', round(completion_time_minutes,2), ' Minutes \033[0m') #Print Completion Time

  return(output) #Return Output Object

} #Predictions as Inference Main Function

{

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
        parameters[['assign_factors']] <- 4
      } else {
        parameters[['assign_factors']] <- as.numeric(assign_factors)
      } #Assign Factor Floor (Default to 4)

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
        outcome_levels <- if(length(unique(outcome_type[,1])) > 2){
          outcome_type = 'Continuous'
        } else {
          outcome_type = 'Binomial'
        }
        parameters[['outcome_type']] = outcome_type


      } #Assign DV Type (Binomial or Continuous)

    } # Other Params (Custom Train Control) + Assign DV Type

    {

      {

        if (is.null(parameters$interactions)){
          combined_vars <- unique(parameters$predictors)
        } else {
          combined_vars <- unique(c(unique(unlist(stringr::str_split(parameters$interactions, pattern = "\\*|\\:"))), unique(parameters$predictors)))
        } #Get All Vars

        full_data <-  data %>%
          select(parameters$outcome, any_of(unlist(combined_vars))) #Get Full Data

        if (!is.null(parameters$interactions)){
          for (interaction in unlist(parameters$interactions)){
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
        factors <- factors[!factors %in% unlist(sparse_factors)]
        non_factors <- non_factors[!non_factors %in% unlist(sparse_factors)]

        parameters[['non_factors']] <- non_factors #Put Non-Factors in parameters
        parameters[['factors']] <- factors #Same for factors
        parameters[['sparse_factors']] <- sparse_factors

        factors <- paste0('factor(', factors, ')') #Add 'as.factor' to factors

        formula_vars <- c(non_factors, factors) #Create Single Formula Vars (No Interactions Yet...)
        formula_vars <- formula_vars[!formula_vars %in% dv]

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


        test_set <- data.frame(parameters$test_set, check.names = F)
        test_set <- test_set[!names(test_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
        train_set <- data.frame(parameters$train_set, check.names = F)
        train_set <- train_set[!names(train_set) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars
        full_data <- data.frame(parameters$full_data, check.names = F)
        full_data <- full_data[!names(full_data) %in% unlist(parameters$sparse_factors)] #Remove Sparse Vars

        parameters[['test_set']] <- test_set #Resend to Params
        parameters[['train_set']] <- train_set #Resend to Params
        parameters[['full_data']] <- full_data #Resend to Params

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
    variables = unique(names(parameters$full_data))
    variables <- variables[!grepl('(\\*|\\:)', variables)]
    test_data = data.frame(parameters$test_set, check.names = F)
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

    } #Check If Factor (# or Less Unique Values)

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


    for (var in variables){

      temp_var <- data[[var]]

      if (length(unique(temp_var)) == 1){
        sparse_factors <- c(sparse_factors, var)
      }


    }


    sparse_check <- list()
    sparse_check[['outcome']] <- dv
    sparse_check[['non-factors']] <- c(variables[!variables %in% factor_variables])
    sparse_check[['factors']] <- c(remaining_factors)
    if (is.null(sparse_factors)){
      sparse_check[['sparse_factors']] <- NULL
    } else {
      sparse_check[['sparse_factors']] <- unique(sparse_factors)
    }

    return(sparse_check) #Return List w/ Factors & Sparse Factors

  } #Check Sparse Factors & Assign as.factor() to Factors for Formula

  print_parameters <- function(parameters){

    message("\033[32m Model: \033[0m", "\033[38m", parameters$model, "\033[0m \n",
            "\033[32m Outcome Variable: \033[0m", "\033[38m", parameters$outcome , "\033[0m \n",
            "\033[32m Predictors: \033[0m", "\033[38m " , ifelse(length(unique(parameters[['predictors']])) < 10, paste(parameters[['predictors']], collapse = " "), length(unique(parameters[['predictors']]))), " \033[0m \n",
            "\033[32m Interaction(s): \033[0m", "\033[38m", ifelse(is.null(parameters[['interactions']]), "None", paste(paste0('(', unlist(parameters$interactions), ')'), collapse = " ")), "\033[0m")


    if (parameters$list_drop_vars == 'FALSE'){
      message("\033[32m Variables to Drop: \033[0m", "\033[38m", ifelse(all(parameters$predictors %in% parameters$drop_vars), "All Predictors", unlist(parameters$drop_vars)), " \033[0m")
    } else {
      message("\033[32m Variables to Drop: \033[0m", paste(paste0('(', names(parameters$drop_vars), ')'), collapse = "; "))
    }

    message("\033[32m Train/Test Split: \033[0m", "\033[38m", (parameters$train_split)*100, "/", 100-((parameters$train_split)*100), "\033[0m \n",
            "\033[32m Vars Dropped Due to Sparse Variance in Train/Test: \033[0m", "\033[31m", ifelse(is.null(parameters$sparse_factors), 'None', paste(unlist(paste0('(', parameters$sparse_factors, ')')), collapse = " ")), "\033[0m \n")


  } #Print Parameters

  declared_model <- function(parameters){

    declared_model <- caret::train(form = as.formula(parameters$base_formula),
                                   data = data.frame(parameters$train_set, check.names = F),
                                   metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
                                   method = as.character(parameters$model),
                                   trControl = parameters$train_control,
                                   localImp = TRUE)

    return(declared_model)

  } #Run Declared ML Model

  placebo_shuffle <- function(declared_model, parameters, output){

    placebos <- data.frame() # Initialize Empty DF

    if(is.null(parameters$interactions)){
      variables <- c(unlist(parameters$predictors))
    } else {
      variables <- c(unlist(parameters$predictors), unlist(parameters$interactions))
    }  # Get Variables


    original_predictions <- predict(output$declared_model, data.frame(parameters$test_set, check.names = F), na.action = na.pass)
    true_values <- data.frame(parameters$test_set, check.names = F)[parameters$outcome][,1] #Set Real Data


    for (rep in 1:as.numeric(parameters$placebo_iterations)){

      if (parameters$list_drop_vars == TRUE){

        variables <- parameters$drop_vars

        for (drop_group in 1:length(variables)){

          shuffle_data <- data.frame(parameters$test_set, check.names = F) #Get Test Data

          temp_drop_group <- unlist(unname(variables[drop_group]))

          for (var_name in 1:length(temp_drop_group)){
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

            original_accuracy <- mean(original_predictions == true_values)
            shuffled_accuracy <- mean(shuffled_predictions == true_values)
            accuracy_change <- shuffled_accuracy - original_accuracy #Get Pred Accuracy Change if Binomial DV

          } else {
            original_rmse <- sqrt(mean((original_predictions - true_values)^2))
            shuffled_rmse <- sqrt(mean((shuffled_predictions - true_values)^2))
            accuracy_change <- original_rmse - shuffled_rmse #Get Root Mean Squared Error Change in Continuous DV
          }

          placebo_temp <- data.frame(rep_count = rep, variable = names(variables[drop_group]), accuracy_change = accuracy_change) # Store the accuracy change
          placebos <- dplyr::bind_rows(placebos, placebo_temp)


        } #If List Drop Vars = TRUE

      } else {

        for (var in variables){

          shuffle_data <- data.frame(parameters$test_set, check.names = F) #Get Test Data

          vars_to_shuffle <- c(var, unlist(stringr::str_split(var, pattern = '(\\*|\\:)'))) #Get All Vars (Including Interactions w/ Vars)
          vars_to_shuffle <- vars_to_shuffle[!grepl('(\\*|\\:)', vars_to_shuffle)] #Shuffle Stand-Alone Vars -- Recompile Interactions
          interaction_vars <- variables[grepl('(\\*|\\:)', variables)] #Grab Interaction Terms from variables vector

          for (var_name in vars_to_shuffle) {
            shuffle_data[[var_name]] <- sample(shuffle_data[[var_name]])
          } #Shuffle Any & All Vars Included in Shuffle

          if (!is.null(interaction_vars)){
            for (interaction in 1:length(interaction_vars)){
              t <- interaction_vars[[interaction]][[1]]
              t <- stringr::str_split(t, pattern = '(\\:|\\*)')[[1]]
              shuffle_data[interaction_vars[interaction]] <- apply(shuffle_data[, t], 1, prod)
            }
          } # For Interaction Vars, Reconfigure After Shuffling Stand-Alone Terms


          shuffled_predictions <- predict(output$declared_model, newdata = shuffle_data, na.action = na.pass) # Predict using the shuffled data

          if (parameters[['outcome_type']] == 'Binomial'){

            original_accuracy <- mean(original_predictions == true_values)
            shuffled_accuracy <- mean(shuffled_predictions == true_values)
            accuracy_change <- shuffled_accuracy - original_accuracy #Get Pred Accuracy Change if Binomial DV

          } else {
            original_rmse <- sqrt(mean((original_predictions - true_values)^2))
            shuffled_rmse <- sqrt(mean((shuffled_predictions - true_values)^2))
            accuracy_change <- original_rmse - shuffled_rmse #Get Root Mean Squared Error Change in Continuous DV
          }

          placebo_temp <- data.frame(rep_count = rep, variable = var, accuracy_change = accuracy_change) # Store the accuracy change
          placebos <- dplyr::bind_rows(placebos, placebo_temp) # Store






        } #For Var in Variables

      }  #If List Drop Vars = F

      if (rep %% 5 == 0) {
        message("\033[37m           Completed Placebo Shuffling Iteration \033[0m", rep) #Print Update

      }

    } #For Rep in Placebo Iterations

    placebos_all_returned <- placebos

    placebo_summary <- placebos %>%
      select(-c(rep_count)) %>%
      rename(var = variable) %>%
      group_by(var) %>%
      summarize(mean_change = mean(accuracy_change),
                sd_change = sd(accuracy_change),
                lower_bound = mean_change - qt(0.975, n() - 1) * (sd_change / sqrt(n())),
                upper_bound = mean_change + qt(0.975, n() - 1) * (sd_change / sqrt(n())),
                max_change = max(accuracy_change),
                min_change = min(accuracy_change))

    placebo <- list()
    placebo[['placebo_summary']] <- placebo_summary
    placebo[['all_returned']] <- placebos_all_returned

    return(placebo)

  } # Placebo Protocol

  dropping_vars <- function(parameters, output){

    fit_change <- data.frame() #Initialize Empty DF to Store Fit Changes from Dropping Vars
    bootstrap_drop_var <- list() #Initialize Empty List to Store Bootstrap Output

    vars_to_drop <- c() #Initialize Empty Object for Vars to Drop (Based on Params Declaration)

    if (parameters$list_drop_vars == 'FALSE'){
      vars_to_drop <- unlist(parameters$drop_vars)
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

          combinations <- dplyr::bind_rows(combinations, temp_combination) #Add to Combinations Frame

        }


      } # Create Combinations (If Non-List Drop Vars)
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

          fit_change <- dplyr::bind_rows(fit_change, change_temp) #Append to fit_change

          {

            test_data = data.frame(output$parameters$test_set, check.names = F) #Grab Test Data
            test_data = test_data[!names(test_data) %in% unlist(parameters$sparse_factors)]

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
            comparison_set <- data.frame(parameters$test_set, check.names = F)[outcome_variable][,1] #Set Real Data

            if (parameters[['outcome_type']] == 'Binomial'){
              accuracy <- mean(predictions == comparison_set) #Get Predictive Accuracy from Predictions v. Real Data if Binomial DV
            } else {
              accuracy <- sqrt(mean((predictions - comparison_set)^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
            }


            bootstrap_output <- list() #Create Empty List to Store Bootstrap Outputs

            for (i in 1:100) {
              bootstrap_indices <- sample(nrow(test_data), replace = TRUE) #Generate Bootstrap Sample
              bootstrap_test_data <- test_data[bootstrap_indices, ] #Subset Test Data by Sample Indeces
              bootstrap_test_data <- bootstrap_test_data[, !names(bootstrap_test_data) == parameters$outcome, drop = FALSE] #Remove DV
              bootstrap_predictions <- predict(output$declared_model, newdata = bootstrap_test_data) #Predict on bootstrap Sample

              if (parameters[['outcome_type']] == 'Binomial'){
                bootstrap_accuracy <- mean(bootstrap_predictions - predictions) #Calculate Accuracy if Binomial

              } else {
                bootstrap_accuracy <- mean(bootstrap_predictions - predictions) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
              }


              bootstrap_output[[i]] <- list(bootstrap_data = bootstrap_test_data,
                                            bootstrap_accuracies = bootstrap_accuracy) #Append to List

            } #Compile Predictions from Bootstrapped Samples of Test Data




          } #Bootstrap

          bootstrap_drop_var[[as.character(temp_combination_row$dropped_var)]] <- bootstrap_output

          message("\033[37m           Completed Variable Omission For \033[0m", temp_combination_row$dropped_var) #Print Update

        }

      } # Assess Fit Change from Omitting Vars (If Non-List Drop Vars)
    } else {
      {

        combinations <- data.frame()
        vars_to_drop <- parameters$drop_vars
        other_vars <- parameters$predictors

        for (drop_var in 1:length(vars_to_drop)){

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

          outcome_variable <- ifelse(parameters$outcome_type == 'Binomial', paste0('factor(', parameters$outcome, ')'), parameters$outcome) # Add factor() to outcome_var if Binomially Distributed

          temp_combination <- paste0(outcome_variable, '~', paste(other_vars, collapse = '+'))

          temp_combination <- data.frame(
            temp_combination = temp_combination,
            dropped_var = names(vars_to_drop[drop_var])) #Collapse Into Single Temp DF

          combinations <- dplyr::bind_rows(combinations, temp_combination) #Add to Combinations Frame

        }

      } #Create Combinations (If List Drop Vars)
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

          fit_change <- dplyr::bind_rows(fit_change, change_temp) #Append to fit_change

          {

            test_data = data.frame(output$parameters$test_set, check.names = F) #Grab Test Data
            test_data = test_data[!names(test_data) %in% unlist(parameters$sparse_factors)]

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
            comparison_set <- data.frame(parameters$test_set, check.names = F)[outcome_variable][,1] #Set Real Data

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

    } #Indicate Source of Drop Vars (List Objects or Declared/Assigned Vars)

    omitting_vars <- list(fit_change = fit_change,
                          bootstrap_drop_var = bootstrap_drop_var)

    return(omitting_vars)

  } #Omitting Variables

  push <- function(output, parameters){

    output <- output

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
      data = data.frame(parameters$test_set, check.names = F) #Get Data (Test Data)
      temp_var_distribution = unique(data[,temp_var]) #Get Distribution of Temp Var

      factor_vars <- output$parameters$factors
      is_factor = TRUE # Default is_factor = T

      tryCatch({
        sd_var <- sd(data[, temp_var])
        steps <- seq(-2 * sd_var, 2 * sd_var, (4 * sd_var) / 100)
        is_factor = FALSE #Test to see if not actually factor/categorical distribution
      }, error = function(e){
        is_factor = TRUE # If it is Factor, Keep it True
      })

      if (is_factor == TRUE || temp_var %in% factor_vars){
        next
      }


      var_push_predictions <- data.frame() #Initialize Empty DF for Push Predictions

      for (step in 1:length(steps)){

        temp_step = steps[step]

        temp_pred <- push_pred(mod = output$declared_model,
                               var = ifelse(is_factor == TRUE, factor(temp_var), temp_var),
                               stepper = temp_step,
                               Z = data,
                               outcome_type = parameters$outcome_type,
                               outcome_var = parameters$outcome,
                               parameters = parameters) #Calculate for Step[step]

        temp_pred <- data.frame(
          step = temp_step,
          onecount = temp_pred[1],
          acc = temp_pred[2]) #Put Output in Temp DF

        var_push_predictions <- dplyr::bind_rows(var_push_predictions, temp_pred)

      }

      push_output[[temp_var]] <- var_push_predictions

      message("\033[37m           Completed Push Protocol For \033[0m", temp_var) #Print Update

    } #For Variable in Variables

    return(push_output)

  } # Push + push_pred from Stepper

  push_pred <- function(mod, var, stepper, Z, outcome_type, outcome_var, parameters){

    if (outcome_type == 'Continuous'){
      Z[[var]] <- Z[[var]] + stepper
      pred <- predict(mod, Z)
      true <- Z[[outcome_var]]
      dif <- pred - true
      return(dif)
    } else {

      Z[[var]] <- Z[[var]] + stepper
      pred <- predict(mod, Z)
      true <- Z[[outcome_var]]
      onecount <- length(which(pred=='1'))/length(true)
      acc <- length(which(pred==true))/length(true)
      return(c(onecount, acc))
    }

    preds <- predict(mod, Z)
    return(length(which(trues==preds))/length(preds))


  } #Predictive Accuracy from Steps

  suppress_message <- function(expr){
    sink(tempfile())
    result <- tryCatch(expr, finally = {
      sink()
    })
    invisible(result)
  } #Special Function to Suppress Messages from Caret (R)

  bootstrap_predictions_ci <- function(output, parameters){

    test_data = data.frame(parameters$test_set, check.names = F) #Grab Test Data
    test_data = test_data[!names(test_data) %in% unlist(parameters$sparse_factors)]

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

    predictions <- predict(output$declared_model, newdata = test_data) #Get Base Predictions
    comparison_set <- data.frame(parameters$test_set, check.names = F)[outcome_variable][,1] #Set Real Data

    if (parameters[['outcome_type']] == 'Binomial'){
      accuracy <- mean(predictions == comparison_set) #Get Predictive Accuracy from Predictions v. Real Data if Binomial DV
    } else {
      accuracy <- sqrt(mean((predictions - comparison_set)^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
    }


    bootstrap_output <- list() #Create Empty List to Store Bootstrap Outputs

    for (i in 1:100) {
      bootstrap_indices <- sample(nrow(test_data), replace = TRUE) #Generate Bootstrap Sample
      bootstrap_test_data <- test_data[bootstrap_indices, ] #Subset Test Data by Sample Indeces
      bootstrap_test_data <- bootstrap_test_data[, !names(bootstrap_test_data) == parameters$outcome, drop = FALSE] #Remove DV
      bootstrap_predictions <- predict(output$declared_model, newdata = bootstrap_test_data) #Predict on bootstrap Sample

      if (parameters[['outcome_type']] == 'Binomial'){
        bootstrap_accuracy <- mean(bootstrap_predictions == test_data[parameters[['outcome']]][bootstrap_indices,]) #Calculate Accuracy if Binomial

      } else {
        bootstrap_accuracy <- sqrt(mean((bootstrap_predictions - test_data[parameters[['outcome']]][bootstrap_indices,])^2)) #Get Root Mean Squared Error from Predictions v. Real Data if Continuous DV
      }

      bootstrap_output[[i]] <- list(bootstrap_data = bootstrap_test_data,
                                    bootstrap_accuracies = bootstrap_accuracy) #Append to List

    } #Compile Predictions from Bootstrapped Samples of Test Data

    return(bootstrap_output) #Return Summary



  } #Bootstrap CIs

  pai_diagnostic <- function(output){

    diagnostics <- list() #Create Empty List to Store Outputs
    parameters <- output$parameters #Grab Parameters

    {

      placebo_vars <- output$placebo$var

      {

        dv_type = output$parameters$outcome_type
        if (dv_type == 'Binomial'){
          y_label = 'Change in Predictive Accuracy\n'
        } else {
          y_label = 'Change in RMSE\n'
        }

        placebo_figure <- output$fit_change %>%
          filter(var %in% placebo_vars) %>%
          mutate(var_numeric = 1:nrow(.)) %>%
          mutate(var = ifelse(grepl("\\*", var), gsub("\\*", " x\n", var), var)) %>%
          ggplot(aes(x = factor(var))) +
          geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
          geom_rect(aes(xmin = var_numeric - 0.15, xmax = var_numeric + 0.15,
                        ymin = min_change, ymax = max_change, fill = 'Range of Predicted\nAccuracy from Placebos'), colour = 'gray5') +
          #geom_rect(aes(xmin = var_numeric - 0.15, xmax = var_numeric + 0.15, ymin = lower_bound, ymax = upper_bound, fill = '95% Confidence\nInterval'), colour = 'gray5') +
          geom_point(aes(y = fit_change, colour = 'Prediction from Model Fit\nAfter Dropping Information'), size = 3, shape = 10) +
          labs(y = y_label,
               x = '\nVariable\n',
               fill = ' ',
               colour = ' ') +
          scale_fill_manual(values = c("gray", "gray50")) +
          scale_colour_manual(values = 'black') +
          theme_minimal() +
          theme(legend.position = 'bottom',
                panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
                axis.text = element_text(size = 10, colour = 'black'),
                axis.title = element_text(size = 12, colour = 'black'))


      } #Render Placebo Figure
      {

        placebo_CI_figure <- output$placebo_all %>%
          rename(var = variable) %>%
          ggplot(aes(x = accuracy_change, y = factor(var))) +
          geom_vline(xintercept = 0, linetype = 2, colour = 'gray5') +
          geom_density_ridges(alpha = 0.75, scale = 0.9) +
          coord_flip() +
          labs(x = y_label,
               y = '\nVariable\n',
               fill = ' ',
               colour = ' ') +
          scale_fill_manual(values = c("gray", "gray50")) +
          scale_colour_manual(values = 'black') +
          theme_minimal() +
          theme(legend.position = 'bottom',
                panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
                axis.text = element_text(size = 10, colour = 'black'),
                axis.title = element_text(size = 12, colour = 'black'))

      } # Render Placebo Confidence Interval Figure
      {

        bootstrap_omit_vars <- data.frame()
        bootstrap_output <- output$omitting_variables_bootstrap

        {

          for (i in 1:length(bootstrap_output)){

            temp_bootstrap_list <- bootstrap_output[[i]]

            for (b in 1:length(temp_bootstrap_list)){
              temp_bootstrap_data <- temp_bootstrap_list[[b]]$bootstrap_data

              if (is.null(temp_bootstrap_data)){
                next
              } #Error Handling (Skip)

              temp_bootstrap_accuracy <- temp_bootstrap_list[[b]]$bootstrap_accuracies #Get Temp Accuracy
              temp_bootstrap <- data.frame(accuracy = temp_bootstrap_accuracy, bootstrap_id = b, dropped_var = names(bootstrap_output)[i]) #Combine
              bootstrap_omit_vars <- dplyr::bind_rows(bootstrap_omit_vars, temp_bootstrap) #Append

            }

          }

        } # Retrieve & Organization Bootstrap Data

        {

          bootstrap_distribution <- bootstrap_omit_vars %>%
            select(accuracy, bootstrap_id, dropped_var) %>%
            unique() %>%
            group_by(dropped_var) %>%
            mutate(median = median(accuracy), #Median
                   mean = mean(accuracy), #Mean
                   conf_lower = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[1]), #2.5%
                   conf_higher = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[2]), #97.5
                   max = max(accuracy), #Max
                   min = min(accuracy), #Min,
                   bootstraps = max(bootstrap_id))

        } # Density of Accuracies by Var When Dropped

        {

          dv_type = output$parameters$outcome_type
          if (dv_type == 'Binomial'){
            y_label = 'Change in Predictive Accuracy After Omitting Information\n'
          } else {
            y_label = 'Change in RMSE After Omitting Information\n'
          }

          placebo_bootstrap_omit_figure <- bootstrap_distribution %>%
            ggplot(aes(x = accuracy, y = factor(dropped_var))) +
            geom_vline(xintercept = 0, linetype = 2, colour = 'gray5') +
            geom_density_ridges(alpha = 0.75, scale = 0.9) +
            coord_flip() +
            labs(x = y_label,
                 y = '\nVariable\n',
                 fill = ' ',
                 colour = ' ') +
            scale_fill_manual(values = c("gray", "gray50")) +
            scale_colour_manual(values = 'black') +
            theme_minimal() +
            theme(legend.position = 'bottom',
                  panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
                  axis.text = element_text(size = 10, colour = 'black'),
                  axis.title = element_text(size = 12, colour = 'black'))


        } #Plot Density of Accuracy Change by Var When Dropped

      } #bootstrap Predictions from Dropping

      if (length(placebo_vars) >= 5){
        placebo_figure <- placebo_figure +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        placebo_CI_figure <- placebo_CI_figure +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        placebo_bootstrap_omit_figure <- placebo_bootstrap_omit_figure +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } # Change X-Axis Label Tilt if More than 5 Vars (Avoids Overlapping Var Labels)

      diagnostics[['placebo']] <- list()

      diagnostics[['placebo']][['placebo_full']] <- placebo_figure #Add Placebo Figure to Diagnostic
      diagnostics[['placebo']][['placebo_CI']] <- placebo_CI_figure #Add Placebo Figure to Diagnostic
      diagnostics[['placebo']][['bootstrap_omit_information']] <- placebo_bootstrap_omit_figure # Add Omit Variable Figure to Diagnostic

    } #Placebo Iterations

    {

      linear_vars <- names(output$push)
      parameters <- output$parameters
      push_output <- output$push #Grab Push Output
      original_test_data <- data.frame(parameters$test_set, check.names = F)

      diagnostic_push <- list() #Create Empty List for Output

      for (var in linear_vars){
        diagnostic_push[[var]] <- list()
      } #Create Var-Level List for diagnostic_push

      for (var in linear_vars){

        temp_dat <- push_output[[var]] #Grab Temp Var


        if (var %in% c(unlist(output$parameters$factors))){
          base_plot <- ggplot(data = temp_dat, aes(x = factor(step), y = acc)) +
            geom_point() #Generate Base Plot
        } else {
          base_plot <- ggplot(data = temp_dat, aes(x = step, y = acc)) +
            geom_point() #Generate Base Plot
        } #Create Base Plot (Assign x as Factor if in Parameters$factors)

        scott_info <- hist(temp_dat$step, breaks = "scott", plot = FALSE) #Get # Bins from Scot's Normal Reference Rule
        breakpoints <- length(scott_info$breaks)
        temp_dat$bin <- cut_interval(as.numeric(temp_dat$step), n = breakpoints) #Assign Bins

        for (temp_bin in 1:length(unique(temp_dat$bin))){
          bin_label <- unique(temp_dat$bin)[temp_bin]
          temp_bin_dat <- temp_dat %>%
            filter(bin == unique(temp_dat$bin)[temp_bin])
          lm_bin_temp <- lm(acc ~ step, data = temp_bin_dat)
          suppressWarnings({
            diagnostic_push[[var]][['linear_fit']][[as.character(bin_label)]] <- broom::tidy(lm_bin_temp) %>%
              mutate(sig = case_when(
                .default = '',
                p.value <= 0.05 & p.value > 0.01 ~ '*',
                p.value <=0.01 & p.value > 0.001 ~ '**',
                p.value <= 0.001 ~ '***'
              ))

            base_plot <- base_plot +
              geom_smooth(method = 'lm', formula = y ~ x, data = temp_bin_dat)
          })

        } #Calculate LM by Bins & Append to Base Plot

        base_plot <- base_plot +
          theme_minimal() +
          labs(x = '\nStep\n',
               y = '\nAccuracy\n') +
          theme(
            panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 12, colour = 'black')
          ) #Create base Plot

        diagnostic_push[[var]][['linear_plot']] <- base_plot #Combine and Store


      } #By Var - Calculate Linear Fit & Plot

      diagnostics[['push']] <- diagnostic_push

    } #Linear Fit Across Bins (Push)

    {

      bootstrap_output <- output$bootstrap_predictions_CI #Get Bootstrap List Output
      bootstrap_accuracies <- data.frame() #Create Empty DF to Create Combined Frame
      diagnostics[['bootstrap']] <- list()

      {

        for (i in 1:length(bootstrap_output)){

          temp_bootstrap_data <- bootstrap_output[[i]]$bootstrap_data #Get Temp Bootstrap Test Data

          if (is.null(temp_bootstrap_data)){
            next
          } #Error Handling (Skip)

          temp_bootstrap_accuracy <- bootstrap_output[[i]]$bootstrap_accuracies #Get Temp Accuracy
          temp_bootstrap <- data.frame(temp_bootstrap_data, accuracy = temp_bootstrap_accuracy, bootstrap_id = i) #Combine
          bootstrap_accuracies <- dplyr::bind_rows(bootstrap_accuracies, temp_bootstrap) #Append

        }

      } # Retrieve & Organization Bootstrap Data

      {

        bootstrap_distribution <- bootstrap_accuracies %>%
          select(accuracy, bootstrap_id) %>%
          unique() %>%
          mutate(median = median(accuracy), #Median
                 mean = mean(accuracy), #Mean
                 conf_lower = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[1]), #2.5%
                 conf_higher = as.numeric(quantile(accuracy, c((1 - 0.95) / 2, 1 - (1 - 0.95) / 2))[2]), #97.5
                 max = max(accuracy), #Max
                 min = min(accuracy), #Min,
                 bootstraps = max(bootstrap_id))

        mean_label <- paste0('Mean (', round(bootstrap_distribution$mean[1], 2), ')')
        median_label <- paste0('Median (', round(bootstrap_distribution$median[1], 2), ')')

        if (output$parameters$outcome_type == 'Binomial'){
          x_label = '\nAccuracy from Bootstrapped Test Sets'
        } else {
          x_label = '\nRMSE from bootstrapped Test Sets'
        }


        bootstrap_ci_figure <- ggplot(data = bootstrap_distribution, aes(x = accuracy)) +
          geom_density(fill = 'gray50', colour = 'gray5') +
          geom_vline(aes(xintercept = mean, colour = 'Mean', linetype = 'Mean')) +
          geom_vline(aes(xintercept = median, colour = 'Median', linetype = 'Median')) +
          geom_hline(yintercept = 0, colour = 'black') +
          scale_colour_manual(name = " ",
                              values = c('Mean' = 'gray5', 'Median' = 'gray5'),
                              labels = c(mean_label, median_label)) +
          scale_linetype_manual(name = " ",
                                values = c('Mean' = "solid", 'Median' = "dashed"),
                                labels = c(mean_label, median_label)) +
          theme_minimal() +
          labs(x = x_label,
               y = '\n') +
          theme(
            panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 12, colour = 'black'),
            legend.position = 'bottom',
            legend.title = element_blank(),
            legend.text = element_text(size = 10, colour = 'black'),
            legend.key = element_rect(colour = "black")
          )

        diagnostics$bootstrap[['bootstrap_output']] <- bootstrap_accuracies
        diagnostics$bootstrap[['bootstrap_distribution']] <- bootstrap_ci_figure

      } #Density of Accuracies Figure

      {

        bootstrap_vars <- names(bootstrap_accuracies[!names(bootstrap_accuracies) %in% c('accuracy', 'bootstrap_id')])
        diagnostics$bootstrap[['variable_figures']] <- list()

        for (var in bootstrap_vars){

          temp_var <- var

          temp_var_bootstrap_data <- bootstrap_accuracies[, c(temp_var, 'accuracy', 'bootstrap_id')] %>%
            group_by(.data[[var]]) %>%
            summarise(mean_accuracy = mean(accuracy),
                      lower_ci = mean_accuracy - 1.96 * sd(accuracy) / sqrt(n()),
                      upper_ci = mean_accuracy + 1.96 * sd(accuracy) / sqrt(n()))

          temp_var_bootstrap_figure <- ggplot(temp_var_bootstrap_data, aes(x = if (temp_var %in% unlist(parameters$factors)) factor(.data[[temp_var]]) else .data[[temp_var]], y = mean_accuracy)) +
            geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5) +
            geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = "95% Confidence\nIntervals"), alpha = 0.2) +  # Add shaded region
            geom_point(size = 2, aes(shape = 'Mean\nAccuracy'), fill = 'white') +  # Add points
            theme_minimal() +
            scale_fill_manual(values = 'deepskyblue3') +
            scale_colour_manual(values = 'black') +
            scale_shape_manual(values = 21) +
            labs(x = paste0('\n', as.character(temp_var)),
                 y = '\n') +
            theme(
              panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
              axis.text = element_text(size = 12, colour = 'black'),
              axis.title = element_text(size = 12, colour = 'black'),
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.text = element_text(size = 10, colour = 'black'),
              legend.key = element_rect(colour = "black"))

          if (!temp_var %in% unlist(parameters$factors)){
            temp_var_bootstrap_figure <- temp_var_bootstrap_figure +
              stat_smooth(method = 'loess', aes(colour = 'Locally Weighted Scatterplot\nSmoothing (LOESS) Fit w/ SEs'), linetype = 2)
          }


          diagnostics$bootstrap$variable_figures[[var]] <- temp_var_bootstrap_figure

        }


      } # Confidence Intervals by Var


    } #Confidence Intervals (Bootstrap)

    return(diagnostics)

  } # Diagnostic Plots & Stats

} #Dependency Functions



js_test <- pai(data = js,
               model = 'parRF',
               outcome = 'direction',
               predictors = NULL,
               interactions = NULL,
               drop_vars = drop_var_list,
               cores = 8,
               placebo_iterations = 100,
               list_drop_vars = TRUE)

save(js_test, file = 'C:/Users/Jake Truscott/Desktop/JS_Test.rdata')
save(js_test, file = "C:/Users/Jake Truscott/Documents/GitHub/Prediction-as-Inference/R/Johnson_Strother_Test_Output.R")


pai_diagnostic_retrieval(output = js_test,
                         diagnostic = 'placebo',
                         type = 'all',
                         combine_plots = T)

pai_diagnostic_retrieval(output = js_test,
                         diagnostic = 'summary')


final_model <- js_test$declared_model
train_data <- data.frame(js_test$parameters$train_set, check.names = F)

trainPred <- predict(final_model, newdata = train_data)
accuracy <- sum(trainPred == train_data$direction) / nrow(train_data)

