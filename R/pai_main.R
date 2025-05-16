#' PAI (Predictions as Inference)
#'
#' @param data A structured data frame or tibble object containing a target variable of interest (y) and other predictors, providing a comprehensive framework for exploratory analysis and predictive modeling.
#' @param model Machine learning model to employ in protocol: see `caret` (CRAN) for more information on available model types.
#' @param outcome Target variable of interest (y); Can be distributed binomial or continuous.
#' @param predictors Independent variables (x...n); Can be declared directly -- e.g., c(var1, var2, etc...) -- or defaults to ALL variables not declared as `outcome` if blank or NULL.
#' @param interactions Interactive term(s) -- Note: Define each interaction element within a vector (ex: c(var1*var2)); Can also declare multiple interaction terms within vector separated by a comma.
#' @param drop_vars Specific terms from `predictors` and (or) `interactions` to drop during information omission protocol. If `list_drop_vars` = TRUE, it will drop grouped terms rather than individual.
#' @param save_drop_var_models If TRUE, will store output from omitted variable models in output object.
#' @param cores Number of computing cores to employ for parallel processing; Defaults to 1.
#' @param placebo_iterations Number of placebo iterations to employ; Defaults to 10.
#' @param folds Number of K-Folds to employ during `trainControl`; Defaults to 5.
#' @param train_split Proportion of data for Train/Test split; Defaults to 0.9 (Indicating an 80/20 Train-Test split.)
#' @param drop_sparse_vars If TRUE, will drop variables with sparse data observed in Train/Test split.
#' @param sparse_variable_threshold If `drop_sparse_vars` = TRUE, will declare a minimum proportion threshold in deciding whether to drop variables with observed sparcity in Train/Test split; Defaults to (0.3).
#' @param custom_tc Custom `trainControl` parameters for unique model selection & specificatin from `caret`; Defaults to "common" parameters for `parRF`, `adaboost`, and `rf` (i.e., tree-based methods).
#' @param assign_factors Integer representing number of unique levels to auto-assign factor status in estimation & prediction; Defaults to 3.
#' @param list_drop_vars If TRUE, allows for the assignment of several variables to drop concurrently as indicated by objects stored in global environment.
#' @param seed Random seed; Defaults to 1234
#'
#' @return
#' @export
#'
#' @examples
pai <- function(data, #Data
                model = NULL, #Caret Model
                outcome = NULL, #DV
                predictors = NULL, #IVs
                interactions = NULL, #Interactive Terms
                drop_vars = NULL, #Defaults to All
                save_drop_var_models = FALSE, # Defaults to FALSE
                cores = NULL, #Defaults to 1
                placebo_iterations = NULL, #Defaults to 10
                folds = NULL, #Defaults to 5
                train_split = 0.8, #Defaults to 80/20
                drop_sparse_vars = TRUE,
                sparse_variable_threshold = NULL, # Total Number of Observations Required For Factor Value[x] (If Below Threshold, Will Default to 888)
                custom_tc = FALSE, #Defaults to Basic TC (3 Repeats, Assigned K-Folds, etc.)
                assign_factors = 3, #Defaults to 3 - Change to Any Number
                list_drop_vars = FALSE, #Defaults to FALSE
                seed = 1234 #Defaults to 1234
){


  dependencies <- list.files('R/pai_main_dependencies', full.names = T)
  for (i in 1:length(dependencies)){
    suppressMessages(suppressWarnings(source(dependencies[i])))
  } # Load All Dependencies Functions

  check_and_install_packages()

  start_time <- Sys.time() #Start Time
  message("\033[34m-------------------------------------------------------------------\033[0m\n",
          "\033[32m---------------------- Beginning PAI Process ----------------------\033[0m\n",
          "\033[34m-------------------------------------------------------------------\033[0m\n") #Start Message

  set.seed(seed) #Set Random Seed (Defaults to 1234)

  output <- list()

  parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, save_drop_var_models, cores, placebo_iterations, folds, train_split, drop_sparse_vars, sparse_variable_threshold, custom_tc, assign_factors, list_drop_vars, seed)
  #parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed, drop_sparse_vars) #Compile Parameters from Input Declarations
  output[['parameters']] <- parameters #Add Parameters to Output Object

  print_parameters(parameters) #Print Parameters

  message("Initializing Parallel Environment with \033[37m", cores, " Core(s) \033[0m") #Print Update

  cl <- makeCluster(as.numeric(parameters$cores)) #Allocate Cores
  registerDoParallel(cl) #Register Parallel Environment

  message("Beginning ", parameters$model) #Print Update

  declared_model <- declared_model(parameters) #Return Declared ML Model
  output[['declared_model']] <- declared_model #Add to

  message('Beginning Push Protocol') #Start Message for Placebo Iterations

  pusher <- suppressWarnings(push(output, parameters)) #Push Protocol
  output[['push']] <- pusher #Append to Output

  message('Beginning Placebo Iterations') #Start Message for Placebo Iterations

  placebo <- placebo_shuffle(declared_model, parameters, output) #Run Placebo Iterations
  output[['placebo']] <- placebo$placebo_summary #Append to Output
  output[['placebo_all']] <- placebo$all_returned #Append to Output

  message('Beginning Variable Omissions') #Start Message for Placebo Iterations

  omitting_variables <- dropping_vars(parameters, output) #Run Omitting Vars
  output[['omitting_variables']] <- omitting_variables$fit_change #Append to Output
  output[['omitting_variables_bootstrap']] <- omitting_variables$bootstrap_drop_var
  if (parameters$save_drop_var_models == TRUE){
    output[['omitting_variables_models_output']] <- omitting_variables$omitting_variables_models_output
  } # Append to Output if Parameter == TRUE


  fit_change <- left_join(placebo$placebo_summary, omitting_variables$fit_change, by = 'var') #Create Fit Change Frame
  output[['fit_change']] <- fit_change #Append to Output

  message('Compiling Bootstrapped Confidence Intervals') #Start Message for Placebo Iterations

  bootstrap_cis <- bootstrap_predictions_ci(output, parameters) #Compile Bootstrapped CIs from Predictions
  output[['bootstrap_predictions_CI']] <- bootstrap_cis #Append to Output

  message('Beginning Diagnostics Compilation') #Start Message for Placebo Iterations

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
