#' PAI Core Function
#'
#' @param data A structured dataset featuring a target variable of interest (y) and other predictors , providing a comprehensive framework for exploratory analysis and predictive modeling.
#' @param model Machine learning model to employ in protocol; see `caret` (CRAN) for more information on available models.
#' @param outcome Target variable of interest (y); Can be distributed binomialy or continuously.
#' @param predictors Covariate(s); can declare directly or defaults to all if blank or NULL.
#' @param interactions Interactive term(s); Define each interaction within a vector as "Var1*Var2" & separated by a comma.
#' @param drop_vars Specific terms from `predictors` and (or) `interactions` to drop during information omission protocol.
#' @param cores Number of computing cores to employ for parallel processing; Defaults to 1.
#' @param placebo_iterations Number of placebo iterations to employ; Defaults to 10.
#' @param folds Number of K-Folds to employ during `trainControl`; Defaults to 5
#' @param train_split Proportion of data for Train/Test split; Defaults to 0.8 (Indicating an 80/20 Train/Test Split).
#' @param custom_tc Custom `trainControl` parameters for unique model selection from `caret`; Defaults to common parameters for `parRF`, `adaboost`, and `rf` (i.e., tree-based methods).
#' @param assign_factors Integer representing number of levels to auto-assign factor terms; Defaults to 3.
#' @param list_drop_vars If TRUE, allows for the assignment of several variables to drop concurrently as indicated by objects stored in global environment.
#' @param seed Random Seed; Defaults to 1234
#'
#' @return
#' @export
#'
#' @examples
#' Example Using `adaboost` with `sandbox_data` and 5 Cores
#' adaboost_test <- pai(data = sandbox_data, model = 'adaboost', outcome = 'var1', predictors = NULL, interactions = c('var2:var3', 'var4:var5'), cores = 5)
#' Example Using `parRF` with `sandbox_data` and list_drop_vars
#' parRF_test <- pai(data = sandbox_data, model = 'parRF', outcome = 'var1', predictors = NULL, list_drop_vars = c(group_1 = c('var2', 'var3', var'4)))
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
