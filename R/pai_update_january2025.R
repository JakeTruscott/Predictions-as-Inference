pai <- function(data, #Data
                model = NULL, #Caret Model
                outcome = NULL, #DV
                predictors = NULL, #IVs
                interactions = NULL, #Interactive Terms
                moderators = NULL, # Moderating Terms (var1*var2)
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

  check_and_install_packages() # Check if Packages & Dependencies Installed
  start_time <- Sys.time() #Start Time
  message(start_message) # Start Message
  set.seed(seed) #Set Random Seed (Defaults to 1234)

  output <- list() # Empty List for Output

  parameters <- pai_params_wrapper(data, model, outcome, predictors, interactions, drop_vars, cores, placebo_iterations, folds, train_split, custom_tc, assign_factors, list_drop_vars, seed) #Compile Parameters from Input Declarations
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

  if (length(parameters[['moderator']]) >= 1){
    message('Beginning Analyses of Potential Moderators: ', length(parameters[['moderator']]), ' Declared...')
    moderator_check <- check_moderator(parameters = parameters, output = output)
    output[['moderator_check']] <- moderator_check
  } # Compile Moderators Analysis (If any declared..)


  message('Beginning Diagnostics Compilation') #Start Message for Placebo Iterations

  diagnostics <- pai_diagnostic(output) #Diagnostic Figures & Tables
  output[['diagnostics']] <- diagnostics

  output[['parameters']] <- parameters #Append Parameters to Output

  stopCluster(cl) #Stop Cluster
  registerDoSEQ() #Register Sequential Backend

  end_time <- Sys.time() #End Time
  completion_time_minutes <- as.numeric((difftime(end_time, start_time, units = "secs")/60)) #Completion Time
  summary = c('\033[32mCompletion Time = ', round(completion_time_minutes,2), ' Minutes \033[0m') #Print Completion Time

  message(end_message)


  return(output) #Return Output Object

} #Predictions as Inference Main Function
