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
          "\033[32m Vars Dropped Due to Sparse Variance in Train/Test: \033[0m", "\033[31m", ifelse(is.null(parameters$sparse_factors) | parameters$drop_sparse_vars == F, 'None', paste(unlist(paste0('(', parameters$sparse_factors, ')')), collapse = " ")), "\033[0m \n")


} #Print Parameters
