sparse_variable_check <- function(parameters){

  data = parameters$full_data
  dv = parameters$outcome
  variables = unique(names(parameters$full_data))
  variables <- variables[!grepl('(\\*|\\:)', variables)]
  test_data = data.frame(parameters$test_set, check.names = F)
  train_data = data.frame(parameters$train_set)

  if (!is.null(parameters$assign_factors)){
    factor_level_min = parameters$assign_factors
  } else{
    factor_level_min = 0
  }


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

  for (var in 1:ncol(data)){
    inherit_factor_status <- is.factor(data[[var]])

    if (inherit_factor_status == TRUE){
      factor_variables <- unique(c(factor_variables, colnames(data)[var]))
    }

  } # Check Factor Status Inherited from Dataframe

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
