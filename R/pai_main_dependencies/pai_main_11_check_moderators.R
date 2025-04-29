#- doubleML is fine
#- declaring potential moderators is fine
#- classic use case (diagnostics) -- Instead of regression "as best as possible, what the *TRUE* effects are"
#- for HTE's -- put it on the user to construct data correctly


check_moderator <- function(parameters, output){

  declared_model_output <- output[['declared_model']] # Retrieve Initial Model
  declared_model_formula <- parameters[['base_formula']]

  moderators <- list() # Empty List to Store Moderator Combos
  moderator_output <- list() # Empty List to Store Moderator Models

  message('Beginning Analyses of Potential Moderators: ', length(parameters[['moderator']]), ' Declared...')

  for (moderator_combination in 1:length(parameters[['moderator']])){

    temp_combination <- parameters[['moderator']][moderator_combination] # Retrieve Temp Combination (1:length(#combinations declared))
    unlisted_moderator_terms <- unlist(strsplit(temp_combination, split = '(\\*|\\:)'))
    factor_terms <- parameters[['factors']]
    temp_combination_returned <- list()
    for (moderator_term in 1:length(unlisted_moderator_terms)){
      temp_moderator_term <- unlisted_moderator_terms[moderator_term]
      if (temp_moderator_term %in% factor_terms){
        temp_combination_returned[moderator_term] <- paste0('factor(', temp_moderator_term, ')')
      } else {
        temp_combination_returned[moderator_term] <- temp_moderator_term
      } # If Term in factor terms
    } # For each term -- check if factor & return

   moderators[moderator_combination] <- paste0(temp_combination_returned, collapse = '*') # Append combination to 'moderator' list
  } # For Each Declared Moderator Combo -- Check if Any Factor, Reassign to 'moderator' list --> to run each moderator model w/ declared interaction


  for (moderator in 1:length(moderators)){
    temp_moderator <- moderators[[moderator]] # Return Moderator from 1:length(moderators)
    temp_moderator_formula <- paste0(declared_model_formula, '+', moderator) # Append temp_moderator to existing formula for declared_model
    temp_moderator_model <- suppressMessages(suppressWarnings(caret::train(form = as.formula(temp_moderator_formula),
                                                                      data = parameters$train_set,
                                                                      metric = ifelse(parameters$outcome_type == 'Binomial', 'Accuracy', 'RMSE'),
                                                                      method = as.character(parameters$model),
                                                                      trControl = parameters$train_control,
                                                                      localImp = TRUE))) # Run Model w/ Moderator Attached

    moderator_output[[moderator]] <- list() # Empty List In moderator_output to store results & preliminary analysis
    moderator_output[[moderator]][['moderator']] <- temp_moderator # Add Moderator Terms
    moderator_output[[moderator]][['formula']] <- temp_moderator_formula # Updated Formula
    moderator_output[[moderator]][['output']] <- temp_moderator_model # Model Output
    moderator_output[[moderator]][['varImp']] <- varImp(temp_moderator_model) # varImp Output
    moderator_output[[moderator]][['resampling']] <- resamples(list(with_interaction = temp_moderator_model, baseline = declared_model_output)) # Resampling
    moderator_output[[moderator]][['moderator_heatmap']] <- suppressMessages(suppressWarnings(partial(temp_moderator_model,
                             pred.var = c(unlist(strsplit(temp_moderator, split = '\\*'))),
                             grid.resolution = 10) %>%
      ggplot(aes(x = c[,1], y = c[,2])) +
      geom_tile(aes(fill = yhat), colour = 'white') +
      scale_fill_gradient(low = "deepskyblue3", high = "coral4") +
      scale_y_continuous(lim = c(min(c[,2]), max(c[,2]))) +
      scale_x_continuous(lim = c(min(c[,1]), max(c[,1]))) +
      theme_minimal() +
      labs(x = paste0('\n', names(c)[1]),
           y = paste(names(c)[2], '\n'),
           title = paste0(names(c[1]), ' x ', names(c)[2]),
           fill = expression(hat(y))) +   # Update the legend title here
      theme(panel.border = element_rect(linewidth = 1, colour = 'black', fill = NA),
            axis.title = element_text(size = 14, colour = 'black'),
            axis.text = element_text(size = 12, colour = 'black'),
            legend.background = element_rect(size = 1, colour = 'black', fill = NA),
            legend.title = element_text(size = 14, face = "bold", colour = "black", hjust = 0.5)))) # Heatmap of Moderator


      } # For Each Moderator in Moderators


  return(moderator_output)

  } # Moderators

output <- js_test

check_moderator_doubleML <- function(output){

  message('Note: DoubleML Requires All Variables Retain Numeric or Integer Structure -- Converting Factor and Character Values Using One-Hot Encoding')

  parameters <- output[['parameters']] # Retrieve Params from Output Object
  declared_dv <- parameters[['outcome']] # Outcome (Declared DV)
  suppress_message(full_data <- parameters[['full_data']] %>%
    mutate(across(where(~ is.character(.) | is.factor(.)), ~ as.numeric(as.factor(.))))) # Full Dataset

  declared_dv_type <- parameters[['outcome_type']] # Declared DV Type (Binomial v. Continuous)

  if (declared_dv_type == 'Binomial'){
    if (!all(unique(full_data[[declared_dv]]) %in% c(0, 1)) || length(unique(full_data[[declared_dv]])) != 2){
      message("\033[97mNote:\033[0m DoubleML With Dichotomous (Binary) Dependent Variable Requires Integer (0/1) Structure\nRe-Encoding:\033[0m")
      message('     --- \033[97m', min(full_data[[declared_dv]]), ' = 0\033[0m')
      message('\033[97m     --- \033[97m', max(full_data[[declared_dv]]), ' = 1\033[0m')
      full_data <- full_data %>%
        mutate(!!declared_dv := ifelse(!!sym(declared_dv) == min(!!sym(declared_dv)), 0, 1))

    } # If Dichotomous DV and Not Distributed (0,1) -- Convert Min to 0, Max to 1
  } # If Dichotomous DV

  declared_treatments <- parameters[['moderators']] <- c('sal.lat.issuemood', 'mqmean')
  #declared_treatments <- parameters[['moderators']] # Declared Moderator(s)

  for (moderator in 1:length(declared_treatments)){

    declared_covariates <- names(full_data)[names(full_data) %in% parameters[['predictors']]] # Reduce Full Data to Just Predictors
    declared_covariates <- setdiff(declared_covariates, c(declared_dv, declared_treatments[moderator])) # Redundancy: Make Sure DF & Active Moderator Also Removed

    moderator_structure <- ifelse(length(unique(full_data[[declared_treatments[moderator]]])) > 2, 'Continuous', 'Binomial')

    {

      assign_doubleML_models <- function(declared_dv_type, moderator_structure) {
        ml_l = lrn("regr.ranger")  # Random forest for Continuous outcomes
        ml_m = lrn("classif.ranger", predict_type = "prob")  # Random forest for classification
        if (declared_dv_type == "Continuous" & moderator_structure == "Continuous") {
          ml_g = ml_l  # Regression for DV
          ml_m = ml_l  # Regression for Treatment
          method = "dml2"
          score = "IV-type"
        } else if (declared_dv_type == "Continuous" & moderator_structure == "Binomial") {
          ml_g = ml_l  # Regression for DV
          ml_m = ml_m  # Classification for Treatment
          method = "dml2"
          score = "partialling out"
        } else if (declared_dv_type == "Binomial" & moderator_structure == "Continuous") {
          ml_g = ml_m  # Classification for DV
          ml_m = ml_l  # Regression for Treatment
          method = "dml2"
          score = "IV-type"
        } else if (declared_dv_type == "Binomial" & moderator_structure == "Binomial") {
          ml_g = ml_m  # Classification for DV
          ml_m = ml_m  # Classification for Treatment
          method = "dml2"
          score = "ATE"
        } else {
          stop("Invalid input: declared_dv_type and moderator_structure should be 'Continuous' or 'Binomial'.")
        }

        return(list(ml_g = ml_g, ml_m = ml_m, method = method, score = score))
      }


    } # Assigning DoubleML Parameters Given Structures of DV & Treatment (Moderator)

    model_settings <- assign_doubleML_models(declared_dv_type, moderator_structure) # Run Above Function


    dml_data_temp <- DoubleMLData$new(data = full_data,
                                      y_col = declared_dv,
                                      d_cols = declared_treatments[moderator],
                                      x_cols = declared_covariates) # Convert to DoubleML Bindings


    if (moderator_structure == "Continuous") {
      dml_model <- DoubleMLPLR$new(
        data = dml_data_temp,
        ml_g = model_settings$ml_g,
        ml_m = model_settings$ml_m,
        dml_procedure = model_settings$method,
        score = model_settings$score
      )
    } else {
      dml_model <- DoubleMLIRM$new(
        data = dml_data_temp,
        ml_g = model_settings$ml_g,
        ml_m = model_settings$ml_m,
        dml_procedure = model_settings$method,
        score = model_settings$score
      )
    }

    dml_model$fit()

  }




}
