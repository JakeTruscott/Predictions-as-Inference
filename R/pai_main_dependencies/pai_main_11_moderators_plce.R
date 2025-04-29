library(DoubleML)

data <- parameters$full_data
data <- data[!is.na(data$sal.lat.issuemood),]

data <- data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
  select(where(~ all(!is.na(.)))) %>%
  select(where(~ all(!is.character(.)))) %>%
  mutate(interaction = sal.lat.issuemood * issuemood)

outcome_col <- data[names(data) %in% parameters$outcome]
outcome_col <- as.numeric(unlist(c(outcome_col)))
predictors_col <- data[names(data) %in% parameters$predictors]
predictors_col <- data[!names(data) %in% 'sal.lat.issuemood']
moderator <- unlist(c(data[names(data) == 'sal.lat.issuemood']))


test <- PLCE::plce(y = outcome_col, treat = moderator, X = predictors_col, num.fit = 1)


plce_moderator_analysis <- funtion(output){

  parameters <- output[['parameters']] # Retrieve Parameters Object
  temp_data <- parameters[['full_data']] # Retrieve Full Dataframe
  #declared_moderators <- parameters[['moderators']] # Retrieve Moderators from Declared Parameters
  moderators_return_object <- list() # Empty List To Store Moderator Output

  declared_moderators <- c('issuemood*sal.lat.issuemood')

  message('Beginning Analysis of Potential Moderators -- ', length(declared_moderators), ' Declared')

  {

    factor_vars <- temp_data %>%
      select(where(is.factor)) # Check Factor Vars
    character_vars <- temp_data %>%
      select(where(is.character)) # Check Character Vars
    vars_needing_scale_adjustment <- unique(c(names(factor_vars), names(character_vars))) # Get Factor & Character Var Names

    if (length(vars_needing_scale_adjustment) >= 1){
      message('Note: PLCE Requires Numeric and Integer Structure \nAdjusting \033[97m(', length(vars_needing_scale_adjustment),')\033[0m Factored and Character Variables to Numeric Structure\n')

      formatted_vars_needing_scale_adjustment <- lapply(seq_along(vars_needing_scale_adjustment), function(i) {
        line_break <- '  ' # Double line break every 3rd item
        paste0("(", i, ") \033[97m", vars_needing_scale_adjustment[i], "\033[0m", '  ')
      })

      message(formatted_vars_needing_scale_adjustment)



    } # Update Message If Factor/Character Scales Need Adjustment


    variables_to_drop <- temp_data %>%
      select(where(~ all(is.na(.)))) # Variables Needing to Be Dropped b/c NA
    variables_to_drop <- names(variables_to_drop) # Retrieve Names


    if (length(variables_to_drop) > 0){

      message('Note: Identified ', length(variables_to_drop), ' Variables in Data Where All Values Are NA')
      message('Dropping: ', paste(variables_to_drop, collapse = ', '))

      temp_date <- temp_data[!names(temp_data) %in% variables_to_drop]

    } # Update Message & Removal of NA Columns (If Necessary)

    temp_data <- suppressMessages(suppressWarnings(
      temp_data %>%
        mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
        mutate(across(where(is.character), ~ as.numeric(.))) %>%
        select(where(~ all(!is.na(.))))
    ))



  } # Clean Data Before PLCE


  for (i in 1:length(declared_moderators)){

    temp_moderator <- declared_moderators[i] # Retrieve Temp Moderator
    temp_moderator <- unlist(strsplit(temp_moderator, split = '(\\*|\\:)')) # Split Temp Moderator Vars
    temp_outcome_col <- temp_data[names(temp_data) %in% parameters$outcome] # Retrieve DV
    if (!is.numeric(temp_outcome_col)){
      temp_outcome_col <- suppressWarnings(suppressMessages(as.numeric(as.character(temp_outcome_col))))
    } # Convert Factor or Character DV to Numeric
    predictors_col <- temp_data[names(temp_data) %in% parameters$predictors]
    predictors_col <- predictors_col[!names(predictors_col) %in% temp_moderator[2]] # Removes Moderating Term from Covariates
    moderator <- unlist(c(temp_data[names(temp_data) == temp_moderator[2]])) # Retrieves Temp Moderator Column

    temp_plce <- suppress_message(PLCE::plce(y = outcome_col, treat = moderator, X = predictors_col, num.fit = 1, sens = T)) # Run Temp PLCE

    temp_plce_return_object <- list() # Empty Object to Store Temp Output & Summary Info

    temp_plce_return_object[['plce_output']] <- temp_plce # Raw Output Object
    temp_plce_return_object[['sensitivity']] <- data.frame(prediction_residuals = temp_plce$treat.res[,1],
                                                           outcome_regression_residuals = temp_plce$treat.res[,2],
                                                           combined_prediction = temp_plce$treat.res[,1],
                                                           combined_outcome_regression = temp_plce$treat.res[,2]) %>%
      tidyr::pivot_longer(cols = c(prediction_residuals, outcome_regression_residuals, combined_prediction, combined_outcome_regression), names_to = 'type') %>%
      mutate(type = case_when(
        type == 'prediction_residuals' ~ 'Prediction',
        type == 'outcome_regression_residuals' ~ 'Regression',
        type == 'combined_prediction' ~ 'Prediction (combined)',
        type == 'combined_outcome_regression' ~ 'Regression (combined)')) %>%
      mutate(graph_number = case_when(
        !grepl('(combined)', type) & grepl('Prediction', type) ~ 'Prediction',
        !grepl('(combined)', type) & grepl('Regression', type) ~ 'Regression',
        grepl('(combined)', type) ~ 'Combined')) %>%
      mutate(type = gsub(' \\(combined\\)', '', type),
             type = factor(type, levels = c('Prediction', 'Regression')),
             graph_number = factor(graph_number, levels = c('Prediction', 'Regression', 'Combined'))) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = value)) +
      geom_histogram(aes(fill = type), colour = 'gray5', bins = 30, alpha = 1/3) +
      geom_density(colour = 'gray5', linewidth = 1.05) +
      geom_vline(xintercept = 0, linetype = 2, alpha = 0.75, colour = 'black') +
      geom_hline(yintercept = 0, linetype = 1, colour = 'black') +
      facet_wrap(~graph_number, nrow = 3) +
      theme_minimal() +
      scale_fill_grey() +
      labs(x = '\nValue',
           y = 'Density\n') +
      theme_minimal() +
      theme(legend.position = 'none',
            panel.border = element_rect(size = 1, colour = 'black', fill = NA),
            axis.text = element_text(size = 12, colour = 'black'),
            axis.title = element_text(size = 14, colour = 'black'),
            strip.text = element_text(size = 12, colour = 'black'),
            strip.background = element_rect(size = 1, colour = 'black', fill = 'gray')) # x1: Unexplained treatment variation after adjusting for covariates.x2: Residualized treatment used in the final causal effect estimation.Note: If either x1 or x2 shows strong skew or clumping at zero, it suggests positivity violations or limited treatment variability.

    temp_plce_return_object[['effect_heterogeneity']] # Make Figure to show heterogeneity

temp_plce_return_object$residual_diagnostic


  } # For Each Moderator -- Run the Analysis
} # Function to Initialize Ratkovic-Style DML Analysis Using PLCE
