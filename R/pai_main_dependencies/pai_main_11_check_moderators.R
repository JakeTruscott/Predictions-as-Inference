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
