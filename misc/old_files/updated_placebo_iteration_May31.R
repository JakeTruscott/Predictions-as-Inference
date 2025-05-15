placebo_shuffle <- function(declared_model, parameters, output){

  set.seed(parameters$seed) # Set Random Seed for Reproducibility
  test_data = data.frame(output$parameters$test_set, check.names = F) # Load Test Data

  n=dim(test)[2]
  r=dim(test)[1]
  namer <- names(test)
  resamps <- preds <- NULL
  base.pred <- predict(declared_model, test_data)
  l <- length(base.pred)
  base.pred <- length(which(base.pred==test_data[[parameters$outcome]]))/length(base.pred)
  for (i in 1:samps){
    resamps <- cbind(resamps, sample(seq(r)))
  }
  namelist <- NULL
  preds <- NULL


  if (parameters$list_drop_vars == TRUE){
    groups = parameters$drop_vars
  } else {
    if(is.null(parameters$interactions)){
      groups <- c(unlist(parameters$predictors))
    } else {
      groups <- c(unlist(parameters$predictors), unlist(parameters$interactions))
    }  # Get Variables

  } # Assign Variables as 'Groups' depending on parameters$list_drop_vars

  for (g in 1:length(groups)){
    group_var = names(groups[g])
    p2 <- NULL
    for (i in 1:parameters$placebo_iterations){
      df <- test
      for (j in groups[g]){
        df[,j] <- df[resamps[,i],j]
      }
      pred <- predict(mod, df)
      p <- length(which(pred==df[[parameters$outcome]]))/length(base.pred)
      p2 <- c(p2, p)
    }
    preds <- cbind(preds, p2)

    if (parameters$list_drop_vars == TRUE){
      message('Completed Placebo Iterations for ', group_var)
    } else {
      message('Completed Placebo Iterations for ', group_var)
    }

  }

  pred.df <- as.data.frame(preds)
  names(pred.df) <- names(groups) #namelist
  preds_list <- list(preds = pred.df, base = base.pred, pred.num=l,
                     means = apply(pred.df, 2, mean)/l,
                     mins = apply(pred.df, 2, min)/l,
                     upper = apply(pred.df, 2, function(x) quantile(x, .95))/l)

  placebos_summary <- data.frame()

  for (i in 1:length(preds_list$preds)){

    temp_group_name <- names(preds_list$preds)[i]
    temp_base <- preds_list$base[1]
    temp_mean <- preds_list$means[i]
    temp_min <- preds_list$mins[i]
    temp_max <- preds_list$upper[i]

    temp_combined <- data.frame(variable = temp_group_name,
                                var_id = i,
                                base = temp_base,
                                base_zero = 0,
                                mean = temp_mean,
                                lwr = temp_min,
                                upr = temp_max)

    placebos_summary <- bind_rows(placebos_summary, temp_combined)

  }

  placebos_final <- data.frame()

  for (i in 1:nrow(placebos_summary)){

    temp_row <- placebos_summary[i,]
    temp_row <- temp_row %>%
      mutate(variable = variable,
             var_id = var_id,
             base = base,
             mean = mean - base,
             lwr = lwr - base,
             upr = upr - base)

    placebos_final <- bind_rows(placebos_final, temp_row)

  }

  placebos <- list()
  placebos[['placebo_summary']] <- placebos_final
  placebos[['all_returned']] <- preds_list


  return(placebos)

}

