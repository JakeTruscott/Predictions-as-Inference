################################################################################
# Hainmeuller Replication -- PAI
# Jake S. Truscott
# Updated May 2025
################################################################################

################################################################################
# Packages & Libraries
################################################################################
library(dplyr)


################################################################################
# Merge Hainmueller
################################################################################

papers <- list.files(file.path('misc', 'hainmueller_replication', 'Data'), full.names = T)
paper_names <- gsub('.*Data\\/', '', papers)
hainmueller_list <- get(load('hainmueller_replication/data/hainmueller_list.rdata'))
papers <- papers[paper_names %in% names(hainmueller_list)]
hainmueller <- list()

for (i in 1:length(papers)){

  temp_paper <- papers[i] # Grab Temp Paper
  temp_paper_name <- gsub('.*Data\\/', '', temp_paper)

  temp_meta <- hainmueller_list[[as.character(temp_paper_name)]]

  for (rep in 1:length(temp_meta)){

    temp_rep <- temp_meta[[rep]] # Recover Rep
    temp_rep_name <- names(temp_meta[rep]) # Recover Name
    hainmueller[[as.character(temp_rep_name)]] <- list() # Add Empty List to Combined

    {

      temp_data_path <- file.path('misc', 'hainmueller_replication', 'Data', temp_paper_name, temp_rep$data) # Recover Data Path
      data <- foreign::read.dta(temp_data_path, convert.factors = F) # Load Data

      if (!is.null(temp_rep$custom)){
        assign("data", data)
        for (line in temp_rep$custom) {
          eval(parse(text = line))
        }
      }  # Custom Data Transformation (If Indicated)


      data[] <- lapply(data, function(x) {
        attr(x, "label") <- NULL
        return(x)
      })
      temp_data <- as.data.frame(data)


      dv_structure <- ifelse(length(unique(temp_data[[as.character(temp_rep$Y)]])) <= 2, 'Binomial', 'Continuous')

    } # Recover Data

    {

      temp_formula <- if (!is.null(temp_rep$Z)){
        paste(temp_rep$Y, '~',  paste(c(temp_rep$D, temp_rep$X, temp_rep$Z), collapse = " + "))
      } else {
        paste(temp_rep$Y, '~',  paste(c(temp_rep$D, temp_rep$X), collapse = " + "))
      }

      if (!is.null(temp_rep$FE)){
        temp_formula <- paste(c(temp_formula, paste0('factor(', temp_rep$FE, ')')), collapse = " + ")
      }

      multiplicative_formula <- if (!is.null(temp_rep$Z)){
        paste(temp_rep$Y, "~",
              paste(c(paste(temp_rep$X, temp_rep$D, sep = ":"),
                      temp_rep$Z), collapse = " + "))
      } else {
        paste(temp_rep$Y, "~",
              paste(c(paste(temp_rep$X, temp_rep$D, sep = ":")), collapse = " + "))
      }

      if (!is.null(temp_rep$FE)){
        multiplicative_formula <- paste(c(multiplicative_formula, paste0('factor(', temp_rep$FE, ')')), collapse = ' + ')
      }

    } # Recover Base Formula (LM + Multiplicative)


    hainmueller[[as.character(temp_rep_name)]] <- list(dv_structure = dv_structure,
                                                       formula = temp_formula,
                                                       multiplicative_formula = multiplicative_formula,
                                                       data = temp_data,
                                                       meta = temp_rep)



  } # For Each Rep in Paper

  message(' ---- Completed ', i, ' of ', length(papers))


}

save(hainmueller, file = 'hainmueller_replication/data/hainmueller_meta.rdata')

################################################################################
# Load Hainmueller
# Run Through PAI --> Export
################################################################################

source('R/pai_main.R') # Source PAI
load('hainmueller_replication/data/hainmueller_meta.rdata') # Load Hainmueller Files w/ Meta

output_folder <- file.path('hainmueller_replication', 'pai_runs')

for (i in 40:length(hainmueller)){

  temp_rep <- hainmueller[[i]] # Grab Temp Rep
  temp_rep_name <- names(hainmueller[i]) # Get Temp Name
  temp_output_path <- file.path(output_folder, paste0(temp_rep_name, '.rdata')) # Compile Temp Output Path

  if (file.exists(temp_output_path)){
    message('Already Completed ', temp_rep_name, '.... Moving On')
    next
  }

  if (is.null(temp_rep$meta$FE)){
    temp_rep$meta$FE <- NULL
  }


  {

    if (grepl('vernby', temp_rep_name, ignore.case = T)){

      data <- temp_rep$data

      school1<-data[data$term==1,c("term","code","schooling","socialvard")]
      school2<-data[data$term==2,c("term","code","schooling","socialvard")]
      school1<-school1[order(school1$code),]
      school2<-school2[order(school2$code),]
      school.diff<-school2$schooling - school1$schooling
      socialvard.diff<-school2$socialvard - school1$socialvard
      diff.dvs<-cbind.data.frame(school.diff= school.diff, socialvard.diff= socialvard.diff, code=school1$code  )
      first<-data[data$term==1, c("code","Taxbase2","pop","manu")]
      first<-first[order(first$code),]
      second<-data[data$term==2, c("code","Taxbase2","pop","manu")]
      second<-second[order(second$code),]
      names(second)<-c("code","Taxbase2.2","pop.2","manu.2")

      data2<-cbind.data.frame(diff.dvs, first, second)
      noncitvotsh <-data$noncitvotsh[data$term==2]
      noncit5 <-data$noncit5[data$term==2]
      noncit5<-noncit5[order(data$code[data$term==2])]
      noncit15 <-data$noncit15[data$term==2]
      noncit15<-noncit15[order(data$code[data$term==2])]
      noncitvotsh <-data$noncitvotsh[data$term==2]
      noncitvotsh <-noncitvotsh[order(data$code[data$term==2])]
      int<-data$noncitvotsh*data$noncit15
      int<-int[order(data$code)]
      int<-int[seq(2, length(int),2)]
      data2$int<-int
      int2<-data$noncitvotsh*data$noncit5
      int2<-int2[order(data$code)]
      int2<-int2[seq(2, length(int2),2)]
      data2$int2<-int2
      data2$noncit5<-noncit5
      data2$noncit15<-noncit15
      data2$noncitvotsh<-noncitvotsh

      data <- data2
      temp_rep$data <- data

    }


  } # Vernby Special Data Pre-Processing

  {
    vars_to_keep <- unlist(Filter(Negate(is.null), list(temp_rep$meta$Y, temp_rep$meta$D, temp_rep$meta$X, temp_rep$meta$Z, temp_rep$meta$FE)))
    temp_rep$data <- temp_rep$data[names(temp_rep$data) %in% vars_to_keep]

  } # Filter Data to Necessary Vars

  temp_rep_lm <- lm(temp_rep$formula, data = temp_rep$data) # Basic LM
  temp_rep_multiplicative <- lm(temp_rep$multiplicative_formula, data = temp_rep$data) # Multiplicative LM

  temp_rep$data <- na.omit(temp_rep$data)

  {

    if (!is.null(temp_rep$meta$FE)){

      for (factor in 1:length(temp_rep$meta$FE)) {

        original_var <- temp_rep$data[[as.character(temp_rep$meta$FE[factor])]]
        orig_type <- class(original_var)
        temp_FE_values <- factor(original_var)
        count_table <- table(temp_FE_values)
        sparse_levels <- names(count_table[count_table < 10]) # Needs to appear fewer than 10 times

        if (length(sparse_levels) >= 1) {
          if (orig_type %in% c("character", "factor")) {
            new_var <- as.character(temp_FE_values)
            new_var[new_var %in% sparse_levels] <- "888"
            temp_rep$data[[as.character(temp_rep$meta$FE[factor])]] <- new_var
          } else if (orig_type %in% c("numeric", "integer")) {
            new_var <- as.character(temp_FE_values)
            new_var[new_var %in% sparse_levels] <- "888"
            temp_rep$data[[as.character(temp_rep$meta$FE[factor])]] <- as.numeric(new_var)
          }
        }

      } # For Each FE -- Replace Sparse (If Any)

    } # If Not Null FE


  } # Sparse FE Factor Check (Replaces Sparse w/ 888 or '888')


  message('Beginning PAI For ', temp_rep_name)

  temp_run <- pai(data = temp_rep$data,
              model = 'parRF',
              outcome = as.character(temp_rep$meta$Y),
              predictors = names(temp_rep$data)[!names(temp_rep$data) %in% c(temp_rep$meta$Y, if (!is.null(temp_rep$meta$FE)) temp_rep$meta$FE else NULL)],
              factors = temp_rep$meta$FE,
              cores = 5,
              assign_factors = NULL,
              drop_vars = NULL,
              sparse_variable_threshold = nrow(temp_rep$data)/10,
              drop_sparse_vars = F,
              placebo_iterations = 50) # Run RF w/ 10 Cores

  hainmueller_run_combined  <- list() # Empty List
  hainmueller_run_combined[['run']] <- temp_run # Export Temp Run
  hainmueller_run_combined[['lm']] <- temp_rep_lm # Export LM
  hainmueller_run_combined[['multiplicative_lm']] <- temp_rep_multiplicative # Export Multiplicative LM

  temp_output_path <- file.path(output_folder, paste0(temp_rep_name, '.rdata')) # Compile Temp Output Path
  save(hainmueller_run_combined, file = temp_output_path) # Export


}

################################################################################
# Append Meta...
################################################################################

pai_runs <- list.files('hainmueller_replication/pai_runs', full.names = T)

for (i in 1:length(pai_runs)){

  temp_run <- get(load(pai_runs[i]))
  temp_run_name <- gsub('.*\\/', '', gsub('\\.rdata', '', pai_runs[i]))
  temp_run_meta <- hainmueller[[as.character(temp_run_name)]]$meta
  temp_run$meta <- temp_run_meta
  save(temp_run, file = pai_runs[i])

  if (i %% 5 == 0){
    message('Completed ', i, ' of ', length(pai_runs))
  }

} # Append Meta Info...


################################################################################
# PAI Diagnostics
################################################################################

pai_runs <- list.files('hainmueller_replication/pai_runs', full.names = T)
source('R/pai_diagnostic.R') # Source PAI Diagnostic


for (i in 1:length(pai_runs)){

  temp_run <- get(load(pai_runs[i]))
  temp_run_meta <- temp_run$meta
  temp_run <- temp_run$run
  temp_pai_name <- gsub('.*pai_runs\\/', '', gsub('\\.rdata', '', pai_runs[i]))

  message('Beginning ', temp_pai_name)

  temp_diagnostic <- list()

  {

    temp_placebo <- pai_diagnostic_retrieval(output = temp_run,
                                             diagnostic = 'placebo',
                                             type = 'figure')
    temp_diagnostic[['placebo']] <- temp_placebo

  } # Placebo

  message(' --- Completed Placebo')

  {

    temp_push <- pai_diagnostic_retrieval(output = temp_run,
                                          diagnostic_type = 'push')


    temp_diagnostic[['push']] <- temp_push

  } # Push Runs

  message(' --- Completed Push')

  {

    output <- temp_run

    temp_diagnostic[['bootstraps']] <- list()
    temp_diagnostic[['bootstraps']][['temp_bootstrap_data']] <- output$diagnostics$bootstrap$bootstrap_output
    temp_diagnostic[['bootstraps']][['temp_bootstrap_distribution']] <- output$diagnostics$bootstrap$bootstrap_distribution

    temp_diagnostic[['bootstraps']][['variable_figures']] <- list()

    temp_vars <- names(output$diagnostics$bootstrap$variable_figures)

    for (var in 1:length(temp_vars)){

      temp_diagnostic[['bootstraps']][['variable_figures']][[as.character(temp_vars[var])]] <- output$diagnostics$bootstrap$variable_figures[[as.character(temp_vars[var])]]

    }


  } # Bootstraps

  message(' --- Completed Bootstraps')

  {

    output <- temp_run

    summary_diagnostics <- list()

    summary_diagnostics[['Performance Metrics']] <- data.frame(output$declared_model$results)


    importance <- tryCatch({
      varImp(output$declared_model)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })

    # Check if importance was successfully retrieved
    if (!is.null(importance)) {
      importance <- as.data.frame(importance$importance[1])
      names(importance) <- 'Importance'
      summary_diagnostics[['Variable Importance']] <- importance
    }

    predictions <- predict(output$declared_model, newdata = output$parameters$test_set)

    if (output$parameters$outcome_type == 'Binomial'){
      true_labels <- data.frame(output$parameters$test_set)[[output$parameters$outcome]]
      true_labels <- as.factor(true_labels)
      predictions <- factor(predictions, levels = levels(true_labels))
      conf_matrix <- confusionMatrix(predictions, true_labels)
      summary_diagnostics[['Confusion Matrix']] <- conf_matrix
      summary_diagnostics['Precision'] <- conf_matrix$byClass["Precision"]
      summary_diagnostics['Recall'] <-  conf_matrix$byClass["Recall"]
      summary_diagnostics['F1'] <-  conf_matrix$byClass["F1"]

      #final_model <- output$declared_model
      #train_data <- data.frame(output$parameters$train_set, check.names = F)
      #trainPred <- predict(final_model, newdata = train_data)
      #in_sample_accuracy <- sum(trainPred == train_data$direction) / nrow(train_data)
      #summary_diagnostics['In-Sample Accuracy'] <- in_sample_accuracy


    } else {

      mse <- mean((data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions)^2)
      rmse <- sqrt(mse)
      mae <- mean(abs(data.frame(output$parameters$test_set)[[output$parameters$outcome]] - predictions))

      summary_diagnostics[['Mean Squared Error (MSE)']] <- mse
      summary_diagnostics[['Root Mean Squared Error (RMSE)']] <- rmse
      summary_diagnostics[['Mean Absolute Error (MAE)']] <- mae
    }


    temp_diagnostic[['summary']] <- summary_diagnostics


  } # Summary

  message(' --- Completed Summary')

  {

    temp_interaction <- c(temp_run_meta$D, temp_run_meta$X)
    temp_full_data <- temp_run$parameters$full_data
    D <- temp_interaction[1]
    X <- temp_interaction[2]

    compute_interaction_AME <- function(model, full_data, X, D,
                                        quantiles = seq(0, 1, 0.1),
                                        n_boot = 200, conf_level = 0.95) {
      X <- as.character(X)
      D <- as.character(D)

      # Robust binary check
      is_binary <- function(v) {
        u <- unique(v[!is.na(v)])
        # Accept numeric 0/1, logical TRUE/FALSE, or strings "0"/"1"
        all_vals <- c(0, 1, TRUE, FALSE, "0", "1")
        length(u) == 2 && all(u %in% all_vals)
      }

      X_binary <- is_binary(full_data[[X]])
      D_binary <- is_binary(full_data[[D]])

      # Debug prints
      #cat("Unique values of D:\n")
      #print(sort(unique(full_data[[D]])))
      #cat("Is D binary? ", D_binary, "\n")

      # Calculate delta for X if continuous
      delta_X <- if (!X_binary) 0.1 * sd(full_data[[X]], na.rm = TRUE) else NA

      get_ame <- function(data, d_val) {
        data_low <- data
        data_high <- data

        # Set moderator D to fixed value
        data_low[[D]] <- d_val
        data_high[[D]] <- d_val

        # Perturb X variable
        if (X_binary) {
          data_low[[X]] <- 0
          data_high[[X]] <- 1
        } else {
          data_low[[X]] <- data_low[[X]] - delta_X
          data_high[[X]] <- data_high[[X]] + delta_X
        }

        # Predict
        pred_low <- predict(model, newdata = data_low)
        pred_high <- predict(model, newdata = data_high)

        # Handle classification: get probability for positive class
        if (is.factor(pred_low) || is.matrix(pred_low)) {
          pred_low <- predict(model, newdata = data_low, type = "prob")[, 2]
          pred_high <- predict(model, newdata = data_high, type = "prob")[, 2]
        }

        mean(pred_high - pred_low, na.rm = TRUE)
      }

      # Get moderator values based on binary/continuous
      if (D_binary) {
        vals <- unique(na.omit(full_data[[D]]))
        # Round numeric values to 0 or 1 to be safe (for floats like 0.999999)
        if (is.numeric(vals)) {
          vals <- sort(round(vals))
        } else {
          vals <- sort(vals)
        }
        d_vals <- vals
      } else {
        d_vals <- quantile(full_data[[D]], probs = quantiles, na.rm = TRUE)
      }

      results <- lapply(d_vals, function(d_val) {
        point_est <- get_ame(full_data, d_val)

        boot_vals <- replicate(n_boot, {
          boot_data <- full_data[sample(nrow(full_data), replace = TRUE), ]
          tryCatch(get_ame(boot_data, d_val), error = function(e) NA)
        })

        alpha <- 1 - conf_level
        lower <- quantile(boot_vals, probs = alpha / 2, na.rm = TRUE)
        upper <- quantile(boot_vals, probs = 1 - alpha / 2, na.rm = TRUE)

        data.frame(D = d_val, AME_X = point_est, CI_lower = lower, CI_upper = upper)
      })

      result_df <- do.call(rbind, results)

      if (!D_binary) {
        # Add quantile names if continuous
        result_df$Quantile <- names(d_vals)
      }

      rownames(result_df) <- NULL
      return(result_df)
    }


    result <- compute_interaction_AME(
      model = temp_run$declared_model,
      full_data = temp_run$parameters$full_data,
      X = temp_run_meta$X,
      D = temp_run_meta$D
    )


  ame <- result %>%
    ggplot(aes(x = D, y = AME_X)) +
    geom_rect(aes(xmin = D - 0.01, xmax = D + 0.01, ymin = CI_lower, ymax = CI_upper), colour = 'black', fill = 'grey75') +
    geom_point(size = 4, shape = 21, colour = 'black', fill = 'white') +
    labs(x = paste0('\n', as.character(D), ' (D)'),
         y = paste0(as.character(X), ' (X)\n')) +
    theme_minimal() +
    theme(panel.background = element_rect(size = 1, colour = 'black', fill = NA),
          axis.text = element_text(size = 14, colour = 'black'),
          axis.title = element_text(size = 16, colour = 'black'))


  temp_diagnostic[['AME_interaction']] <- ame


  } # Marginal Effects Interaction

  message(' --- Completed AMEs from Interaction')


  temp_output_path <- file.path('hainmueller_replication', 'pai_diagnostics', paste0(temp_pai_name, '.rdata'))
  save(temp_diagnostic, file = temp_output_path)

  message('Completed ', temp_pai_name, ' --- ', i, ' of ', length(pai_runs))

}

