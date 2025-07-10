
push_pred <- function(mod, var, stepper, Z, outcome_type) {

  Z[[var]] <- Z[[var]] + stepper

  if (outcome_type == "Continuous") {
    pred <- predict(mod, newdata = Z) # Return Pred if Continuous
    return(mean(pred, na.rm = TRUE))

  } else if (outcome_type == "Binomial") {

    prob <- predict(mod, newdata = Z, type = "prob")[, "1"] # Return Mean Prob of 1 if Binom
    return(mean(prob, na.rm = TRUE))

  } else {
    stop("Unknown outcome type. Use 'Continuous' or 'Binomial'.")
  }
}
