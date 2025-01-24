push_pred <- function(mod, var, stepper, Z, outcome_type, outcome_var, parameters){

  if (outcome_type == 'Continuous'){
    Z[[var]] <- Z[[var]]  + stepper
    pred <- predict(mod, Z)
    true <- Z[[outcome_var]]
    diff <- pred - true
    return(diff)
  } else {

    Z[[var]] <- Z[[var]]  + stepper
    pred <- predict(mod, Z)
    true <- Z[[outcome_var]]
    onecount <- length(which(pred=='1'))/length(true)
    acc <- length(which(pred==true))/length(true)
    return(c(onecount, acc))
  }

} #Predictive Accuracy from Steps
