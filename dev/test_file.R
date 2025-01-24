

library(caret); library(dplyr); library(stringr); library(doParallel); library(broom); library(cowplot); library(grid); library(gridExtra); library(doSNOW); library(gridtext); library(patchwork); library(randomForest); library(rlist); library(xgboost); library(ggplot2); library(ggridges)


load("E:/PAI/Strother_Johnson/data/Chapter3-Models-BIG/permutation-dfs.RData")

p <- perms[[1]] # Cases w/ Issue Mood


#js <- p[[3]]
js <- p[[1]]
js <- data.frame(js)

sparse_values_check <- function(data){
  sparse_values_list <- list()

  for (var in names(data)){

    if (!is.factor(data[[var]])){
      next
    }

    freq_table <- table(data[[var]])
    rare_values <- names(freq_table[freq_table < 30])

    if (length(rare_values) > 0){
      sparse_values_list[[var]] <- rare_values
    }

  }

  return(sparse_values_list)

}

rare_values <- sparse_values_check(js)

for (var in names(js)) {

  if (!var %in% names(rare_values)) {
    next
  }

  values_to_amend <- rare_values[[var]]

  if (is.factor(js[[var]])) {

    js[[var]] <- as.character(js[[var]])  # Convert factor to character

    js[[var]][js[[var]] %in% values_to_amend] <- 888  # Replace values

    js[[var]] <- factor(js[[var]])  # Convert back to factor
  } else {

    js[[var]][js[[var]] %in% values_to_amend] <- 888 # Replace values in character or numeric vectors
  }
}


moods <- grep('mood', names(js))
sals <- unique(c(grep('salience', names(js)), grep('CLR', names(js)), grep('sal.', names(js)), grep('Sal.', names(js))))
lc <- grep('lcD', names(js))
jr <- grep('judRev', names(js))
lats <- grep('lat', names(js))
issues <- which(names(js) %in% c('issueArea', "issuecluster", "issueFactor",
                                 "iA.2", "iA.1", "iA.7", "iA.8", "adminAction", "adminActionState"))
amicis <- which(names(js) %in% c("amaff", "amrev", 'totam', 'wlf', 'chamber', 'sg', 'aclu'))
ideo <- which(names(js) %in% c('scmed', 'mqmed', 'mqmean'))
groups <- list(moods=moods, sals=sals, lc=lc, jr=jr, lats=lats, issues=issues, amicis=amicis, ideo=ideo)

drop_var_list <- list()

for (i in 1:length(groups)){

  temp_group <- groups[[i]]
  temp_group_name <- names(groups[i])

  temp_group_list <- c()

  for (g in temp_group){

    colname = names(js[g])
    temp_group_list <- c(temp_group_list, colname)
  }

  drop_var_list[[as.character(temp_group_name)]] <- temp_group_list

}

rm(list=setdiff(ls(), c('js', 'drop_var_list')))


data = js #Data
model = 'parRF' #Caret Model
outcome = 'direction' #DV
predictors = NULL #IVs
interactions = NULL #Interactive Terms
drop_vars = drop_var_list #Defaults to All
cores = 8 #Defaults to 1
placebo_iterations = 200 #Defaults to 10
folds = 10 #Defaults to 5
train_split = 0.8 #Defaults to 80/20
custom_tc = "repeats = 5" #Defaults to Basic TC (3 Repeats Assigned K-Folds etc.)
assign_factors = 5 #Defaults to 3 - Change to Any Number
list_drop_vars = TRUE #Defaults to FALSE
seed = 254
drop_sparse_vars = T
save_drop_var_models = F
sparse_variable_threshold = NULL

js_test <- pai(data = js,
               model = 'parRF',
               outcome = 'direction',
               drop_vars = drop_var_list,
               cores = 8,
               placebo_iterations = 50,
               folds = 5,
               train_split = 0.8,
               custom_tc = 'repeats = 5',
               assign_factors = 5,
               list_drop_vars = T,
               seed = 123,
               drop_sparse_vars = T)


pai_diagnostic_retrieval(js_test,
                         diagnostic = 'placebo',
                         type = 'all')


pai_diagnostic_retrieval(output = js_test,
                         diagnostic = 'summary')

