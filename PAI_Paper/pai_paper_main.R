################################################################################
# Predictions as Inference - Mood Information (Johnson & Strother)
# Code Developed by Jake S. Truscott
# Updated March 2024
################################################################################


################################################################################
#Load Packages
################################################################################
library(randomForest); library(doParallel); library(caret); library(parallel); library(rlist); library(dplyr); library(gridExtra); library(gridtext); library(grid); library(doSNOW); library(patchwork); library(stringr); library(cowplot); library(xgboost)


################################################################################
#Load Data From Dropbox
#Put Data w/ Mood Info Into Filtered List
################################################################################

dropbox <- "E:/Strother_Johnson/data" #Dropbox Directory
load(file.path(dropbox, 'permutation-dfs.rdata')) #Load Permutations DF

scotus_data <- list() #Initialize Empty List

for (i in 1:length(perms)){

  temp_justice <- names(perms[i]) #Get Justice Name
  temp_data <- perms[i][[1]] #Get Data (w/ Mood Info)

  scotus_data[as.character(temp_justice)] <- temp_data

  if(i %% 2 == 0){
    message('Completed ', i, ' of ', length(perms))
  }


} #Put Data w/ Mood into Filtered List

rm(list=setdiff(ls(), "scotus_data")) #Clean Global Env.

################################################################################
#Organize Mood & Other Info Into Groups
################################################################################

df <- scotus_data[[1]] #Get a Sample DF
mood <- names(df)[grepl('mood', names(df))]
issues <- c('issueArea', "issuecluster", "issueFactor",
            "iA.2", "iA.1", "iA.7", "iA.8", "adminAction", "adminActionState")
issues <- issues[issues %in% names(df)]
amicis <- c("amaff", "amrev", 'totam', 'wlf', 'chamber', 'sg', 'aclu')
amicis <- amicis[amicis %in% names(df)]
ideo <- c('scmed', 'mqmed', 'mqmean')
ideo <- ideo[ideo %in% names(df)]
lc <- names(df)[grep('lcD', names(df))]
jr <- names(df)[grep('judRev', names(df))]
lats <- names(df)[grep('lat', names(df))]
sals <- unique(c(grep('salience', names(df)), grep('CLR', names(df)), grep('sal.', names(df)), grep('Sal.', names(df))))


################################################################################
# Set Output Directory
################################################################################

output_directory <- "E:/PAI"
model_types <- c('parRF', 'xgbTree')

already_completed <- list.files(output_directory)

scotus_data <- scotus_data[!names(scotus_data) %in% already_completed]

#scotus_data <- scotus_data[4:length(scotus_data)]

for (i in 1:length(scotus_data)){

  temp_data <- scotus_data[[i]]

  temp_justice <- names(scotus_data[i]) #Get Temp Justice
  start_message <- sprintf("\033[38;5;208mBeginning Process for \033[0m\033[38;5;15m%s\033[0m", temp_justice) #Compile Start Message
  cat(start_message, '\n') #Print Start Message

  justice_directory <- file.path(output_directory, temp_justice) #Create New Folder Directory for Justice
  if (!dir.exists(justice_directory)){
    dir.create(justice_directory)
  } #Create New Justice-level Folder (If Does Not Already Exist...)


  for (mt in model_types){
    output <- capture.output({
      suppressMessages(
        suppressPackageStartupMessages(
          temp_pai <- pai_main(data = temp_data,
                               outcome = 'direction',
                               predictors = NULL,
                               factors = NULL,
                               assign_factors = c(TRUE, 4),
                               interactions = NULL,
                               list_drop_vars = TRUE,
                               drop_vars = list(Mood = mood,
                                                Issue = issues,
                                                Amici = amicis,
                                                Ideology = ideo,
                                                `Lower Court` = lc,
                                                `Judicial Review` = jr,
                                                Lateral = lats),
                               ml = c(mt, 8, 100, 5),
                               custom_tc = F,
                               seed = 1234)))}) #Run PAI Process (Will Silence Output Text...)


    model_level_directory <- paste0(justice_directory, '/', paste0(temp_justice, "_", mt, '.rdata')) #Create Model-Level Directory Output

    save(temp_pai, file = model_level_directory) #Save to Folder
    message('      ', mt, ' Completed...\n') #Print Update When Complete Each Model


  } #For mt in model_type

  end_message <- sprintf("\033[38;5;202mCompleted Process for \033[0m\033[38;5;15m%s\033[0m", temp_justice) #Compile End Message
  cat(end_message, '\n') #Print End Message

} #Main Function










