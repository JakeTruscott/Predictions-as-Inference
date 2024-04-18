################################################################################
# Predictions as Inference - Mood Information (Johnson & Strother)
# Code Developed by Jake S. Truscott
# Updated March 2024
################################################################################


################################################################################
#Load Packages
################################################################################
library(randomForest); library(doParallel); library(caret); library(parallel); library(rlist); library(dplyr); library(gridExtra); library(gridtext); library(grid); library(doSNOW); library(patchwork); library(stringr); library(cowplot); library(xgboost); library(foreign)


################################################################################
#Load Data From Dropbox
#Put Data w/ Mood Info Into Filtered List
################################################################################

dropbox <- "E:/Strother_Johnson/data" #Dropbox Directory
load(file.path(dropbox, 'permutation-dfs.rdata')) #Load Permutations DF
load(file.path(dropbox, 'Chapter3-Models/Cases-FullData.rdata')) #Load Permutations DF

scotus_data <- list() #Initialize Empty List

for (i in 1:length(perms)){

  temp_justice <- names(perms[i]) #Get Justice Name
  temp_data <- perms[i][[1]] #Get Data (w/ Mood Info)

  scotus_data[as.character(temp_justice)] <- temp_data

  if(i %% 2 == 0){
    message('Completed ', i, ' of ', length(perms))
  }


} #Put Data w/ Mood into Filtered List

rm(list=setdiff(ls(), c('scotus_data', 'info'))) #Clean Global Env.

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
model_types <- c('parRF', 'xgbTree', 'adaboost')

################################################################################
#Run PAI Estimates on SCOTUS Data
################################################################################

already_completed <- list.files(output_directory)

scotus_data <- scotus_data[!names(scotus_data) %in% already_completed]

scotus_data <- info$modlist$with.dat

scotus_data <- bind_rows(scotus_data$test, scotus_data$train)

scotus_data <- list('Cases' = scotus_data)

for (i in 1:length(scotus_data)){

  #temp_data <- scotus_data[[i]]
  temp_data <- scotus_data[[i]]

  temp_justice <- names(scotus_data[i]) #Get Temp Justice
  start_message <- sprintf("\033[38;5;208mBeginning Process for \033[0m\033[38;5;15m%s\033[0m", temp_justice) #Compile Start Message
  cat(start_message, '\n') #Print Start Message

  justice_directory <- file.path(output_directory, temp_justice) #Create New Folder Directory for Justice
  if (!dir.exists(justice_directory)){
    dir.create(justice_directory)
  } #Create New Justice-level Folder (If Does Not Already Exist...)


  for (mt in model_types){

    model_level_directory <- paste0(justice_directory, '/', paste0(temp_justice, "_", mt, '.rdata')) #Create Model-Level Directory Output

    if(file.exists(model_level_directory)){
      message('      ', mt, ' Already Completed...\n')
      next
      } #Skip If Model Completed in Earlier Iterations

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


################################################################################
# Hainmueller Replication
################################################################################

rep_path = "C:/Users/Jake Truscott/OneDrive - purdue.edu/Active Research/SJT_R_Package/Paper Materials/Hainmueller_Replication/Replication/Code"

rep_files <- list.files(rep_path, full.names = T)
rep_files <- rep_files[grepl('rep\\_', rep_files)]
rep_files <- rep_files[!rep_files %in% c('rep_bodea_io_2015', 'rep_clark_2006', 'rep_clark_2014', 'rep_hellwig_2007', 'rep_pelc_2011', 'rep_williams_2011', 'rep_chapman_2009')]

hainmueller_params <- list()

for (i in 1:length(rep_files)){

  temp_file <- readLines(rep_files[i])
  temp_file <- gsub('"', "'", temp_file)

  temp_params <- list()
  temp_params['data'] <- grep("^d<-", temp_file, value = TRUE)
  temp_params['y'] = grep("^Y=", temp_file, value = TRUE)

  temp_params


}



aklin <- na.omit(read.dta(file.path(path,"Data/Aklin_AJPS_2013/rep_aklin_2013.dta")))

aklin_test <- pai_main(data = aklin,
                       outcome = 'drenew_capacity_nh_share',
                       predictors = c('left_to_right', 'right_to_left', 'linnovation_x_oil', 'oilcrude_price2007dollar_bp', 'lrenewpc', 'left_executive', 'rigth_executive', 'election_year', 'renewablecapacity_3yr_average', 'hydronuclear_3yr', 'year', 'traditional_electricity_share', 'year_dummy'),
                       factors = NULL,
                       assign_factors = c(TRUE, 4),
                       interactions = NULL,
                       list_drop_vars = FALSE,
                       drop_vars = NULL,
                       ml = c('parRF', 5, 100, 5),
                       custom_tc = F,
                       seed = 1234)


diagnostic_test <- pai_diagnostic(pai_object = aklin_test)

diagnostic_test$figures$placebo
