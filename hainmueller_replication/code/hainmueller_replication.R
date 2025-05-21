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
papers <- papers[!grepl('Banks_Valentino_AJPS_2012', papers, ignore.case = T)]
papers <- papers[!grepl('Bodea', papers, ignore.case = T)]
hainmueller_list <- get(load('hainmueller_replication/data/hainmueller_list.rdata'))
hainmueller <- list()

for (i in 1:length(papers)){

  temp_paper <- papers[i] # Grab Temp Paper
  temp_data_files <- list.files(papers[i], pattern = '.dta', full.names = T)[1] # Recover DTA Files

  temp_paper_name <- gsub('.*\\/', '', gsub('\\.dta', '', temp_data_files[1])) # Recover Name
  temp_paper_name <- gsub('(a$|b$)', '', temp_paper_name)
  temp_paper_meta <- hainmueller_list[[as.character(temp_paper_name)]]

  if (length(temp_paper_meta) == 0){
    message('No Meta for ', temp_paper_name)
    next
  }

  hainmueller[[as.character(temp_paper_name)]] <- list() # Set Paper-Level List in hainmueller


  temp_formula <- if (!is.null(temp_paper_meta$Z)){
   as.formula(paste(temp_paper_meta$Y, '~',  paste(c(temp_paper_meta$D, temp_paper_meta$X, temp_paper_meta$Z), collapse = " + ")))
  } else {
    as.formula(paste(temp_paper_meta$Y, '~',  paste(c(temp_paper_meta$D, temp_paper_meta$X), collapse = " + ")))
  }

  multiplicative_formula <- if (!is.null(temp_paper_meta$Z)){
    as.formula(paste(temp_paper_meta$Y, "~",
        paste(c(paste(temp_paper_meta$X, temp_paper_meta$D, sep = ":"),
            temp_paper_meta$Z), collapse = " + ")))
  } else {
    as.formula(paste(temp_paper_meta$Y, "~",
                     paste(c(paste(temp_paper_meta$X, temp_paper_meta$D, sep = ":")), collapse = " + ")))
  }

  temp_data <- haven::read_dta(temp_data_files[1]) # Pull First Paper (Shouldn't Be Multiple)
  temp_data[] <- lapply(temp_data, function(x) {
    attr(x, "label") <- NULL
    return(x)
  })
  temp_data <- as.data.frame(temp_data)
  vars_to_keep <- c(temp_paper_meta$Y, temp_paper_meta$D, temp_paper_meta$X, temp_paper_meta$Z)
  temp_data <- temp_data[names(temp_data) %in% vars_to_keep]


  dv_structure <- ifelse(length(unique(temp_data[[as.character(temp_paper_meta$Y)]])) <= 2, 'Binomial', 'Continuous')

  hainmueller[[as.character(temp_paper_name)]] <- list(dv_structure = dv_structure,
                                                       formula = temp_formula,
                                                       multiplicative_formula = multiplicative_formula,
                                                       data = temp_data,
                                                       meta = temp_paper_meta)


  message(' ---- Completed ', i, ' of ', length(papers))


}

save(hainmueller, file = 'hainmueller_replication/data/hainmueller_meta.rdata')

################################################################################
# Load Hainmueller
# Run Through PAI --> Export
################################################################################

source('R/pai_main.R') # Source PAI
load('hainmueller_replication/data/hainmueller_meta.rdata') # Load Hainmueller Files w/ Meta

hainmueller_runs_combined <- list() # Empty List to Store Locally (Will Also Export Individually)
output_folder <- file.path('hainmueller_replication', 'pai_runs')

for (i in 1:length(hainmueller)){

  temp_rep <- hainmueller[[i]] # Grab Temp Rep
  temp_rep_name <- names(hainmueller[i]) # Get Temp Name
  temp_output_path <- file.path(output_folder, paste0(temp_rep_name, '.rdata')) # Compile Temp Output Path

  if (file.exists(temp_output_path)){
    message('Already Completed ', temp_rep_name, '.... Moving On')
    next
  }

  temp_rep_lm <- lm(temp_rep$formula, data = temp_rep$data) # Basic LM
  temp_rep_multiplicative <- lm(temp_rep$multiplicative_formula, data = temp_rep$data) # Multiplicative LM

  temp_rep$data <- na.omit(temp_rep$data)

  message('Beginning PAI For ', temp_rep_name)

  temp_run <- pai(data = temp_rep$data,
              model = 'rf',
              outcome = as.character(temp_rep$meta$Y),
              predictors = as.character(names(temp_rep$data[!names(temp_rep$data) %in% temp_rep$meta$Y])),
              cores = 10,
              assign_factors = 10,
              placebo_iterations = 50) # Run RF w/ 10 Cores

  hainmueller_runs_combined[[as.character(temp_rep_name)]]  <- list() # Empty List
  hainmueller_runs_combined[[as.character(temp_rep_name)]][['lm']] <- temp_rep_lm # Export LM
  hainmueller_runs_combined[[as.character(temp_rep_name)]][['multiplicative_lm']] <- temp_rep_multiplicative # Export Multiplicative LM
  hainmueller_runs_combined[[as.character(temp_rep_name)]] <- temp_run # Export to List

  temp_output_path <- file.path(output_folder, paste0(temp_rep_name, '.rdata')) # Compile Temp Output Path
  save(temp_run, file = temp_output_path) # Export


}


