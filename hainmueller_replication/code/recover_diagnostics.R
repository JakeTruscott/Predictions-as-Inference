################################################################################
# Sample Code to Render Diagnostics
################################################################################

################################################################################
# Load Source Files
################################################################################

#pai_dir <- *DIRECTORY* # Set Directory to Folder Location Containing Files

source(file.path(pai_dir, 'code',  'pai_main.R')) # Source PAI Main -- Will Load Necessary Packages
source(file.path(pai_dir, 'code', 'pai_diagnostic.R')) # Source PAI Diagnostic


################################################################################
# List Available Replication Files (Diagnostics)
################################################################################

hainmueller_replications <- list.files(file.path(pai_dir, 'pai_diagnostics'), full.names = T) # List Diagnostics

temp_diagnostic <- get(load(hainmueller_replications[2])) # Replace Index With Associated File

################################################################################
# Render Individual Diagnostics
################################################################################

###########################################
# Placebo Figure
###########################################

temp_diagnostic$placebo # Prints Placebo Figure


###########################################
# Variable-Level Push Figures & Data
###########################################

available_variables <- names(temp_diagnostic$push$figures$linear) # Variable Names

temp_diagnostic$push$figures$linear[1] # Continuous Linear Fit Figure
temp_diagnostic$push$slope_tables$linear[1] # Continuous Linear Fit Data

temp_diagnostic$push$figures$static[1] # Static Bins Figure
temp_diagnostic$push$slope_tables$static[1] # Static Bins Data

temp_diagnostic$push$figures$rolling[1] # Rolling Bins Figure
temp_diagnostic$push$slope_tables$rolling[2] # Rolling Bins Data

temp_diagnostic$push$figures$rolling_extended[1] # Extended Rolling Bins Figure
temp_diagnostic$push$figures$rolling_extended[1] # Extended Rolling Bins Figure


###########################################
# Bootstraps
###########################################

temp_diagnostic$bootstraps$temp_bootstrap_distribution # Overall Boostrap Distribution

available_variables <- names(temp_diagnostic$bootstraps$variable_figures) # Variable Names

temp_diagnostic$bootstraps$variable_figures[1] # Variable Level Bootstrap Figures


###########################################
# Model Performance
###########################################

temp_diagnostic$summary$`Performance Metrics` # Performance Metrics
temp_diagnostic$summary$`Variable Importance` # Variable Importance

temp_diagnostic$summary # Prints All Summary Info (May Be Different Beyond Perfomance & VarImp Re: DV)

###########################################
# Interaction Effect
###########################################

temp_diagnostic$AME_interaction

