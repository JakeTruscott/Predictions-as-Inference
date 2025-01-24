check_and_install_packages <- function() {

  packages <- c('caret', 'dplyr', 'stringr', 'doParallel', 'broom', 'cowplot', 'grid', 'gridExtra', 'doSNOW', 'gridtext', 'patchwork', 'randomForest', 'rlist', 'xgboost', 'ggplot2', 'ggridges', 'tidyr')

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  # If there are missing packages
  if (length(missing_packages) > 0) {
    message("The Following Necessary Packages Are Missing: ", paste(missing_packages, collapse = ", "))

    response <- readline("Would You Like to Install Them? (Y/N): ")
    if (tolower(response) %in% c("yes", "y", 'Y', 'Yes')) {
      install.packages(missing_packages)
      message("Packages Install Successfully.")
    } else {
      stop("Packages Must Be Installed Prior to Compilation. Please Install And Try Again")
    }
  } # Ask the user if they want to install the missing packages

  lapply(packages, library, character.only = TRUE)
}
