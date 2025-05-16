check_and_install_packages <- function(){

  packages <- c('caret', 'dplyr', 'stringr', 'doParallel', 'broom', 'cowplot',
                'grid', 'gridExtra', 'doSNOW', 'gridtext', 'patchwork',
                'randomForest', 'rlist', 'xgboost', 'ggplot2', 'ggridges', 'tidyr')

  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing_packages) > 0) {
    message("The following necessary packages are missing: ", paste(missing_packages, collapse = ", "))

    response <- readline("Would you like to install them? (Y/N): ")
    if (tolower(response) %in% c("yes", "y")) {
      install.packages(missing_packages, dependencies = TRUE, quiet = TRUE)
      message("Packages installed successfully.")
    } else {
      stop("Packages must be installed prior to compilation. Please install and try again.")
    }
  }

  invisible(lapply(packages, function(pkg) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }))

  message('All Packages Installed and Deployed Correctly')
}

check_and_install_packages() # Deploy
