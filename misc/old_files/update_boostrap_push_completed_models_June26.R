################################################################################
# Fixing Push & Boostrap CI w/ Updated Code
# After Done -- Redo Diagnostics
################################################################################

################################################################################
# Routine:
# 1) Load Justice-Level Object (Regular & IssueMood Data)
# 2) Re-Run DropVar Models & Boostrap CI
# 3) Redo Push
# 4) Save as New Object in External Drive -- External: PAI Updated July 2024
# 5) Once all Push & CI Done -- Redo Diagnostics
################################################################################


missing_parRF <- c()

external <- list.files("E:/PAI/pai_updated_June2024", full.names = T)
for (i in 1:length(external)){
  temp_folder <- external[i]
  temp_justice <- gsub("E:/PAI/pai_updated_June2024/", '', external[i])
  folders <- list.files(temp_folder)
  if (!'parRF' %in% folders){
    missing_parRF <- c(missing_parRF, temp_justice)
  }
}
missing_parRF
