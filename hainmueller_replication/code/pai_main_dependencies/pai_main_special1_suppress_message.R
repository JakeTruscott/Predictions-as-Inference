suppress_message <- function(expr){
  sink(tempfile())
  result <- tryCatch(expr, finally = {
    sink()
  })
  invisible(result)
} #Special Function to Suppress Messages from Caret (R)
