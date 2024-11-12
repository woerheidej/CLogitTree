#' Function to perform prepare.data
#'
#' Performs prepare.data, which corrects the data types inside your data frame to what is needed for the package#'
#' @param data Data frame (containing all required variables)
#' @return
#' \item{data}{Corrected data frame}
#' @author Jack Wörheide: \email{ge97pah@@tum.de}

prepare.data <- function(data) {
  for (i in 1:length(data)) {
    if (is.data.frame(data[, i])) {
      data[, i] <- unlist(data[, i])
    }
    if (!is.numeric(data[, i])) {
      data[, i] <- as.factor(data[, i])
    }
  }
  return(data)
}
