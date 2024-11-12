#' Function to perform CLogitTree
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
      if(length(unique(data[, i])) > length(data[, i])/2){
        print(paste0("The column named: ", names(data)[i], ", seems to be an ID or open text, please have a look at it. You can drop the variable using: data <- data[, -", i,"]"))
      }
    }
    
  }
  return(data)
}
