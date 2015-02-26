#' Print method for looptimer
#' 
#' @exportMethod print
#' @export

print.looptimer <- function(x, ...){
  if(!is.null(x$message)){
    cat(x$message, "\n")
  }else{
    if(!is.null(x$tvec)) cat(x$speed, "sec/iteration\n")
    else cat("looptimer: no data.\n")
  }
}