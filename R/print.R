#' Print method for looptimer
#' @param x looptimer object
#' @param ... ignored
#' @exportMethod print
#' @export

print.looptimer <- function(x, ...){
  if(x$i %% x$printevery != 0) return()
  if(!is.null(x$message)){
    x$printer(x, paste0(x$message, x$endline))
  }else{
    if(!is.null(x$tvec)) x$printer(x, paste0(x$speed, "sec/iteration", x$endline))
    else                 x$printer(x, paste0(   "looptimer: no data.", x$endline))
  }
}