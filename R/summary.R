#' Summary method for looptimer
#' 
#' @param x looptimer-object
#' 
#' @details 
#' Prints the iterations, average time and the total time taken. Perhaps call after the loop.
#' 
#' @export

summary.looptimer <- function(x, ...){
  tot <- Sys.time() - x$created
  mesg <- paste0(x$prefix, "[", x$i,"/",x$n,"][ave time ", format(x$speed), " secs][total time: ", format(tot), "]\n")
  x$printer(x, paste0("\n", x$start, mesg, x$end))
}