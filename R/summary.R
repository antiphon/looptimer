#' Summary method for looptimer
#' 
#' @export

summary.looptimer <- function(x, ...){
  tot <- Sys.time() - x$created
  mesg <- paste0(x$prefix, "[", x$i,"/",x$n,"][ave time ", format(x$speed), " secs][total time: ", format(tot), "]\n")
  cat(mesg)
}