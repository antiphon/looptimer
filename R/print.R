#' Print method for looptimer
#' 
#' @exportMethod print
#' @export

print.looptimer <- function(x, ...){
  if(x$i %% x$printevery != 0) return()
  dev <- function(x) cat(x)#message(x, appendLF = FALSE)
  if(!is.null(x$message)){
    dev(paste0(x$message, x$endline))
  }else{
    if(!is.null(x$tvec)) dev(paste0(x$speed, "sec/iteration", x$endline))
    else                 dev(paste0("looptimer: no data.", x$endline))
  }
}