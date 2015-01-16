#' Loop timer
#'
#' For timing loops
#' 
#' @param tim Previous instance of timer to be updated
#' @param n total count in the loop
#' @param i iteration that was just finished
#' @param when_ready if true add ETA to message.
#' 
#' @details
#' Approximative iteration speed, time left, and ETA.
#' 
#' @examples
#' 
#' 
#' N <- 100
#' t0 <- timer()
#' for(i in 1:N){
#'  # do something
#'  Sys.sleep(runif(1))
#'  t0 <- timer(t0, N, i, TRUE)
#'  cat(t0$message,"\n")
#' }
#' 
#' 
#' @export
looptimer <- function(tim, n, i, when_ready=FALSE){
  if(missing(tim)){
    tim <- list(tvec=NULL, speed=Inf)
  }else{
    ti <- Sys.time()-tim$Tlast
    tim$tvec <- c(tim$tvec, ti)
    tim$speed <- mean(tim$tvec, trim = 0.1)
    tim$eta <- NA
    tim$message <- NULL
    if(!missing(n)&!missing(i)) {
      tim$eta <- Sys.time()+tim$speed*(n-i)
      tim$left <- format(tim$eta-Sys.time(), digits = 2)
      end <- if(when_ready) paste0(", ready ", format(tim$eta),"]") else "]"
      tim$message <- paste0("[",i,"/",n,"][ave time ", round(tim$speed,2), " secs, ", tim$left, " left", end)
        
    }
  }
  tim$Tlast <- Sys.time()
  tim
}