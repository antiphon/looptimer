#' Loop timer
#'
#' For timing loops
#' 
#' @param tim Previous instance of timer to be updated
#' @param n total count in the loop
#' @param i iteration that was just finished
#' @param when_ready if true add ETA to message.
#' @param memory How many iteration back remember when computing the average time
#' @details
#' Approximative iteration speed, time left, and ETA.
#' 
#' @examples
#' 
#' 
#' N <- 100
#' t0 <- looptimer()
#' for(i in 1:N){
#'  # do something
#'  Sys.sleep(runif(1))
#'  print(  t0 <- looptimer(t0, N, i, TRUE) )
#' }
#' 
#' 
#' @export
looptimer <- function(tim, n, i, when_ready=TRUE, memory=50){
  if(missing(tim)){
    tim <- list(tvec=NULL, speed=Inf)
    if(!missing(n)) {tim$n <- n; tim$i <- 0}
  }
  else{
    ti <- difftime(Sys.time(), tim$Tlast, units="secs")
    tim$tvec <- c(tim$tvec, ti)
    ndata <- length(tim$tvec)
    tim$speed <- mean(tim$tvec[ndata+1-1:min(ndata, memory)], trim = 0.1) ## speed in secs
    tim$eta <- NA
    tim$message <- NULL
    if(missing(n)){ n <- NULL; if(!is.null(tim$n)) n <- tim$n }
    if(missing(i)){ i <- NULL; if(!is.null(tim$i)) i <- (tim$i <- tim$i + 1 )}
    if(!missing(n)) tim$n <- n
    if(!missing(i)) tim$i <- i
    if(!is.null(n) & !is.null(i)){
      tim$eta <- Sys.time()+tim$speed*(n-i)
      tim$left <- format(tim$eta-Sys.time(), digits = 2)
      end <- if(when_ready) paste0(", ready ", format(tim$eta),"]") else "]"
      tim$message <- paste0("[",i,"/",n,"][ave time ", format(tim$speed), " secs, ", tim$left, " left", end)
        
    }
  }
  tim$Tlast <- Sys.time()
  class(tim) <- "looptimer"
  tim
}
