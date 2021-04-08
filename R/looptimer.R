#' Loop timer
#'
#' For timing loops
#' 
#' @param tim Previous instance of timer to be updated
#' @param n total count in the loop
#' @param i iteration that was just finished
#' @param when_ready if true add ETA to message.
#' @param memory How many iteration back remember when computing the average time
#' @param prefix prefix for print message
#' @param endline If print called, which endline character to use? Default '\n'. Usual alternative is something like '   \r'
#' @param printevery Print every nth iteration only. Affects print-method.
#' @param color Use ANSI color for the print text? Integer from 1 to 256.
#' @param color_bg Use ANSI color for the print background? Integer from 1 to 256. Overrides 'color'.
#' @details
#' Approximative iteration speed, time left, and ETA.
#' 
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

looptimer <- function(tim, n, i, when_ready=TRUE, 
                      memory=50, prefix = "", 
                      endline = "\n", printevery = 1,
                      color = NULL,
                      bg = NULL){
  # from crayon
  fgcodes <- c(paste0('\x1b[38;5;', 0:255, 'm'), '\x1b[39m')
  bgcodes <- c(paste0('\x1b[48;5;', 0:255, 'm'), '\x1b[49m')
  reset <- 257
  #
  if(missing(tim)){
    tim <- list(tvec=NULL, 
                speed=Inf, 
                prefix = prefix, 
                when_ready = when_ready, 
                endline = endline, 
                printevery = printevery,
                i = 0,
                start = "",
                end   = "")
    if(!missing(n)) {tim$n <- n}
    tim$created <- Sys.time()
    if(!is.null(color)) {
      tim$start <- fgcodes[color[1]]
      if(length(color)>1)
        tim$start <- paste0(tim$start, bgcodes[color[2]])
      tim$end <- paste0(fgcodes[reset], bgcodes[reset])
    }
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
      end <- if(tim$when_ready) paste0(", ready ", format(tim$eta),"]") else "]"
      tim$message <- paste0(tim$prefix, "[",i,"/",n,"][ave time ", 
                            format(tim$speed), " secs, ", tim$left, " left", end)
    }
  }
  tim$Tlast <- Sys.time()
  class(tim) <- "looptimer"
  tim
}
