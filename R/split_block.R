#' Split a vector to blocks of certain size
#' 
#' @param i vector to split
#' @param n elements of i per block
#' 
#' @details last block might not contain n elements, but thats ok.
#' @export

split_block <- function(i,n){
  ni <- length(i)
  
  nblocks <- ceiling(ni/n)
  iblocks <- rep(1:nblocks, each = n)[1:ni]
  split(i, iblocks)
}