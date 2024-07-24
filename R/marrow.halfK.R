#' Test of homogeneity within each single point pattern
#'
#' @param hf A hyperframe
#' @param Kest A summary function
#' @import spatstat
#' @return Plots of summary function with point patterns split into halves both ways.
#' @export
#'
#' @examples marrow.halfK(hf, Kest)

marrow.halfK <- function(hf = hf, Kest = Kest) {

  for(i in 1:10){

  ckt <- hf$ckit[[i]]

  mx <- median(coords(ckt)$x)
  halves <- chop.tess(Window(ckt), infline(v=mx))
  plot(anylapply(split(ckt, halves), Kest))

  my <- median(coords(ckt)$y)
  halves <- chop.tess(Window(ckt), infline(h=my))
  plot(anylapply(split(ckt, halves), Kest))

  }

}
