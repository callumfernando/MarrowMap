#' Distance function to components of the tumour microenvironment
#' @param hf A hyperframe
#' @importFrom spatstat.geom distfun
#' @return Adds distance functions to the hyperframe
#' @export
#'
#' @examples dist.tme()

dist.tme <- function(){

  hf$dist_vasc <- with(hf, distfun(vasc))
  hf$dist_adipo <- with(hf, distfun(adipo))

}
