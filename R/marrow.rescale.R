#' Rescale hf
#'
#' @param hf A hyperframe
#' @import spatstat
#' @return A rescaled hyperframe of data
#' @export
#'
#' @examples marrow.rescale(hf)
marrow.rescale <- function(hf){

  colnames <- names(hf)[1:5]

  for (cn in colnames){
    hf[[cn]] <<- solapply(hf[[cn]], rescale, s = 2.41370987, unitname = "\u03bcm")
  }

  return(hf)

}
