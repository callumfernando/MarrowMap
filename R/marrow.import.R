
#' @title Import and clean
#' @import spatstat
#' @import MarrowMap
#' @return A loaded hyperframe of data from marrow.gen
#' @export
#'
#' @examples marrow.import()
marrow.import <- function(){

marrow.load()

row.num(hf)
unmark.ckit(hf)
dist.tme()
hf$dens <- density(hf$cells)
spl <- split(hf, hf$group)
fatty <- spl[[1]]
lean <- spl[[2]]

}
