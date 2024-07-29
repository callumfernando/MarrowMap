#' @title Unmark the ckit column
#' @param hf A hyperframe
#' @return An unmarked ckit column for hyperframe
#' @export
#' @importFrom spatstat.geom unmark
#'
#' @examples unmark.ckit(hf)

unmark.ckit <- function(hf){

  hf$ckit <<- lapply(hf$ckit, unmark)

}
