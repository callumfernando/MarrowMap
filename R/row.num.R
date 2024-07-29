#' Appropriate row numbers
#' @param hf A hyperframe
#' @return Renumber the row of hyperframe
#' @export
#'
#' @examples row.num(hf)
row.num <- function(hf){

  nr <- nrow(hf)
  rownames(hf) <<- as.character(c(1:nr))
}
