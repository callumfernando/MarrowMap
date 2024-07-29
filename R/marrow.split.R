#' @title Split into test groups
#' @param hf A hyperframe of point patterns with factor groups
#' @import spatstat
#' @return Multiple hyperframes split by group factor
#' @export
#'
#' @examples marrow.split(hf)
marrow.split <- function(hf){

    spl <<- split(hf, hf$group)
  #lapply(spl, marrow.plot)

  print("Split hyperframe according to group saved to 'spl' ")

  return(spl)

  for(i in 1:length(spl)) {
    name <- levels(hf$group)[[i]]
    print(paste("Assigning to global environment:", name))
    assign(name, value = spl[[i]], envir = .GlobalEnv)


  # for(i in 1:length(levels(hf$group))){
  #
  #   name <- levels(hf$group)[[i]]
  #   assign(name, value = spl[[i]], envir = .GlobalEnv)

  }

}
