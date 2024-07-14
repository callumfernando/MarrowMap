#' @title marrow.load
#' @description
#' Load a hyperframe created by the marrow.gen function
#'
#' @author Callum Jeevan Fernando
#' @import tcltk
#' @import readr
#' @return Loaded hyperframe
#' @export
#'
#' @examples marrow.load()

marrow.load <- function() {

  print("Please choose the working directory which contains the hyperframe")
  dir <- tk_choose.dir(caption = "Choose working directory")

  setwd(dir)

  #Check that hyperframe file exists within chosen directory

  file_path <- file.path(dir, "hf.rds")
  if (!file.exists(file_path)) {
    print("File 'hf.rds' not found in the selected directory.")
  } else {
  hf <<- readRDS("hf.rds")
  }

}
