#' Split plots of bone marrow point pattern hyperframe
#'
#' @param spl A list of split hyperframes generated from marrow.split()
#' @import spatstat
#'
#' @return Split plots of hyperframe
#' @export
#'
#' @examples split.plot(spl)
split.plot <- function(){

  plot(lean,
       quote({
         plot(win, add = FALSE, main = "")
         plot(adipo, col = "green", add=TRUE)
         plot(ckit, cols = "darkorchid1", chars = 15,
              use.marks = TRUE, legend = TRUE, cex = 0.3,
              add = TRUE)
       }),
       main = "Lean tibia samples"
  )

  plot(fatty,
       quote({
         plot(win, add = FALSE, main = "")
         plot(adipo, col = "green", add=TRUE)
         plot(ckit, cols = "darkorchid1", chars = 15,
              use.marks = TRUE, legend = TRUE, cex = 0.3,
              add = TRUE)
       }),
       main = "Fatty tibia samples"
  )

}
