#' marrow.plot
#'
#' @return a collection of plots
#' @export
#'
#' @examples
#' marrow.plot()
marrow.plot <- function() {

  plot(hf,
       quote({ plot(vasc, main="", col = "red");
         plot(adipo, col = "green", add=TRUE)
         plot(cells, cols = c("darkorchid1","skyblue1"), chars = c(15,16),
              use.marks = TRUE, legend = TRUE, cex = 0.5,
              add = TRUE)
       }),
       main = "All Samples"
  )
}
