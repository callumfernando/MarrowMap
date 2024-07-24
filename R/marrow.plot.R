#' marrow.plot
#'
#' @return a collection of plots
#' @export
#'
#' @examples
#' marrow.plot()
marrow.plot <- function(hf = hf) {

      plot(hf,
       quote({ plot(vasc, main="", col = "red")
         plot(adipo, col = "green", add=TRUE)
         plot(cells, cols = c("darkorchid1","skyblue1"), chars = c(15,16),
              use.marks = TRUE, legend = TRUE, cex = 0.3,
              add = TRUE)
         plot(win, add = TRUE)
           }),
        main = ""
       )


   plot(hf,
        quote({
          plot(win, add = FALSE, main = "")
          plot(vasc, main="", col = "red", add = TRUE)
          plot(adipo, col = "green", add=TRUE)
          plot(ckit, cols = "darkorchid1", chars = 15,
               use.marks = TRUE, legend = TRUE, cex = 0.6,
               add = TRUE)
        }),

        main = "CKIT cells and TME"

   )


   plot(hf,
        quote({ plot(win, add = FALSE, main = "")
          plot(adipo, col = "green", add=TRUE)
          plot(ckit, cols = "darkorchid1", chars = 15,
               use.marks = TRUE, legend = TRUE, cex = 0.6,
               add = TRUE)

        }),
        main = "CKIT positive cells and adipocytes"
   )

}
