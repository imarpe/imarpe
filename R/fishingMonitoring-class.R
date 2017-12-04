
#' @title Report method for fishingMonitoring objects
#' @description Export a report with an analysis of the fishing monitoring.
#' @param object Object of \code{fishingMonitoring} class.
#' @param axisPlot A list with the y-axis of the graphics number one and three of the report.
#' @export
#' @method report fishingMonitoring
report.fishingMonitoring = function(object, format = "latex", tangle = FALSE, output = NULL, open = FALSE,
                                    axisPlot = list(plot1Lab2 = c(0, 3e6, 5e5),
                                                    plot3Lab1 = list(c(0, 3e+05, 5e4), c(0, 20e3, 5e3), c(0, 3e+05, 5e4)),
                                                    plot3Lab2 = list(c(0, 1e+06, 2e5), c(0, 05e5, 1e5), c(0, 1e+06, 2e5))),
                                    juvLimits = list(number = c(37.7, 4.1), weight = c(21.5, 3))) {

  if(is.null(output)) output = getwd()

  outputName = deparse(substitute(object))

  skeleton = system.file("reports", "fishingMonitoring-report.Rmd", package = "imarpe")

  if(isTRUE(tangle)) {
    knit(skeleton, tangle = tangle, encoding = "latin1")
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(outputName, ".R"))
  }

  outputFile = paste0(outputName, "_output.pdf")
  render(input = skeleton, output_format = c("pdf_document"), output_file = outputFile, output_dir = output, 
         encoding = "latin1")

  if(isTRUE(open)) shell.exec(outputFile)

  return(invisible(file.path(output, outputFile)))
}