
#' @title Report method for fishingMonitoring objects
#' @description Export a report with an analysis of the fishing monitoring.
#' @param x Object of \code{fishingMonitoring} class.
#' @param axisPlot A list with the y-axis of the graphics number one and three of the report.
#' @export
#' @method report fishingMonitoring
report.fishingMonitoring = function(x, format = "latex", tangle=FALSE, output = NULL, open = TRUE,
                                    axisPlot = list(plot1Lab2 = c(0, 3e6, 5e5),
                                                    plot3Lab1 = list(c(0, 3e+05, 5e4), c(0, 20e3, 5e3), c(0, 3e+05, 5e4)),
                                                    plot3Lab2 = list(c(0, 1e+06, 2e5), c(0, 5e5, 1e5), c(0, 1e+06, 2e5)))) {

  if(is.null(output)) output = getwd()

  outputName = deparse(substitute(x))

  skeleton = system.file("reports", "fishingMonitoring-report.Rmd", package = "imarpe")

  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE, encoding = "latin1")
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(outputName, ".R"))
  }

  outputFile = paste0(outputName, "_output.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output, encoding = "latin1")

  if(isTRUE(open)) shell.exec(outputFile)

  return(invisible(file.path(output, outputFile)))

}

