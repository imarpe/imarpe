
#' @title Report method for fishingMonitoring objects
#' @description Export a report with an analysis of the fishing monitoring.
#' @param x Object of \code{fishingMonitoring} class.
#' @param axisPlot A list with the y-axis of the graphics number one and three of the report.
#' @export
#' @method report fishingMonitoring
report.fishingMonitoring <- function(x, format = "latex", tangle = FALSE, output = NULL, open = FALSE,
                                     axisPlot = list(ylim_p1 = list(axis2_1 = c(0, 7e6, 1e6),
                                                                    axis4_1 = c(0, 3e6, 1e5),
                                                                    axis4_2 = c(0, 100, 20),
                                                                    axis2_3 = c(0, 100, 20)),
                                                     ylim_p2 = list(axis2 = c(0, 1, 0.25),
                                                                    axis4 = c(0, 20, 5)),
                                                     ylim_p3 = list(axis2_1 = c(0, 03e5, 5e4),
                                                                    axis4_1 = c(0, 01e6, 2e5),
                                                                    axis2_2 = c(0, 20e3, 5e3),
                                                                    axis4_2 = c(0, 05e5, 1e5),
                                                                    axis2_3 = c(0, 03e5, 5e4),
                                                                    axis4_3 = c(0, 01e6, 2e5))),
                                                     juvLimits = list(number = c(37.7, 4.1), weight = c(21.5, 3))){
  if(is.null(output)) output = getwd()
  
  outputName <- deparse(substitute(object))
  
  skeleton <- system.file("reports", "fishingMonitoring-report.Rmd", package = "imarpe")
  
  if(isTRUE(tangle)) {
    knit(skeleton, tangle = tangle, encoding = "latin1")
    f1 <- gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from = basename(f1), to = paste0(outputName, ".R"))
  }
  
  outputFile <- paste0("Daily-report_upto_", format(x$endDate, format = "%d%m%Y"), ".pdf")
  render(input = skeleton, output_format = c("pdf_document"), output_file = outputFile, output_dir = output, 
         encoding = "latin1")
  
  if(isTRUE(open)) shell.exec(outputFile)
  
  return(invisible(file.path(output, outputFile)))
}