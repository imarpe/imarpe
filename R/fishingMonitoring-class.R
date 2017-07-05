
#' @title Report method for x objects
#' @description x
#' @param x
#' @export
#' @method report fishingMonitoring
report.fishingMonitoring = function(x, format = "latex", tangle=FALSE, output = NULL, open = TRUE) {

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

