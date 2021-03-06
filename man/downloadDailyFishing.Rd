% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imarpe-auxiliar.R
\name{downloadDailyFishing}
\alias{downloadDailyFishing}
\title{Download daily fishing monitoring files}
\usage{
downloadDailyFishing(directory = NULL,
  urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal",
  startDate, endDate, saveFile = TRUE)
}
\arguments{
\item{directory}{Directory where the landing and effort information are stored. By default it
is \code{NULL}, temporarily saved and then deleted. If you want to keep this parameter must be changed.}

\item{urlFishingMonitoring}{The web address (url - Uniform Resource Locator) for downloading
the landings. By default it is \url{http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal}.}

\item{startDate}{Start date to download the files.}

\item{endDate}{End date to download the files.}

\item{saveFile}{A logical parameter. By default is \code{TRUE} to save a data frame
with the fishery information that has been downloaded. The name to save the file is
"dailyFishing" with the start date (without "-") and the end date (without "-"), this
characters saparated by "_".}
}
\value{
A data frame where the columns are the variables in spanish:
\itemize{
 \item anho: for years
 \item mes: for months
 \item dia: for days
 \item especie: for species
 \item tipo_flota: for type of fleet
 \item puerto: for port
 \item captura: for the catch or landing of the species
 \item embarcaciones: for the effort
}
}
\description{
Function to download landing and effort anchovy information from an official
repository of the IMARPE. The unique effort of this data base is number of boats.
}
\examples{
# Download fishing information, by default it is saved.
fishingInfo = downloadDailyFishing(startDate = "2017-4-20", endDate = "2017-6-19")
View(fishingInfo)

#Use saveFile = FALSE to do not save the information.
fishingInfo = downloadDailyFishing(startDate = "2017-4-20", endDate = "2017-6-19", saveFile = FALSE)
View(fishingInfo)

}
