
fisheryData = read.csv("data-raw/fisheryData.csv", stringsAsFactors = FALSE)
save(fisheryData, file = "data/fisheryData.RData")
