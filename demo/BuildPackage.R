# 1.Using roxygen to create the .rd files
library(roxygen2)
roxygenize("/home/vromero/VILMA/PROJECT_IMARPE_TOOLS/imarpeTools",roclets= c("collate", "rd"))

# 2.Building and checking the package through the terminal
system(paste0("R CMD build ",getwd(),sep=""))
system(paste0("R CMD check ",getwd(),"/imarpe_0.1.tar.gz",sep=""))
