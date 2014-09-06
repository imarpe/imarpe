# 1.Using roxygen to create the .rd files
library(roxygen2)
roxygenize("/home/vromero/VILMA/PROJECT_IMARPE_TOOLS/imarpeTools",roclets= c("collate", "rd"))

# 2.Building and checking the package through the terminal
system('R CMD build /home/vromero/VILMA/PROJECT_IMARPE_TOOLS/imarpeTools')
system('R CMD check /home/vromero/VILMA/PROJECT_IMARPE_TOOLS/imarpeTools/imarpe_0.1.tar.gz')