library(devtools)
devtools::install_github("rstudio/shinytest")
library(shinytest)
shinytest::installDependencies()

testApp()

# recordTest()

