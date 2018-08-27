library(devtools)
devtools::install_github("rstudio/shinytest")
library(shinytest)
shinytest::installDependencies()


# test the application
testApp()

# record a new test
# recordTest()

