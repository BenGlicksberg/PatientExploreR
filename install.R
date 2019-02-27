#####################################################
## Install all required packages for PatientExploreR
## Ben Glicksberg
## 9/2018
#####################################################

install.packages(c("devtools","shiny","shinyWidgets","shinyjs","shinythemes","DBI","RMySQL","plotly","stringr","dplyr","data.table","purrr"))

# install OHDSI packages
library(devtools)

devtools::install_github("ohdsi/DatabaseConnectorJars")
devtools::install_github("ohdsi/DatabaseConnector")
devtools::install_github("ohdsi/SqlRender")

# install specific versions of other packages
devtools::install_github("rstudio/DT")
devtools::install_github("daattali/timevis")
devtools::install_github("daattali/shinyalert")

