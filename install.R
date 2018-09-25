#####################################################
## Install all required packages for PatientExploreR
## Ben Glicksberg
## 9/2018
#####################################################

install.packages(c("devtools","shiny","shinyWidgets","shinyjs","shinyalert","shinythemes","DBI","RMySQL","plotly","timevis","stringr","dplyr","data.table","purrr","DT"))

# install OHDSI packages
library(devtools)

devtools::install_github("ohdsi/DatabaseConnectorJars")
devtools::install_github("ohdsi/DatabaseConnector")
devtools::install_github("ohdsi/SqlRender")