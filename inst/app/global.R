library(argonDash)
library(argonR)
library(fusionchartsR)
library(reshape2)
library(shinyjs)
library(dplyr)

# Set the maximum file to 2GB
options(shiny.maxRequestSize = 2000 * 1024^2)

# Load Python scripts
path_fx <- system.file("python", package = "genesengApp")
all_fx <- list.files(path_fx, full.names = TRUE)
for(file in all_fx){
  reticulate::source_python(file)
}


### Credentials
con <- DBI::dbConnect(
  odbc::odbc(),
  .connection_string = Sys.getenv("SQL_DRIVER"),
  db = "credentials",
  server = Sys.getenv("BDD_host"),
  UID = Sys.getenv("SQL_ID"),
  PWD = Sys.getenv("SQL_PASSWORD"),
  Port = "3306"
)

creds <- DBI::dbReadTable(con, "creds")

credentials <- data.frame(
  user = creds$user,
  password = creds$password,
  start = creds$purchase_date,
  expire = creds$expire_date,
  admin = creds$admin,
  comment = "Simple and secure authentification mechanism 
  for single 'Shiny' applications.",
  stringsAsFactors = FALSE
)

DBI::dbDisconnect(con)


