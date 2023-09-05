# Open project (50 projects max)
observeEvent(input[[paste0("sent_to_", vars()[1,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 1
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[2,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 2
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[3,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 3
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[4,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 4
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[5,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 5
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[6,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 6
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[7,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 7
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[8,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 8
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[9,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 9
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[10,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 10
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[11,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 11
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[12,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 12
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[13,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 13
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[14,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 14
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[15,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 15
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[16,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 16
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[17,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 17
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[18,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 18
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[19,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 19
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[20,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 20
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[21,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 21
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[22,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 22
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[23,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 23
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[24,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 24
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[25,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 25
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[26,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 26
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[27,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 27
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[28,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 28
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[29,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 29
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[30,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 30
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[31,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 31
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[32,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 32
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[33,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 33
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[34,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 34
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[35,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 35
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[36,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 36
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[37,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 37
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[38,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 38
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[39,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 39
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[40,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 40
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[41,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 41
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[42,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 42
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[43,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 43
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[44,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 44
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[45,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 45
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[46,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 46
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[47,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 47
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[48,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 48
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[49,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 49
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})

observeEvent(input[[paste0("sent_to_", vars()[50,"name"])]], {
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    db = res_auth[["user"]],
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
  query$value <- 50
  DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
  DBI::dbDisconnect(con)
  
  refresh()
  
})