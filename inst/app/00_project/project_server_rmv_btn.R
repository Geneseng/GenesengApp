# Remove projects
observeEvent(input[[paste0("rv_to_", vars()[1,"name"])]], {
  
  NB <- 1
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])

        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[2,"name"])]], {
  
  NB <- 2
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[3,"name"])]], {
  
  NB <- 3
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[4,"name"])]], {
  
  NB <- 4
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[5,"name"])]], {
  
  NB <- 5
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[6,"name"])]], {
  
  NB <- 6
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[7,"name"])]], {
  
  NB <- 7
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[8,"name"])]], {
  
  NB <- 8
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[9,"name"])]], {
  
  NB <- 9
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[10,"name"])]], {
  
  NB <- 10
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[11,"name"])]], {
  
  NB <- 11
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[12,"name"])]], {
  
  NB <- 12
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[13,"name"])]], {
  
  NB <- 13
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[14,"name"])]], {
  
  NB <- 14
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[15,"name"])]], {
  
  NB <- 15
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[16,"name"])]], {
  
  NB <- 16
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[17,"name"])]], {
  
  NB <- 17
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[18,"name"])]], {
  
  NB <- 18
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[19,"name"])]], {
  
  NB <- 19
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[20,"name"])]], {
  
  NB <- 20
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[21,"name"])]], {
  
  NB <- 21
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[22,"name"])]], {
  
  NB <- 22
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[23,"name"])]], {
  
  NB <- 23
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[24,"name"])]], {
  
  NB <- 24
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[25,"name"])]], {
  
  NB <- 25
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[26,"name"])]], {
  
  NB <- 26
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[27,"name"])]], {
  
  NB <- 27
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[28,"name"])]], {
  
  NB <- 28
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[29,"name"])]], {
  
  NB <- 29
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[30,"name"])]], {
  
  NB <- 30
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[31,"name"])]], {
  
  NB <- 31
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[32,"name"])]], {
  
  NB <- 32
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[33,"name"])]], {
  
  NB <- 33
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[34,"name"])]], {
  
  NB <- 34
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[35,"name"])]], {
  
  NB <- 35
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[36,"name"])]], {
  
  NB <- 36
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[37,"name"])]], {
  
  NB <- 37
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[38,"name"])]], {
  
  NB <- 38
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[39,"name"])]], {
  
  NB <- 39
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[40,"name"])]], {
  
  NB <- 40
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[41,"name"])]], {
  
  NB <- 41
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[42,"name"])]], {
  
  NB <- 42
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[43,"name"])]], {
  
  NB <- 43
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[44,"name"])]], {
  
  NB <- 44
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[45,"name"])]], {
  
  NB <- 45
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[46,"name"])]], {
  
  NB <- 46
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[47,"name"])]], {
  
  NB <- 47
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[48,"name"])]], {
  
  NB <- 48
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[49,"name"])]], {
  
  NB <- 49
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})

observeEvent(input[[paste0("rv_to_", vars()[50,"name"])]], {
  
  NB <- 50
  
  shinyalert::shinyalert(
    title = sub(res_auth[["user"]], "", paste("Remove", vars()[NB,"name"], "project")), 
    text = "Removed project can't be restored!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#FF595E",
    callbackR = function(x){
      if(isTRUE(x)){
        
        con <- DBI::dbConnect(
          odbc::odbc(),
          .connection_string = Sys.getenv("SQL_DRIVER"),
          db = res_auth[["user"]],
          server = Sys.getenv("BDD_host"),
          UID = Sys.getenv("SQL_ID"),
          PWD = Sys.getenv("SQL_PASSWORD"),
          Port = "3306"
        )
        
        DBI::dbRemoveTable(con, vars()[NB,"name"])
        
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
        query <- query[-NB, ]
        
        if(nrow(query) > 0){
          query$value <- nrow(query) 
          DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
        } else {
          DBI::dbRemoveTable(con, paste0(res_auth[["user"]], "Summary"))
        }
        
        DBI::dbDisconnect(con)
        
        refresh()
        
      }
    }
  )
})