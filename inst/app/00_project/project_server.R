# Display Main table
output$geneseng_main_project <- renderUI({
  
  con <- DBI::dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("SQL_DRIVER"),
    server = Sys.getenv("BDD_host"),
    UID = Sys.getenv("SQL_ID"),
    PWD = Sys.getenv("SQL_PASSWORD"),
    Port = "3306"
  )
  
  schemas <- DBI::dbGetQuery(con, DBI::sqlInterpolate(con, "SHOW SCHEMAS;"))[,1]
  
  if(res_auth[["user"]] %in% schemas){
    
    DBI::dbGetQuery(con, DBI::sqlInterpolate(con, paste("USE", res_auth[["user"]])))
    
    if(DBI::dbExistsTable(con, paste0(res_auth[["user"]], "Summary"))){
      
      query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
      DBI::dbDisconnect(con)
      
      
      lst <- lapply(1:nrow(query), function(i){
        
        if(query$file[i] == "Tabular"){
          badge <- argonBadge(text = query$file[i], status = "success")
        }
        
        tmp <- tagList(
          argonTableItems(
            argonTableItem(sub(res_auth[["user"]], "", query$name[i])),
            argonTableItem(query$description[i]),
            argonTableItem(query$type[i]),
            argonTableItem(badge),
            argonTableItem(query$dt[i]),
            argonTableItem(
              actionButton(inputId = query$open[i], label = "open", icon = icon("eye"), style = "background-color:#5E72E3; border-color: #5E72E3;"),
              actionButton(inputId = query$rmv[i], label = "trash", icon = icon("trash"), style = "background-color:#FF595E; border-color: #FF595E;")
            )
          )
        )
      })
      
      argonTable(
        cardWrap = TRUE,
        headTitles = c("Name","Description", "Method", "Type of file", "Date (MM-DD-YYYY)", ""),
        tagList(
          lst
        )
      )
      
    } else {
      DBI::dbDisconnect(con)
      return(NULL)
    }
  } else {
    DBI::dbDisconnect(con)
    return(NULL)
  }
})


# Show New project panel
observeEvent(input$newProject, {
  shinyalert::shinyalert(
    title = "Create new project", 
    text = "Import biomarkers from any source",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#5E72E3",
    callbackR = function(x){
      if(isTRUE(x)){
        show("tab-Tab_geneseng_upload_dataset") 
        session$sendCustomMessage(type = "update-tabs", message = "_geneseng_upload_dataset")
      }
    }
  )
})

# Display the active project
observe({
  number <- number()
  if(!is.null(res_auth[["user"]]) & number > 0){
    show("tab-Tab_geneseng_tabular")
  }
})

# [TABULAR] projects
output$tabName <- renderText({
  number <- number()
  sub(res_auth[["user"]], "", vars()[number,"name"])
})

