## STEP 1: SELECT THE SOURCE OF YOUR BIOMARKERS

# Go to step 2
observeEvent(input$tabular_biomarker, {
  hide("progressProject01")
  show("progressProject02")
})

################################################################################

# Display help
observeEvent(input$tabularFileHelp, {
  shinyalert::shinyalert(
    html = TRUE,
    title = "Expected File Architecture",
    text = tagList(
      tags$img(
        src = "assets/img/upload/upload_tabular.png",
        height = "175px"
      )
    ),
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    confirmButtonText = "Close",
    confirmButtonCol = "#5E72E3"
  )
})

## STEP 2: IMPORT YOUR DATASET

# Return to step 1
observeEvent(input$return01, {
  show("progressProject01")
  hide("progressProject02")
})

# Upload a tabular dataset
dataset <- reactive({
  req(input$upload_file$datapath)
  if(grepl(pattern = ".xls|.xlsx|.csv|.txt", x = input$upload_file$datapath)){
    
    df <- genesengTools::geneseng_import_file(
      filename = input$upload_file$datapath, 
      sheet = 1
    )
    
    show("next01")
    
  } else {
    df <- NULL
  }
  return(df)
})


observeEvent(input$upload_file, {
  if(!grepl(pattern = ".xls|.xlsx|.csv|.txt", x = input$upload_file$datapath)){
    shinyalert::shinyalert(
      title = "Error",
      text = "Only .xls(x), .csv or .txt files are supported!",
      type = "error",
      confirmButtonCol = "#FF595E"
    )
  } else {
    df <- dataset()
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(1:ncol(df), function(j){
        argonTableItem(df[i,j])
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    # Display the table
    output$upload_tabular_table <- renderUI({
      tags$div(
        style = "overflow-y:auto; height:300px; width:750px; ",
        argonTable(
          cardWrap = TRUE,
          headTitles = names(df),
          tagList(
            lst
          )
        )
      )
    })
    
    show("upload_tabular_table")
    
  }
})

# Go to step 3
observeEvent(input$next01, {
  shinyjs::show("progressProject03")
  shinyjs::hide("progressProject02")
})

################################################################################

## STEP 3: DESCRIPTION

# Return to step 2
observeEvent(input$return02, {
  shinyjs::show("progressProject02")
  shinyjs::hide("progressProject03")
})


# Select Classification or Regression
observe({
  
  df <- dataset()
  
  if(!is.null(df)){
    n <- apply(df, 2, function(x) length(unique(x)) > 7)
    if(input$final_project_method == "Classification"){
      updateSelectizeInput(
        session,
        inputId = "final_project_target_var",
        choices = names(df)[!n]
      )
    } else {
      updateSelectizeInput(
        session,
        inputId = "final_project_target_var",
        choices = names(df)[n]
      )
    }
  }
  
})

observe({
  updateTextInput(session, "final_project_name", value = input$name_of_project)
  updateTextInput(session, "final_project_description", value = input$name_of_description)
  updateTextInput(session, "final_project_data_type", value = "Tabular")
})

# Validate project
observeEvent(input$valid_project, {
  shinyalert::shinyalert(
    title = "Confirm project",
    text = "The Project can't be modified!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#5E72E3",
    callbackR = function(x){
      if(isTRUE(x)){
        
        # Prevent Project size
        if(input$name_of_project == "" | input$name_of_description == ""){
          shinyalert::shinyalert(
            title = "Error",
            text = "We found some empty arguments!",
            type = "error",
            confirmButtonCol = "#FF595E"
          )
        } else if(nchar(input$name_of_project) > 20 & nchar(input$name_of_description) > 35){
          shinyalert::shinyalert(
            title = "Error",
            text = "Arguments exceed the maximum of authorized characters!",
            type = "error",
            confirmButtonCol = "#FF595E"
          )
        } else if(nchar(input$name_of_project) > 20){
          shinyalert::shinyalert(
            title = "Error",
            text = "Name project can't exceed 20 characters!",
            type = "error",
            confirmButtonCol = "#FF595E"
          )
        } else if(
          grepl(
            pattern = "(\\~|\\`|\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\(|\\)|\\-|\\_|\\+|\\=|\\{|\\}|\\[|\\]|\\|\\/|\\:|\\;\\>|\\,|\\.|\\s+)",  
            x = input$name_of_project
          )
        ){
          shinyalert::shinyalert(
            title = "Error",
            text = "Name project can't contain special characters or spaces !",
            type = "error",
            confirmButtonCol = "#FF595E"
          )
        } else if(nchar(input$name_of_description) > 35){
          shinyalert::shinyalert(
            title = "Error",
            text = "Description project can't exceed 35 characters!",
            type = "error",
            confirmButtonCol = "#FF595E"
          )
        } else {
          
          # Prevent if the target variable contains Missing values (NAs)
          df <- dataset()
          target <- input$final_project_target_var
          
          if(sum(is.na(df[,target])) > 0){
            shinyalert::shinyalert(
              title = paste("The", target, "variable contains Missing value(s)"), 
              text = "Change the variable of interest!",
              closeOnEsc = FALSE,
              closeOnClickOutside = FALSE,
              showCancelButton = FALSE,
              type = "error",
              confirmButtonCol = "#FF595E"
            )
          } else {
            
            # Connection to mySQL
            con <- DBI::dbConnect(
              odbc::odbc(),
              .connection_string = Sys.getenv("SQL_DRIVER"),
              db = res_auth[["user"]],
              server = Sys.getenv("BDD_host"),
              UID = Sys.getenv("SQL_ID"),
              PWD = Sys.getenv("SQL_PASSWORD"),
              Port = "3306"
            )
            
            # Storage the dataset
            summary_table <- data.frame(
              value = 1,
              name = paste0(res_auth[["user"]], input$name_of_project),
              description = input$name_of_description,
              type = input$final_project_method,
              file = "Tabular",
              dt = format(Sys.time(), '%m-%d-%Y'),
              target = input$final_project_target_var,
              columns = ncol(dataset()),
              rows = nrow(dataset()),
              open = paste0("sent_to_", res_auth[["user"]], input$name_of_project),
              rmv = paste0("rv_to_", res_auth[["user"]], input$name_of_project)
            )
            
            
            if(DBI::dbExistsTable(con, paste0(res_auth[["user"]], "Summary"))){
              
              query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
              
              
              # Prevent redundant project names
              if(summary_table$name %in% query$name){
                
                shinyalert::shinyalert(
                  title = paste(input$name_of_project, "is not available!"), 
                  text = "Please find a valid projet name!",
                  closeOnEsc = FALSE,
                  closeOnClickOutside = FALSE,
                  showCancelButton = FALSE,
                  type = "error",
                  confirmButtonCol = "#FF595E"
                )
                
                DBI::dbDisconnect(con)
                
              } else {
                
                summary_table <- dplyr::bind_rows(query, summary_table)
                summary_table$value <- nrow(summary_table)
                DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), summary_table, overwrite = TRUE)
                DBI::dbWriteTable(con, paste0(res_auth[["user"]], input$name_of_project), dataset())
                DBI::dbDisconnect(con)
                shinyjs::refresh()
                
              }
              
            } else {
              
              DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), summary_table)
              DBI::dbWriteTable(con, paste0(res_auth[["user"]], input$name_of_project), dataset())
              DBI::dbDisconnect(con)
              shinyjs::refresh()
              
            }
          }
        }
      }
    }
  )
})