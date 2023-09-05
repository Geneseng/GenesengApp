################## Reactive ####################

tutorial <- reactive({
  
  if(!is.null(res_auth[["user"]])){
    
    con <- DBI::dbConnect(
      odbc::odbc(),
      .connection_string = Sys.getenv("SQL_DRIVER"),
      db = res_auth[["user"]],
      server = Sys.getenv("BDD_host"),
      UID = Sys.getenv("SQL_ID"),
      PWD = Sys.getenv("SQL_PASSWORD"),
      Port = "3306"
    )
    
    if(length( DBI::dbListTables(con)) > 0){
      
      number <- number()
      
      if(vars()[number,"file"] == "Tabular"){
        
        if(!is.null(vars()[number,"name"])){
          dataset <- DBI::dbReadTable(con, vars()[number,"name"])
        } else {
          dataset <- data.frame()
        }
        
        DBI::dbDisconnect(con)
        return(dataset)
        
      }
      
    }
    
  }
  
})


# Generate stats
tutorial_stats <- reactive({
  stats <- genesengStats::geneseng_summary_stats(data = tutorial())
  return(stats)
})


# Generate stats by groups
tutorial_stats_group <- reactive({
  
  number <- number()
  
  if(vars()[number,"type"] == "Regression"){
    
    hide("modal_tutorial_statistics_bygroup")
    
  } else {
    
    stats_byGroup <- genesengStats::geneseng_summary_stats(
      data = tutorial(), 
      group = vars()[number,"target"]
    )
    
    return(stats_byGroup)
    
  }
  
})

# Generate PCA results
tutorial_multivariate_pca <- reactive({
  df <- tutorial()
  n <- apply(df, 2, function(x) length(unique(x)))
  FactoMineR::PCA(df, graph = FALSE, quali.sup = which(!(n > 7)), ncp = length(n))
})


# Generate tests
tutorial_tests <- reactive({
  
  number <- number()
  
  if(vars()[number,"type"] == "Regression"){
    hide("modal_tutorial_statistical_tests")
    hide("modal_tutorial_statistical_tests02")
  } else {
    
    test <- genesengStats::geneseng_summary_tests(
      data = tutorial(), 
      group = vars()[number,"target"]
    )
    
    if(is.null(test[[2]])){
      hide("modal_tutorial_statistical_tests02")
    }
    
    return(test)
    
  }
})


# Generate correlation
tutorial_correlation <- reactive({
  
  corr <- genesengStats::geneseng_summary_corr(data = tutorial())
  
  if(is.null(corr[[1]])){
    hide("modal_tutorial_correlation")
  }
  
  if(is.null(corr[[2]])){
    hide("modal_tutorial_correlation2")
  }
  
  if(is.null(corr[[3]])){
    hide("modal_tutorial_correlation3")
  }
  
  return(corr)
  
})


########################### Performances #######################################

# Prototype Best Features
tutorial_best_features <- reactive({
  
  number <- number()
  df <- tutorial()
  
  # Only used for regression or binary classification
  if(vars()[number,"type"] == "Regression" | length(unique(df[,vars()[number,"target"]])) == 2){
    res <- genesengStats::geneseng_best_model(
      data = df,
      group = vars()[number,"target"],
      direction =  "backward"
    )
  } else {
    hide("download_best_features_model")
  }
  
})

# Generate Individual performances
tutorial_performance <- reactive({
  
  number <- number()
  df <- tutorial()
  
  if(vars()[number,"type"] == "Classification"){
    
    perf <- genesengStats::geneseng_summary_class_metrics(
      data = df, 
      group = vars()[number,"target"]
    )

  } else {
    
    hide("modal_tutorial_individual_Performance")
    
    perf <- genesengStats::geneseng_summary_reg_metrics(
      data = df, 
      group = vars()[number,"target"]
    )
    
  }
  
  return(perf)
  
})


# Display the entire dataset (Dataset > Summary)
output$sumary_dataset <- renderUI({
  
  number <- number()
  
  argonTable(
    cardWrap = TRUE,
    headTitles = c("Name","Type", "NB. variables", "Nb. individuals", "Target"),
    argonTableItems(
      argonTableItem(sub(res_auth[["user"]], "", vars()[number,"name"])),
      argonTableItem(vars()[number,"type"]),
      argonTableItem(vars()[number,"columns"]),
      argonTableItem(vars()[number,"rows"]),
      argonTableItem(vars()[number,"target"])
    )
  )
  
})


# Display the entire dataset (Dataset > Dataset)
output$geneseng_tutorial_dataset <- renderUI ({
  
  df <- tutorial()
  lst <- lapply(1:nrow(df), function(i){
    tmp <- lapply(1:ncol(df), function(j){
      argonTableItem(df[i,j])
    })
    argonTableItems(
      tagList(tmp)
    )
  })
  
  tags$div(
    style = "overflow-y:auto; height: 500px;",
    argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
  )

})

output$dataset_download <- downloadHandler(
  filename = function() {
    paste0(res_auth[["user"]], "_dataset_", format(Sys.time(), '%m_%d_%Y'), ".xlsx")
  },
  content = function(file) {
    genesengTools::geneseng_export_file(data = tutorial(), file)
  }
)


# Manipulate the current dataset (Dataset > Wrangling)

# Subset columns
observe({
  updateSelectInput(
    session, 
    inputId = "subset_columns", 
    choices = names(tutorial()), 
    selected = names(tutorial())
  )
})

#observeEvent(input$switch1, {
#  toggle("filter_subset_data")
#})

#observe({
#  req(tutorial())
#  n <- apply(tutorial(), 2, function(x) length(unique(x)))
#  updateSelectInput(
#    session,
#    inputId = "subset_filter_names",
#    choices = names(tutorial())[n > 7]
#  )
#})


#observeEvent(input$switch2, {
#  toggle("mutate_subset_data")
#})

#observe({
 # updateSelectInput(
 #   session, 
#    inputId = "subset_columns02", 
#    choices = names(tutorial()), 
#    selected = names(tutorial())
#  )
#})

#observe({
#  req(input$subset_mutate_operator)
#  trans <- c("expr", "log1", "log2", "log10")
#  if(input$subset_mutate_operator %in% trans){
#    hide("subset_mutate_value")
#  } else {
#    show("subset_mutate_value")
#  }
#})

# SUBMIT
observeEvent(input$modify_dataset, {
  
  df_isolate <- isolate({
    
    # Subset columns
    df_subset_columns <- tutorial()[,input$subset_columns]
    
    # Subset rows
    # var_filter <- df_subset_columns[,input$subset_filter_names]
    
   # if(isTRUE(input$switch1)){
    #  df_filter <- switch(
    #    input$subset_filter_operator,
    #    et = df_subset_columns[var_filter == as.numeric(input$subset_filter_value), ],
    #    net = df_subset_columns[var_filter != as.numeric(input$subset_filter_value), ],
    #    slt = df_subset_columns[var_filter < as.numeric(input$subset_filter_value), ],
    #    lt = df_subset_columns[var_filter <= as.numeric(input$subset_filter_value), ],
    #    gt = df_subset_columns[var_filter >= as.numeric(input$subset_filter_value), ],
    #    sgt = df_subset_columns[var_filter > as.numeric(input$subset_filter_value), ]
     # )
   # } else {
    #  df_filter <- df_subset_columns
  #  }
    
    # Create new variable
    #if(isTRUE(input$switch2)){
     # df_mutate <- switch(
     #   input$subset_mutate_operator,
     #   expr = exp(df_filter[,input$subset_columns02, drop = FALSE]),
     #   log1 = log(df_filter[,input$subset_columns02, drop = FALSE]),
     #   log2 = log2(df_filter[,input$subset_columns02, drop = FALSE]),
     #   log10 = log10(df_filter[,input$subset_columns02, drop = FALSE])
     # )
      
     # names(df_mutate) <- paste0(input$subset_columns02, "_2")
     # df_mutate <- cbind(df_filter, df_mutate)
      
    #} else {
    #  df_mutate <- df_filter
   # }
    
  })
  
  output$wrangle_dataset <- renderUI({
    
    df <- df_isolate
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(1:ncol(df), function(j){
        argonTableItem(df[i,j])
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    tags$div(
      style = "overflow-y:auto; height: 300px;",
      argonTable(
        cardWrap = TRUE,
        headTitles = names(df),
        tagList(
          lst
        )
      )
    )
    

  })
  
  if(isTRUE(input$save_and_close)){
    shinyalert::shinyalert(
      title = "Save dataset", 
      text = "Current dataset will be permanently replaced!",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      showCancelButton = TRUE,
      confirmButtonText = "Yes",
      confirmButtonCol = "#5E72E3",
      callbackR = function(x){
        number <- number()
        if(isTRUE(x)){
          
          shinyalert::shinyalert(
            title = "Error", 
            text = "This feature is not yet available!",
            type = "error",
            closeOnEsc = FALSE,
            closeOnClickOutside = FALSE,
            confirmButtonText = "Close",
            confirmButtonCol = "#FF595E",
          ) 
          
          # df <- df_isolate
          
          #con <- DBI::dbConnect(
          #  odbc::odbc(),
          #  .connection_string = Sys.getenv("SQL_DRIVER"),
          #  server = BDD_host,
          #  UID = BDD_user,
          #  PWD = BDD_password,
          #  Port = mysql_port
          #)
          
          # Update the dataset
          #query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
          #query[number, "columns"] <- ncol(df)
          
          #DBI::dbWriteTable(con, paste0(res_auth[["user"]], "Summary"), query, overwrite = TRUE)
          #DBI::dbWriteTable(con, paste0(res_auth[["user"]], input$name_of_project), df, overwrite = TRUE)
          #DBI::dbDisconnect(con)
          
          #shinyjs::refresh()
          
        }
      }
    )
  }
  
})



