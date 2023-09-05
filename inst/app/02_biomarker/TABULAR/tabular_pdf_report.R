# Tutorial : Generate report
observeEvent(input$generate_tutorial_report, {
  shinyalert::shinyalert(
    title = "Make new report", 
    text = "This can take a long time!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#5E72E3",
    callbackR = function(x){
      if(isTRUE(x)){
        
        number <- number()
        
          disable("generate_tutorial_report")
          Sys.sleep(0.1)
          path <- file.path(tempdir(), "tutorial_report.pdf")
          df <- tutorial()
          
          if(vars()[number,"type"] == "Classification"){
            if(length(unique(df[,vars()[number,"target"]])) == 2){
              genesengTools::geneseng_two_class_report(
                output_path = tempdir(),
                title = sub(res_auth[["user"]], "", vars()[number,"name"]),
                dataset = df,
                target  = vars()[number,"target"],
                filename = "tutorial_report"
              )
            } else {
              genesengTools::geneseng_multi_class_report(
                output_path = tempdir(),
                title = sub(res_auth[["user"]], "", vars()[number,"name"]),
                dataset = tutorial(),
                target  = vars()[number,"target"],
                filename = "tutorial_report"
              )
            }
          } else {
            genesengTools::geneseng_reg_report(
              output_path = tempdir(),
              title = sub(res_auth[["user"]], "", vars()[number,"name"]),
              dataset = tutorial(),
              target  = vars()[number,"target"],
              filename = "tutorial_report"
            )
          }
          

          file.copy(from = path, to = system.file("app/www", package = "genesengApp"), overwrite = TRUE)
          shinyjs::hide("generate_tutorial_report")
          shinyjs::show("pdfview")
          
          # Remove file
          unlink(path, recursive = TRUE)
          
        }
      }
    )
})

# Display pdf
output$pdfview <- renderUI({
  tags$iframe(src = "tutorial_report.pdf", style= "height: 575px; width:100%")
})


