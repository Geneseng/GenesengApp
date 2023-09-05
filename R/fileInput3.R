#' Alternative version of shiny::fileInput
#'
#' @export
fileInput3 <- function (inputId, label, multiple = FALSE, accept = NULL, width = NULL, 
          buttonLabel = "Browse...", placeholder = "No file selected", 
          capture = NULL, style) 
{
  restoredValue <- restoreInput(id = inputId, default = NULL)
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                         style = paste("position: absolute !important; top: -99999px !important; left: -99999px !important;", style), 
                         `data-restore` = restoredValue, webkitdirectory = TRUE)
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }
  div(class = "form-group shiny-input-container", style = "width:100%; ", 
      div(class = "input-group", 
          tags$label(class = "input-group-btn input-group-prepend", 
                                                      span(class = "btn btn-default btn-file", buttonLabel, 
                                                           inputTag)), tags$input(type = "text", class = "form-control", 
                                                                                  placeholder = placeholder, readonly = "readonly")), 
      tags$div(id = paste(inputId, "_progress", sep = ""), 
               class = "progress active shiny-file-input-progress", 
               tags$div(class = "progress-bar")))
}

