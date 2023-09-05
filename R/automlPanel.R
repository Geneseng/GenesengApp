#' AutoML dashboard
#' 
#' @param globalID inputId of the global div
#' @param href_img path to autoML documentation
#' @param img logo of the image
#' @param btn_model inputId of the modal button
#' @param btn_pred inputId of the prediction outputs
#' @param btn_download inputId of the download button
#' @param train_score score of the train score
#' @param test_score score of the test score
#' @param metric Metric
#' 
#' @import shiny
#' @import argonR
#' 
#' @export
automlPanel <- function(
  globalID = "randomID",
  href_img = "https://docs.h2o.ai/h2o/latest-stable/h2o-docs/automl.html",
  size_img = "120px",
  img = 'h2o_automl.jpg',
  btn_model = "h2o_model_info", 
  btn_pred = "h2o_model_pred", 
  btn_download = "h2o_model_download",
  train_score = 0.98, 
  test_score = 0.95,
  metric = "Accuracy"
  ){
  tags$div(
    id = globalID,
    style = "border:1px solid #e3e3e3;",
    argonRow(
      argonColumn(
        offset = 9,
        width = 1,
        actionButton(
          inputId = btn_model,
          label = "",
          icon = icon("eye"),
          style = "margin-left:-95px; margin-top:15px; background-color:#5E72E3; border-color:#5E72E3; "
        )
      ),
      argonColumn(
        width = 1,
        downloadButton(
          outputId = btn_pred,
          label = "",
          icon = icon("file-excel"),
          style = "margin-left:-75px; margin-top:15px; background-color:#5E72E3; border-color:#5E72E3; "
        )
      ),
      argonColumn(
        width = 1,
        downloadButton(
          outputId = btn_download,
          label = "",
          style = "margin-left:-60px; margin-top:15px; background-color:#5E72E3; border-color:#5E72E3; "
        )
      )
    ),
    argonRow(
      argonColumn(
        width = 3,
        tags$a(
          href = href_img,
          target="_blank",
          tags$img(src = img, height = size_img, style = "margin-left: 15px; margin-bottom: 15px;")
        )
      ),
      argonColumn(
        offset = 1,
        width = 8,
        tags$br(),
        train_score,
        test_score
      )
    ),
    argonRow(
      argonColumn(
        offset = 8,
        width = 4,
        metric
      )
    )
  )
}


