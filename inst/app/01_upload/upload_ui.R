argonTabItem(
  tabName = "Tab_geneseng_upload_dataset",
  tags$div(
    class = "card shadow",
    tags$div(
      class = "card-body",
      style = "background-color:transparent;",

      # Step #1 - TABULAR
      tags$div(
        id = "progressProject01",
        argonRow(
          width = 12,
          argonColumn(
            offset = 1,
            width = 10,
            tags$div(
              style = "color:#5E72E3; text-align:center;",
              tags$span("Step 1"),
              argonH1("SELECT THE SOURCE OF YOUR BIOMARKERS", display = 4, style = "color:#5E72E3;")
            )
          )
        ),
        tags$br(),
        argonRow(
          width = 12,
          argonColumn(
            width = 3,
            tags$div(
              class = "content_img",
              actionButton(
                inputId  = "tabular_biomarker",
                class = "content_img",
                label = NULL,
                icon = NULL,
                width = "100%",
                style = "background-image: url('assets/img/upload/tabular.jpg');"
              ),
              tags$div("TABULAR")
            )
          )
        )
      ),
      
      # Step 2 - TABULAR
      hidden(
        tags$div(
          id = "progressProject02",
          argonRow(
            width = 12,
            argonColumn(
              offset = 11,
              width = 1,
              actionButton(
                inputId = "tabularFileHelp",
                label = NULL,
                icon = icon("info"),
                style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
              )
            )
          ),
          argonRow(
            width = 12,
            argonColumn(
              offset = 2,
              width = 8,
              tags$div(
                style = "color:#5E72E3; text-align:center;",
                tags$span("Step 2"),
                argonH1("IMPORT YOUR TABULAR DATASET", display = 4, style = "color:#5E72E3;")
              )
            )
          ),
          tags$br(),
          argonRow(
            width = 12,
            argonColumn(
              width = 12,
              genesengApp::upload_biomarker_file(
                inputId  = "upload_file",
                label = "Upload File"
              )
            ),
            tags$br(),
            hidden(
              uiOutput("upload_tabular_table", width = "100%")
            )
          ),
          tags$hr(),
          tags$br(),
          tags$br(),
          argonRow(
            width = 12,
            argonColumn(
              width = 2,
              actionButton(
                inputId = "return01",
                class = "primary",
                label = "Previous",
                icon = icon("long-arrow-alt-left"),
                width = NULL,
                style = "float:right; bottom:0; right:5; position: absolute; background-color:#5E72E3; border-color:#5E72E3"
              )
            ),
            argonColumn(
              offset = 8,
              width = 2,
              hidden(
                actionButton(
                  inputId = "next01",
                  class = "primary",
                  label = "Next",
                  icon = NULL,
                  width = NULL,
                  HTML('<i class="fas fa-long-arrow-alt-right"></i>'),
                  style = "float:right; bottom:0; right:0; position: absolute; background-color:#5E72E3; border-color:#5E72E3"
                )
              )
            )
          )
        )
        
      )
    )
  )
)