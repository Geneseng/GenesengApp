# Global 
source("global.R",  local = TRUE)$value

# Ui.R
ui <- argonDashPage(
  title = "Geneseng | Biomarker discovery as a service",
  author = "Alex Yahiaoui Martinez",
  description = "Geneseng SaaS",
  sidebar = argonDashSidebar(
    id = "geneseng_sidebar",
    brand_url = NULL,
    vertical = FALSE,
    side = "left",
    size = "md",
    skin = "light",
    background = "white",
    dropdownMenus = NULL,
    argonSidebarMenu(
      style = "display:-webkit-inline-box;",
      actionButton(
        inputId = "return_to_website",
        label = "",
        icon = NULL,
        width = NULL,
        style = "background-color:white; border-color:white;",
        tags$img(src = "assets/img/geneseng_logo.svg",  height = "30px")
      ),
      argonSidebarItem(
        tabName = "Tab_geneseng_all_projects",
        icon = NULL,
        "Dashboard"
      ),
      hidden(
        argonSidebarItem(
          tabName = "Tab_geneseng_upload_dataset",
          icon = NULL,
          "New project"
        )
      ),
      hidden(
        argonSidebarItem(
          tabName = "Tab_geneseng_tabular",
          icon = NULL,
          textOutput("tabName")
        )
      )
    )
  ),
  navbar = NULL,
  header = NULL,
  body = argonDashBody(
    
    # CSS
    tags$head(
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert(),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/body.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/selectize.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/slider.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/logo.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/badge.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "assets/css/app/btn.css"),
      tags$link(rel = "stylesheet", type = "text/css", href="assets/css/app/style.css"),
      tags$style("body{font-family: Arial !important;}"),
      tags$style(".form-control:focus{border-color:#5E72E3; }"),
      
      ## Shinymanager
      tags$style(".mfb-component--br{display:none !important;}")
      
    ),
    
    argonTabItems(
      source("00_project/project_ui.R",  local = TRUE)$value,
      source("01_upload/upload_ui.R",  local = TRUE)$value,
      source("02_biomarker/TABULAR/tabular_ui.R",  local = TRUE)$value
    ),
    
    # For Tab
    tags$script(
      'Shiny.addCustomMessageHandler("update-tabs", function(message) {
                // hide and inactivate all not selected tabs
                $(".active.show").removeClass("active show");
                $(".tab-pane.active.show").removeClass("active show");
                
                // add active class to the current selected tab and show its content
                $("#tab-Tab" + message).addClass("active show");
                $("#shiny-tab-Tab" +  message).addClass("active show");
               });'
    ),
    tags$script(src = "assets/js/file_upload.js")
  ),
  footer = argonDashFooter(
    copyrights = NULL,
    src = "#"
  )
)

# Wrap your UI with secure_app
ui2 <- shinymanager::secure_app(
  ui = ui,
  tags_top = tags$div(
    shinyjs::useShinyjs(),
    tags$head(tags$style("#auth-auth-mod {background-color:#05032B;}")),
    tags$img(src = "assets/img/geneseng_logo.svg", height = 70),
    uiOutput("ChangeTxtContent")
  ),
  tags_bottom = tagList(
    column(
      width = 10,
      fluidRow(
        tags$i(
          tags$b(
            "username:"
          ),
          "tutorial"
        )
      ),
      fluidRow(
        tags$i(
          tags$b(
            "password:"
          ),
          "password"
        )
      )
    )
  ),
  head_auth = tags$link(rel='stylesheet', id ='compiled.css-css', href = 'assets/css/app/authenticator.css')
)

# Server.R
server <- function(input, output, session){
  
  # Credentials
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials), 
    keep_token = TRUE
  )

  vars <- reactive({
    
    con <- DBI::dbConnect(
      odbc::odbc(),
      .connection_string = Sys.getenv("SQL_DRIVER"),
      server = Sys.getenv("BDD_host"),
      UID = Sys.getenv("SQL_ID"),
      PWD = Sys.getenv("SQL_PASSWORD"),
      Port = "3306"
    )
    
    schemas <- DBI::dbGetQuery(con, DBI::sqlInterpolate(con, "SHOW SCHEMAS;"))[,1]
    
    if(is.null(res_auth[["user"]])){
      query <- NULL
    } else if(!(res_auth[["user"]] %in% schemas)) {
      DBI::dbGetQuery(con, DBI::sqlInterpolate(con, paste("Create SCHEMA", res_auth[["user"]])))
      DBI::dbGetQuery(con, DBI::sqlInterpolate(con, paste("USE", res_auth[["user"]])))
      query <- NULL
    } else {
      DBI::dbGetQuery(con, DBI::sqlInterpolate(con, paste("USE", res_auth[["user"]])))
      if(DBI::dbExistsTable(con, paste0(res_auth[["user"]], "Summary"))){
        query <- DBI::dbReadTable(con, paste0(res_auth[["user"]], "Summary"))
      } else {
        query <- NULL
      }
    }
    
    DBI::dbDisconnect(con)
    
    return(query)
    
  })
  
  
  # Number
  number <- reactive({
    if(is.null(vars())){
      idx <- 0
    } else {
      idx <- vars()[1,"value"]
    }
    return(idx)
  })

  ## Project
  source("00_project/project_server.R",  local = TRUE)$value
  source("00_project/project_server_open_btn.R",  local = TRUE)$value
  source("00_project/project_server_rmv_btn.R",  local = TRUE)$value
  
  ## Upload files
  # [TABULAR]
  source("01_upload/TABULAR/upload_tabular_server.R",  local = TRUE)$value

  ## Analysis
  # [TABULAR]
  source("02_biomarker/TABULAR/tabular_dataset.R",  local = TRUE)$value
  source("02_biomarker/TABULAR/tabular_analysis.R",  local = TRUE)$value
  source("02_biomarker/TABULAR/tabular_ML.R",  local = TRUE)$value
  source("02_biomarker/TABULAR/tabular_pdf_report.R",  local = TRUE)$value
  
}

shinyApp(ui = ui2, server = server)

