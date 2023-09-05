argonTabItem(
  tabName = "Tab_geneseng_all_projects",
  actionButton(
    inputId = "newProject",
    label = "New project",
    icon = argonIcon("fat-add"),
    style = "margin-bottom:15px; background-color:#5E72E3; border-color: #5E72E3; "
  ),
  tags$br(),
  uiOutput("geneseng_main_project")
)