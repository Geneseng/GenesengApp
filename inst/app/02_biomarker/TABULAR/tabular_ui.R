argonTabItem(
  tabName = "Tab_geneseng_tabular",
  argonRow(
    argonColumn(
      width = 12,
      argonTabSet(
        id = "Tab1",
        card_wrapper = TRUE,
        horizontal = TRUE,
        circle = FALSE,
        size = "sm",
        width = 12,
        iconList = list(
          icon("table"), 
          argonIcon("chart-bar-32"), 
          argonIcon("laptop"), 
          icon("file-pdf")
        ),
        argonTab(
          tabName = "Dataset",
          argonRow(
            argonColumn(
              width = 12,
              argonTabSet(
                id = "TestingTab100000",
                card_wrapper = FALSE,
                horizontal = TRUE,
                circle = FALSE,
                size = "sm",
                width = 12,
                iconList = list(
                  icon("table"),
                  icon("table")
                ),
                argonTab(
                  tabName = "Dataset",
                  active = FALSE,
                  argonTabItem(
                    tabName = "dataset_tab",
                    argonRow(
                      width = 12,
                      argonColumn(
                        offset = 11,
                        width = 1,
                        downloadButton(
                          outputId = "dataset_download",
                          label = NULL,
                          icon = icon("download"),
                          style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                        )
                      )
                    ),
                    argonRow(
                      argonColumn(
                        width = 4,
                        argonH1("Summary", display = 4)
                      )
                    ),
                    tags$hr(),
                    argonRow(
                      argonColumn(
                        width = 12,
                        uiOutput("sumary_dataset")
                      )
                    ),
                    tags$br(),
                    tags$br(),
                    argonRow(
                      argonColumn(
                        width = 4,
                        argonH1("Dataset", display = 4)
                      )
                    ),
                    tags$hr(),
                    uiOutput("geneseng_tutorial_dataset")
                  )
                ),
                argonTab(
                  tabName = "Customized",
                  argonTabItem(
                    tabName = "wrangle_tab",
                    argonRow(
                      tags$div(
                        argonBadge(text = "Beta", status = "danger"),
                        style = "padding-right:10px; "
                      ),
                      argonH1("Customized", display = 4)
                    ),
                    tags$hr(),
                    sidebarLayout(
                      sidebarPanel(
                        width = 4,
                        selectInput(
                          inputId = "subset_columns",
                          label = "Subset columns",
                          choices = NULL,
                          multiple = TRUE
                        ),
                        #argonRow(
                         # tags$h3("Subset rows", style = "padding-right:10px; color:#525F7F; margin-left:15px; "),
                          #argonSwitch(
                           # inputId = "switch1"
                          #)
                       # ),
                        #tags$div(
                         # id = "filter_subset_data",
                          #selectInput(
                          #  inputId = "subset_filter_names",
                          #  label = NULL,
                          #  choices = NULL
                          #),
                          #selectInput(
                          #  inputId = "subset_filter_operator",
                          #  label = NULL,
                          #  choices = c(
                          #    "equal to" = "et",
                          #    "not equal to" = "net",
                          #    "strictly lesser than" = "slt",
                          #    "lesser than" = "lt",
                          #    "greater than" = "gt",
                          #    "strictly greater than" = "sgt"
                          #  )
                          #),
                          #numericInput(
                          #  inputId = "subset_filter_value",
                          #  label = NULL,
                          #  value = 0
                          #)
                       # ),
                       # argonRow(
                        #  tags$h3("Create new variable", style = "padding-right:10px; color:#525F7F; margin-left:15px; "),
                        #  argonSwitch(
                        #    inputId = "switch2"
                         # )
                       # ),
                        #tags$div(
                        #  id = "mutate_subset_data",
                         # selectInput(
                         #   inputId = "subset_columns02",
                         #   label = NULL,
                         #   choices = NULL,
                         #   multiple = FALSE
                         # ),
                         # selectInput(
                         #   inputId = "subset_mutate_operator",
                         #   label = NULL,
                         #   choices = list(
                          #    "Simple transformation" = c(
                          #      "exponential" = "expr",
                          #      "log base 1" = "log1",
                          #      "log base 2" = "log2",
                          #      "log base 10" = "log10"
                           #   )
                           # )
                         # ),
                         # numericInput(
                         #   inputId = "subset_mutate_value",
                          #  label = NULL,
                          #  value = 0
                         # )
                       # ),
                        checkboxInput(
                          inputId = "save_and_close",
                          label = "Save dataset",
                          value = FALSE
                        ),
                        actionButton(
                          inputId = "modify_dataset",
                          label = "SUBMIT",
                          width = "100%",
                          style = "background-color:#5E72E3; border-color: #5E72E3; "
                        )
                      ),
                      mainPanel(
                        width = 8,
                        uiOutput(outputId = "wrangle_dataset")
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        argonTab(
          tabName = "Analysis",
          argonRow(
            argonColumn(
              width = 12,
              argonTabSet(
                id = "TestingTab1",
                card_wrapper = FALSE,
                horizontal = TRUE,
                circle = FALSE,
                size = "sm",
                width = 12,
                iconList = list(
                  argonIcon("chart-bar-32"), 
                  argonIcon("chart-bar-32"), 
                  argonIcon("chart-bar-32"), 
                  argonIcon("chart-bar-32")
                ),
                argonTab(
                  tabName = "Statistics",
                  active = FALSE,
                  argonRow(
                    argonColumn(
                      width = 12,
                      argonTabSet(
                        id = "TestingTab1",
                        card_wrapper = FALSE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32")
                        ),
                        argonTab(
                          tabName = "Univariate analysis",
                          argonTabItem(
                            tabName = "univariate_analysis_tab",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_global_statistics",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "univariate_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 4,
                                argonH1("Statistical metrics", display = 4)
                              )
                            ),
                            tags$hr(),
                            tags$div(
                              uiOutput("geneseng_tutorial_global_statistics"),
                              style = "font-size:80%;"
                            ),
                            tags$br(),
                            uiOutput("geneseng_tutorial_global_statistics2")
                          )
                        ),
                        argonTab(
                          tabName = "Bivariate analysis",
                          argonTabItem(
                            tabName = "bivariate_analysis_tab",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_statistics_bygroup",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "bivariate_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 4,
                                argonH1("Statistical metrics", display = 4)
                              )
                            ),
                            tags$hr(),
                            tags$div(
                              uiOutput("geneseng_tutorial_statistics_bygroup"),
                              style = "font-size:80%;"
                            ),
                            br(),
                            uiOutput("geneseng_tutorial_statistics_bygroup02")
                          )
                        ),
                        argonTab(
                          tabName = "Multivariate analysis",
                          argonTabItem(
                            tabName = "multivariate_analysis_tab",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_statistics_PCA",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "multivariate_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 6,
                                argonH1("Principal Component Analysis", display = 4)
                              )
                            ),
                            tags$hr(),
                            uiOutput("contribution_pca")
                          )
                        )
                      )
                    )
                  )
                ),
                argonTab(
                  tabName = "Tests",
                  active = FALSE,
                  argonRow(
                    argonColumn(
                      width = 12,
                      argonTabSet(
                        id = "TestingTab1",
                        card_wrapper = FALSE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32")
                        ),
                        argonTab(
                          tabName = "Continuous variables",
                          argonTabItem(
                            tabName = "continuous_variables_test",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_statistical_tests",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "statistcal_tests_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 6,
                                argonH1("Continuous variables", display = 4)
                              )
                            ),
                            tags$hr(),
                            uiOutput("geneseng_tutorial_statistical_tests")
                          )
                        ),
                        argonTab(
                          tabName = "Categorical variables",
                          argonTabItem(
                            tabName = "categorical_variables_test",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_statistical_tests02",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "statistcal_tests02_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 6,
                                argonH1("Categorical variables", display = 4)
                              )
                            ),
                            tags$hr(),
                            uiOutput("geneseng_tutorial_statistical_tests2")
                          )
                        )
                      )
                    )
                  )
                ),
                argonTab(
                  tabName = "Correlations",
                  active = FALSE,
                  argonRow(
                    argonColumn(
                      width = 12,
                      argonTabSet(
                        id = "TestingTabCorr",
                        card_wrapper = FALSE,
                        horizontal = TRUE,
                        circle = FALSE,
                        size = "sm",
                        width = 12,
                        iconList = list(
                          argonIcon("chart-bar-32"), 
                          argonIcon("chart-bar-32"),
                          argonIcon("chart-bar-32")
                        ),
                        argonTab(
                          tabName = "Continuous & Continuous",
                          argonTabItem(
                            tabName = "continuous_relationships_corr",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_correlation",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "continuous_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 5,
                                argonH1("Continous relationships", display = 4),
                              )
                            ),
                            tags$hr(),
                            uiOutput("geneseng_correlation")
                          )
                        ),
                        argonTab(
                          tabName = "Continuous & Categorical",
                          argonTabItem(
                            tabName = "continous_categorical_relationships_corr",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_correlation2",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "continuous_categorical_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 8,
                                argonH1("Continuous & Categorical relationships", display = 4)
                              )
                            ),
                            tags$hr(),
                            uiOutput("geneseng_correlation2"),
                            hidden(uiOutput(outputId = "PointBC"))
                          )
                        ),
                        argonTab(
                          tabName = "Categorical & Categorical",
                          argonTabItem(
                            tabName = "categorical_relationship_test",
                            argonRow(
                              width = 12,
                              argonColumn(
                                offset = 10,
                                width = 1,
                                actionButton(
                                  inputId = "modal_tutorial_correlation3",
                                  label = NULL,
                                  icon = icon("eye"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              ),
                              argonColumn(
                                width = 1,
                                actionButton(
                                  inputId = "categorical_helper",
                                  label = NULL,
                                  icon = icon("info"),
                                  style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                                )
                              )
                            ),
                            argonRow(
                              argonColumn(
                                width = 5,
                                argonH1("Categorical relationships", display = 4)
                              )
                            ),
                            tags$hr(),
                            uiOutput("geneseng_correlation3")
                          )
                        )
                      )
                    )
                  )
                ),
                argonTab(
                  tabName = "Performances",
                  active = FALSE,
                  argonTabItem(
                    tabName = "performances01",
                    argonRow(
                      width = 12,
                      argonColumn(
                        offset = 9,
                        width = 1,
                        actionButton(
                          inputId = "modal_tutorial_individual_Performance",
                          label = NULL,
                          icon = icon("eye"),
                          style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                        )
                      ),
                      argonColumn(
                        width = 1,
                        downloadButton(
                          outputId = "download_best_features_model",
                          label = NULL,
                          icon = icon("download"),
                          style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                        )
                      ),
                      argonColumn(
                        width = 1,
                        actionButton(
                          inputId = "performance_helper",
                          label = NULL,
                          icon = icon("info"),
                          style = "background-color:#5E72E3; border-color: #5E72E3; margin-right:5%; "
                        )
                      )
                    ),
                    argonRow(
                      argonColumn(
                        width = 3,
                        argonH1("Best Features", display = 4)
                      )
                    ),
                    tags$hr(),
                    uiOutput("geneseng_prototype_best_features"),
                    tags$br(),
                    tags$br(),
                    argonRow(
                      argonColumn(
                        width = 3,
                        uiOutput("switch_ml_problem")
                      )
                    ),
                    tags$hr(),
                    uiOutput("geneseng_tutorial_individual_performance"),
                    tags$br(),
                    tags$br(),
                    argonRow(
                      argonColumn(
                        width = 3,
                        argonH1("One vs All", display = 4)
                      )
                    ),
                    tags$hr(),
                    uiOutput("geneseng_tutorial_individual_performance02")
                  )
                )
              )
            )
          )
        ),
        argonTab(
          tabName = "ML",
          active = FALSE,
          argonTabItem(
            tabName = "ML01",
            argonRow(
              tags$div(
                argonBadge(text = "Beta", status = "danger"),
                style = "padding-right:10px; "
              ),
              argonH1("Machine Learning", display = 4)
            ),
            tags$hr(),
            tags$div(
              id = "settings_tutorial_ML",
              argonRow(
                argonColumn(
                  width = 5,
                  tags$div(
                    id = "accordion",
                    style = "overflow-y: auto; height:600px; overflow-x:hidden; ",
                    tags$div(
                      class = "card",
                      tags$div(
                        class = "card-header",
                        id = "headingTwo",
                        tags$h5(
                          class = "mb-0",
                          tags$button(
                            class = "btn btn-link",
                            `data-toggle` ="collapse",
                            `data-target` ="#collapseTwo",
                            `aria-expanded` ="true",
                            `aria-controls` ="collapseTwo",
                            "General Settings",
                            style = "font-size:20px"
                          )
                        )
                      ),
                      tags$div(
                        id = "collapseTwo",
                        class = "collapse show",
                        `aria-labelledby` = "headingTwo",
                        `data-parent` = "#accordion",
                        tags$div(
                          class="card-body",
                          tags$div(
                            style = "margin: 10px 0px 10px 10px",
                            sliderInput(
                              inputId = "tutorial_split_train_test",
                              label = "Proportion of Train set",
                              min = 0.01,
                              max = 1,
                              value = 0.6,
                              step = 0.01,
                              width = "500px"
                            ),
                            selectizeInput(
                              inputId = "tutorial_ML_biomarkers",
                              label = "Define the Biomarkers",
                              choices = NULL,
                              multiple = TRUE,
                              width = "500px"
                            ),
                            selectizeInput(
                              inputId = "select_best_metrics",
                              label = "Define the Metric",
                              choices = list(
                                Classification = c("Accuracy", "AUC"),
                                Regression = c("RMSE", "MAE")
                              ),
                              width = "500px"
                            ),
                            numericInput(
                              inputId = "tutorial_set_seed",
                              label = "Random number for reproducibility",
                              value = as.integer(42),
                              min = 1,
                              max = 2147483647,
                              width = "500px"
                            )
                          )
                        )
                      )
                    ),
                    
                    # Autogluon
                    tags$div(
                      class = "card",
                      fluidRow(
                        column(
                          width = 10,
                          tags$div(
                            class = "card-header",
                            id = "headingAutogluon", 
                            tags$h5(
                              class = "mb-0",
                              tags$button(
                                class = "btn btn-link collapsed",
                                `data-toggle` ="collapse",
                                `data-target` ="#collapseAutogluon",
                                `aria-expanded` ="false",
                                `aria-controls` ="collapseAutogluon",
                                "AutoGluon settings",
                                style = "font-size:20px"
                              )
                            )
                          )
                        ),
                        column(
                          width = 2,
                          tags$div(
                            style = "margin-top:100%; transform: scale(1.5);",
                            checkboxInput(
                              inputId = "checkbox_autogluon",
                              label = NULL,
                              value = TRUE
                            )
                          )
                        )
                      ),
                      tags$div(
                        id = "collapseAutogluon",
                        class = "collapse",
                        `aria-labelledby` = "headingAutogluon",
                        `data-parent` = "#accordion",
                        tags$div(
                          class="card-body",
                          tags$div(
                            style = "margin: 10px",
                            selectInput(
                              inputId = "autogluon_presets",
                              label = "Improve performance of models (need increase time)",
                              choices = c(
                                'medium_quality', 
                                'good_quality', 
                                'high_quality', 
                                'best_quality'
                              ),
                              width = "500px"
                            ),
                            numericInput(
                              inputId = "autogluon_max_time",
                              label = "Maximum time to fit (seconde)",
                              value = 0,
                              min = 0,
                              width = "500px"
                            )
                          )
                        )
                      )
                    ),
                    
                    # Flaml
                    tags$div(
                      class = "card",
                      fluidRow(
                        column(
                          width = 10,
                          tags$div(
                            class = "card-header",
                            id = "headingFlaml", 
                            tags$h5(
                              class = "mb-0",
                              tags$button(
                                class = "btn btn-link collapsed",
                                `data-toggle` ="collapse",
                                `data-target` ="#collapseFlaml",
                                `aria-expanded` ="false",
                                `aria-controls` ="collapseFlaml",
                                "FLAML settings",
                                style = "font-size:20px"
                              )
                            )
                          )
                        ),
                        column(
                          width = 2,
                          tags$div(
                            style = "margin-top:100%; transform: scale(1.5);",
                            checkboxInput(
                              inputId = "checkbox_flaml",
                              label = NULL,
                              value = TRUE
                            )
                          )
                        )
                      ),
                      tags$div(
                        id = "collapseFlaml",
                        class = "collapse",
                        `aria-labelledby` = "headingFlaml",
                        `data-parent` = "#accordion",
                        tags$div(
                          class="card-body",
                          tags$div(
                            style = "margin: 10px",
                            numericInput(
                              inputId = "flaml_max_time",
                              label = "Maximum time to fit (seconde)",
                              value = 30,
                              min = 30,
                              width = "500px"
                            )
                          )
                        )
                      )
                    ),
                    
                    # H2o
                    tags$div(
                      class = "card",
                      fluidRow(
                        column(
                          width = 10,
                          tags$div(
                            class = "card-header",
                            id = "headingThree",
                            tags$h5(
                              class = "mb-0",
                              tags$button(
                                class = "btn btn-link collapsed",
                                `data-toggle` ="collapse",
                                `data-target` ="#collapseThree",
                                `aria-expanded` ="false",
                                `aria-controls` ="collapseThree",
                                "H2o settings",
                                style = "font-size:20px"
                              )
                            )
                          )
                        ),
                        column(
                          width = 2,
                          tags$div(
                            style = "margin-top:100%; transform: scale(1.5);",
                            checkboxInput(
                              inputId = "checkbox_h2o",
                              label = NULL,
                              value = FALSE
                            )
                          )
                        )
                      ),
                      tags$div(
                        id = "collapseThree",
                        class = "collapse",
                        `aria-labelledby` = "headingThree",
                        `data-parent` = "#accordion",
                        tags$div(
                          class="card-body",
                          tags$div(
                            style = "margin: 10px",
                            numericInput(
                              inputId = "h2o_max_models",
                              label = "Maximum number of models to build",
                              value = as.integer(10),
                              min = 2,
                              width = "500px"
                            ),
                            numericInput(
                              inputId = "h2o_max_time",
                              label = "Maximum time to fit (seconde)",
                              value = as.integer(30),
                              min = 30,
                              width = "500px"
                            ),
                            numericInput(
                              inputId = "h2o_cross_validation",
                              label = "Cross Validation",
                              value = as.integer(10),
                              min = 0,
                              max = 10,
                              width = "500px"
                            )
                          )
                        )
                      )
                    ),
                    
                    # TPOT
                    #tags$div(
                    #  class = "card",
                    #  fluidRow(
                    #    column(
                    #      width = 10,
                    #      tags$div(
                    #        class = "card-header",
                    #        id = "headingFour",
                    #        tags$h5(
                    #          class = "mb-0",
                    #          tags$button(
                    #            class = "btn btn-link collapsed",
                    #            `data-toggle` ="collapse",
                    #            `data-target` ="#collapseFour",
                    #            `aria-expanded` ="false",
                    #            `aria-controls` ="collapseFour",
                    #            "TPOT settings",
                    #            style = "font-size:20px"
                    #          )
                    #        )
                    #      )
                    #    ),
                    #    column(
                    #      width = 2,
                    #      tags$div(
                    #        style = "margin-top:100%; transform: scale(1.5);",
                    #        checkboxInput(
                    #        inputId = "checkbox_tpot",
                    #          label = NULL,
                    #          value = FALSE
                    #        )
                    #      )
                    #    )
                    #  ),
                    #  tags$div(
                    #    id = "collapseFour",
                    #    class = "collapse",
                    #    `aria-labelledby` = "headingFour",
                    #    `data-parent` = "#accordion",
                    #    tags$div(
                    #      class="card-body",
                    #      tags$div(
                    #        style = "margin:10px",
                    #        numericInput(
                    #          inputId = "tpot_generations",
                    #          label = "Number of iterations to the run pipeline optimization",
                    #          value = as.integer(1),
                    #          min = 1,
                    #          width = "500px"
                    #        ),
                    #        numericInput(
                    #          inputId = "tpot_population_size",
                    #          label = "Number of individuals to retain at each generations",
                    #          value = as.integer(50),
                    #          min = 1,
                    #          width = "500px"
                    #        ),
                    #        numericInput(
                    #          inputId = "mutation_rate",
                    #          label = "Mutation rate",
                    #          value = 0.9,
                    #          min = 0,
                    #          max = 1,
                    #          step = 0.01,
                    #          width = "500px"
                    #        ),
                    #        numericInput(
                    #          inputId = "crossover_rate",
                    #          label = "Crossover rate",
                    #          value = 0.1,
                    #          min = 0,
                    #          max = 1,
                    #          step = 0.01,
                    #          width = "500px"
                    #        ),
                    #        numericInput(
                    #          inputId = "tpot_early_stop",
                    #          label = "Ends the optimization process if there is no improvement",
                    #          value = as.integer(5),
                    #          min = 0,
                    #          step = 1,
                    #          width = "500px"
                    #        ),
                    #        numericInput(
                    #          inputId = "tpot_cross_validation",
                    #          label = "Cross Validation",
                    #          value = as.integer(10),
                    #          min = 0,
                    #          max = 10,
                    #          width = "500px"
                    #        )
                    #      )
                    #    )
                    #  )
                    #),
                    
                    # Autosklearn
                    tags$div(
                      class = "card",
                      fluidRow(
                        column(
                          width = 10,
                          tags$div(
                            class = "card-header",
                            id = "headingAutosklearn",
                            tags$h5(
                              class = "mb-0",
                              tags$button(
                                class = "btn btn-link collapsed",
                                `data-toggle` ="collapse",
                                `data-target` ="#collapseAutosklearn",
                                `aria-expanded` ="false",
                                `aria-controls` ="collapseAutosklearn",
                                "Auto-sklearn settings",
                                style = "font-size:20px"
                              )
                            )
                          )
                        ),
                        column(
                          width = 2,
                          tags$div(
                            style = "margin-top:100%; transform: scale(1.5);",
                            checkboxInput(
                              inputId = "checkbox_sklearn",
                              label = NULL,
                              value = FALSE
                            )
                          )
                        )
                      ),
                      tags$div(
                        id = "collapseAutosklearn",
                        class = "collapse",
                        `aria-labelledby` = "headingAutosklearn",
                        `data-parent` = "#accordion",
                        tags$div(
                          class="card-body",
                          tags$div(
                            style = "margin:10px",
                            numericInput(
                              inputId = "time_left_for_this_task",
                              label = "Time limit for the search of appropriate models (seconde)",
                              value = as.integer(120),
                              min = as.integer(30),
                              width = "500px"
                            ),
                            numericInput(
                              inputId = "per_run_time_limit",
                              label = "Time limit for a single call to the model (seconde)",
                              value = as.integer(30),
                              min = as.integer(10),
                              width = "500px"
                            )
                          )
                        )
                      )
                    ),
                    
                    # GAMA
                    #tags$div(
                    #  class = "card",
                    #  fluidRow(
                    #    column(
                    #      width = 10,
                    #      tags$div(
                    #        class = "card-header",
                    #        id = "headingGama",
                    #        tags$h5(
                    #          class = "mb-0",
                    #          tags$button(
                    #            class = "btn btn-link collapsed",
                    #            `data-toggle` ="collapse",
                    #            `data-target` ="#collapseGama",
                    #            `aria-expanded` ="false",
                    #            `aria-controls` ="collapseGama",
                    #            "GAMA settings",
                    #            style = "font-size:20px"
                    #          )
                    #        )
                    #      )
                    #    ),
                    #    column(
                    #      width = 2,
                    #      tags$div(
                    #        style = "margin-top:100%; transform: scale(1.5);",
                    #        checkboxInput(
                    #          inputId = "checkbox_gama",
                    #          label = NULL,
                    #          value = FALSE
                    #        )
                    #      )
                    #    )
                    #  ),
                    #  tags$div(
                    #    id = "collapseGama",
                    #    class = "collapse",
                    #    `aria-labelledby` = "headingGama",
                    #    `data-parent` = "#accordion",
                    #    tags$div(
                    #      class="card-body",
                    #      tags$div(
                    #        style = "margin:10px",
                    #        numericInput(
                    #          inputId = "gama_max_total_time",
                    #          label = "Time left for this task (seconde)",
                    #          value = as.integer(180),
                    #          min = as.integer(30),
                    #          width = "500px"
                    #        )
                    #      )
                    #    )
                    #  )
                    #),
                    
                    argonRow(
                      argonColumn(
                        offset = 4,
                        width = 1,
                        tags$br(),
                        actionButton(
                          inputId = "tutorial_ML_train",
                          label = "Train",
                          icon = icon("play"),
                          style = "background-color:#5E72E3; border-color: #5E72E3; color:white;"
                        )
                      )
                    )
                  )
                ),
                argonColumn(
                  width = 7,
                  hidden(
                    tags$div(
                      id = "automl_scoring",
                      style = "overflow-y: auto; overflow-x:hidden; height:600px;",
                      genesengApp::automlPanel(
                        globalID = "input_autogluon",
                        href_img = "https://auto.gluon.ai/stable/index.html",
                        img = "assets/img/ml/autogluon_automl.png",
                        btn_model = "autogluon_model_info", 
                        btn_pred = "autogluon_model_pred",
                        btn_download = "autogluon_model_download",
                        train_score = uiOutput(outputId = "autogluon_train_score"),
                        test_score = uiOutput(outputId = "autogluon_test_score"),
                        metric = uiOutput(outputId = "autogluon_metric")
                      ),
                      genesengApp::automlPanel(
                        globalID = "input_flaml",
                        href_img = "https://github.com/microsoft/FLAML",
                        img = "assets/img/ml/flaml_automl.png",
                        btn_model = "flaml_model_info", 
                        btn_pred = "flaml_model_pred",
                        btn_download = "flaml_model_download",
                        train_score = uiOutput(outputId = "flaml_train_score"),
                        test_score = uiOutput(outputId = "flaml_test_score"),
                        metric = uiOutput(outputId = "flaml_metric")
                      ),
                      genesengApp::automlPanel(
                        globalID = "input_h2o",
                        href_img = "https://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html#",
                        img = "assets/img/ml/h2o_automl.jpg",
                        btn_model = "h2o_model_info", 
                        btn_pred = "h2o_model_pred",
                        btn_download = "h2o_model_download",
                        train_score = uiOutput(outputId = "h2o_train_score"),
                        test_score = uiOutput(outputId = "h2o_test_score"),
                        metric = uiOutput(outputId = "h2o_metric")
                      ),
                      #genesengApp::automlPanel(
                      # globalID = "input_tpot",
                      #  href_img = "https://epistasislab.github.io/tpot/",
                      #  img = "assets/img/ml/tpot_automl.jpg",
                      #  btn_model = "tpot_model_info", 
                      #  btn_pred = "tpot_model_pred",
                      #  btn_download = "tpot_model_download",
                      #  train_score = uiOutput(outputId = "tpot_train_score"),
                      #  test_score = uiOutput(outputId = "tpot_test_score"),
                      #  metric = uiOutput(outputId = "tpot_metric")
                      #),
                      genesengApp::automlPanel(
                        globalID = "input_sklearn",
                        href_img = "https://automl.github.io/auto-sklearn/master/",
                        size_img = "35px",
                        img = "assets/img/ml/autosklearn.png",
                        btn_model = "sklearn_model_info", 
                        btn_pred = "sklearn_model_pred",
                        btn_download = "sklearn_model_download",
                        train_score = uiOutput(outputId = "sklearn_train_score"),
                        test_score = uiOutput(outputId = "sklearn_test_score"),
                        metric = uiOutput(outputId = "sklearn_metric")
                      )
                      #genesengApp::automlPanel(
                      #  globalID = "input_gama",
                      #  href_img = "https://github.com/openml-labs/gama",
                      #  size_img = "75px",
                      #  img = "assets/img/ml/gama.png",
                      #  btn_model = "gama_model_info", 
                      #  btn_pred = "gama_model_pred",
                      #  btn_download = "gama_model_download",
                      #  train_score = uiOutput(outputId = "gama_train_score"),
                      #  test_score = uiOutput(outputId = "gama_test_score"),
                      #  metric = uiOutput(outputId = "gama_metric")
                      #)
                    )
                  )
                )
              )
            )
          )
        ),
        argonTab(
          tabName = "Report",
          active = FALSE,
          style = "height:600px;",
          shinybusy::add_busy_bar(color = "#5E72E3"),
          actionButton(
            inputId = "generate_tutorial_report",
            label = "Generate Report",
            icon = icon("play"),
            style = "
                background-color:#5E72E3; 
                border-color: #5E72E3;   
                margin: 0;
                position: absolute;
                top: 50%;
                left: 50%;
                -ms-transform: translate(-50%, -50%);
                transform: translate(-50%, -50%);
                padding:15px;
                "
          ),
          hidden(
            uiOutput(outputId = "pdfview")
          )
        )
      )
    )
  )
)
