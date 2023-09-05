observe({
  
  number <- number()
  
  updateSelectInput(
    session = session,
    inputId = "tutorial_ML_biomarkers",
    choices = setdiff(names(tutorial()), vars()[number,"target"]),
    selected = setdiff(names(tutorial()), vars()[number,"target"])
  )
  
})




### Manage settings

## AutoGluon
observeEvent(input$checkbox_autogluon, {
  if(isFALSE(input$checkbox_autogluon)){
    disable("headingAutogluon")
  } else {
    enable("headingAutogluon")
  }
})

## Flaml
observeEvent(input$checkbox_flaml, {
  if(isFALSE(input$checkbox_flaml)){
    disable("headingFlaml")
  } else {
    enable("headingFlaml")
  }
})

## H2o
observeEvent(input$checkbox_h2o, {
  if(isFALSE(input$checkbox_h2o)){
    disable("headingThree")
  } else {
    enable("headingThree")
  }
})


## sklearn
observeEvent(input$checkbox_sklearn, {
  if(isFALSE(input$checkbox_sklearn)){
    disable("headingAutosklearn")
  } else {
    enable("headingAutosklearn")
  }
})


# Train model button
observe({
  
  if(!is.null(input$select_best_metrics)){
    
    number <- number()
    df <- tutorial()
    target <- vars()[number,"target"]
    type <- vars()[number,"type"]
    
    if(isFALSE(input$checkbox_autogluon) & isFALSE(input$checkbox_flaml) & 
      isFALSE(input$checkbox_h2o) & isFALSE(input$checkbox_sklearn)
    ){
      disable("tutorial_ML_train")
    } else if(!is.null(type)){
      if(type == "Classification" & input$select_best_metrics %in% c("RMSE", "MAE")){
        disable("tutorial_ML_train")
      } else if(type == "Regression" & input$select_best_metrics %in% c("Accuracy", "AUC")) {
        disable("tutorial_ML_train")
      } else if(type == "Classification" & length(unique(df[,target])) > 2 & input$select_best_metrics %in% c("AUC", "RMSE", "MAE")) {
        disable("tutorial_ML_train")
      } else {
        enable("tutorial_ML_train")
      }
    }
    
  }
  
})

# Train models
observeEvent(input$tutorial_ML_train, {
  shinyalert::shinyalert(
    title = "Train models", 
    text = "This can take a long time!",
    closeOnEsc = FALSE,
    closeOnClickOutside = FALSE,
    showCancelButton = TRUE,
    confirmButtonText = "Yes",
    confirmButtonCol = "#5E72E3",
    callbackR = function(x){
      if(isTRUE(x)){
        
        hide("automl_scoring")
        
        # Set up the dataset
        number <- number()
        df <- tutorial()
        df2 <- df[,c(input$tutorial_ML_biomarkers, vars()[number,"target"])]
        
        # Label Encoder
        df3 <- MakeLabelEncoder(df2)
        
        # Train/Test split
        split <- split_train_test_set(
          data = df3, 
          y = vars()[number,"target"],
          train_size = input$tutorial_split_train_test,
          seed = input$tutorial_set_seed
        )
        
        X_train <- split$X_train
        X_test <- split$X_test
        y_train <- split$y_train
        y_test <- split$y_test
        
################################################################################
        
############################### AutoGluon autoML ###############################
        
        if(isFALSE(input$checkbox_autogluon)){
          hide("input_autogluon")
        } else {
          
          hide("input_autogluon")
          
          set.seed(input$tutorial_set_seed)
          
          autogluon_model <- autogluon_autoML(
            train = cbind(X_train, Target = y_train), 
            y_label = "Target",
            time_limit = input$autogluon_max_time,
            presets = input$autogluon_presets
          )

          
          # Train score
          output$autogluon_train_score <- renderUI({
            
            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_train, autogluon_model$predict(X_train)))) / sum(table(y_train, autogluon_model$predict(X_train))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_train, autogluon_model$predict_proba(X_train)[,1]),
                genesengApp::compute_roc_auc(y_train, autogluon_model$predict_proba(X_train)[,2])
              ),
              "RMSE" = sqrt(mean((y_train - autogluon_model$predict(X_train)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_train - autogluon_model$predict(X_train)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Train set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Test score
          output$autogluon_test_score <- renderUI({

            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_test, autogluon_model$predict(X_test)))) / sum(table(y_test, autogluon_model$predict(X_test))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_test, autogluon_model$predict_proba(X_test)[,1]),
                genesengApp::compute_roc_auc(y_test, autogluon_model$predict_proba(X_test)[,2])
              ),
              "RMSE" = sqrt(mean((y_test - autogluon_model$predict(X_test)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_test - autogluon_model$predict(X_test)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Test set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Display metric used
          output$autogluon_metric <- renderUI({
            tags$h4(
              tags$i(
                paste0("Metric: ", isolate(input$select_best_metrics))
              )
            )
          })
          
          # Best Model
          observeEvent(input$autogluon_model_info, {
            showModal(
              modalDialog(
                footer = NULL,
                size = "l",
                easyClose = TRUE,
                renderUI({
                  
                  # Best Model & Tunes
                  learner <- autogluon_model$leaderboard(cbind(X_train, Target = y_train))[1,]
                  
                  # Best Features
                  lst <- lapply(1, function(i){
                    tmp <- lapply(1:ncol(learner), function(j){
                      argonTableItem(learner[i,j])
                    })
                    argonTableItems(
                      tagList(tmp)
                    )
                  })
                  
                  tagList(
                    tags$div(
                      style = "overflow-y:auto; height: 150px;",
                      argonTable(
                        cardWrap = TRUE,
                        headTitles = gsub("test", "train", names(learner)),
                        tagList(
                          lst
                        )
                      )
                    ),
                    renderfusionPlot({
                      permutated_score <- genesengApp::geneseng_feature_importance(
                        n = 100,
                        model = autogluon_model,
                        X_train = X_train,
                        y_train = y_train,
                        metric = isolate(input$select_best_metrics)
                      )
                      
                      permutated_score %>%
                        melt() %>%
                        fusionPlot(x = "Var2", y = "value", type = "boxandwhisker2d") %>%
                        fusionCaption(caption = "[AutoGluon] - Permutation Feature Importance") %>%
                        fusionSubcaption(subcaption = "Features were randomly shuffled 100 times") %>%
                        fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                        fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
                    })
                  )
                })
              )
            )
          })
          
          # Download predictions
          output$autogluon_model_pred <- downloadHandler(
            filename = function() {
              "autogluon_ml_prediction.xlsx"
            },
            content = function(file) {
              
              if(vars()[number,"type"] == "Classification"){
                colsn <- unique(df2[,vars()[number,"target"]])
                y_train_true <- colsn[y_train + 1]
                y_train_pred <- colsn[autogluon_model$predict(X_train) + 1]
                y_test_true <- colsn[y_test + 1]
                y_test_pred <- colsn[autogluon_model$predict(X_test) + 1]
              } else {
                y_train_true <- y_train
                y_train_pred <- autogluon_model$predict(X_train)
                y_test_true <- y_test
                y_test_pred <- autogluon_model$predict(X_test)
              }
              
              genesengTools::geneseng_export_file(
                data = list(
                  train = data.frame(
                    true = y_train_true,
                    pred =  y_train_pred
                  ),
                  test = data.frame(
                    true = y_test_true,
                    pred = y_test_pred
                  )
                ),
                filename = file
              )
              
            }
          )
          
          # Export best model
          output$autogluon_model_download <- downloadHandler(
            filename = function() {
              "autogluon_ml_model.pkl"
            },
            content = function(file) {
              reticulate::py_save_object(autogluon_model, file)
            }
          )
          
          show("input_autogluon")
          
        }

########################### Flaml automl #######################################
        
        if(isFALSE(input$checkbox_flaml)){
          hide("input_flaml")
        } else {
          
          hide("input_flaml")
          
          set.seed(input$tutorial_set_seed)
          
          flaml_model <- flaml_autoML(
            X_train = X_train,
            y_train = y_train,
            time_budget = as.integer(input$flaml_max_time)
          )
          
          # Train score
          output$flaml_train_score <- renderUI({
            
            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_train, flaml_model$predict(X_train)))) / sum(table(y_train, flaml_model$predict(X_train))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_train, flaml_model$predict_proba(X_train)[,1]),
                genesengApp::compute_roc_auc(y_train, flaml_model$predict_proba(X_train)[,2])
              ),
              "RMSE" = sqrt(mean((y_train - flaml_model$predict(X_train)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_train - flaml_model$predict(X_train)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Train set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Test score
          output$flaml_test_score <- renderUI({
            
            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_test, flaml_model$predict(X_test)))) / sum(table(y_test, flaml_model$predict(X_test))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_test, flaml_model$predict_proba(X_test)[,1]),
                genesengApp::compute_roc_auc(y_test, flaml_model$predict_proba(X_test)[,2])
              ),
              "RMSE" = sqrt(mean((y_test - flaml_model$predict(X_test)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_test - flaml_model$predict(X_test)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Test set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Display metric used
          output$flaml_metric <- renderUI({
            tags$h4(
              tags$i(
                paste0("Metric: ", isolate(input$select_best_metrics))
              )
            )
          })
          
          # Best Model
          observeEvent(input$flaml_model_info, {
            showModal(
              modalDialog(
                footer = NULL,
                size = "l",
                easyClose = TRUE,
                renderUI({
                  
                  # Best Model & Tunes
                  learner <- cbind(Model = flaml_model$best_estimator, t(unlist(flaml_model$best_config)))
                  
                  # Best Features
                  lst <- lapply(1, function(i){
                    tmp <- lapply(1:ncol(learner), function(j){
                      argonTableItem(learner[i,j])
                    })
                    argonTableItems(
                      tagList(tmp)
                    )
                  })
                  
                  tagList(
                    tags$div(
                      style = "overflow-y:auto; height: 150px;",
                      argonTable(
                        cardWrap = TRUE,
                        headTitles = colnames(learner),
                        tagList(
                          lst
                        )
                      )
                    ),
                    renderfusionPlot({
                      permutated_score <- genesengApp::geneseng_feature_importance(
                        n = 100,
                        model = flaml_model,
                        X_train = X_train,
                        y_train = y_train,
                        metric = input$select_best_metrics
                      )
                      
                      permutated_score %>%
                        melt() %>%
                        fusionPlot(x = "Var2", y = "value", type = "boxandwhisker2d") %>%
                        fusionCaption(caption = "[FLAML] - Permutation Feature Importance") %>%
                        fusionSubcaption(subcaption = "Features were randomly shuffled 100 times") %>%
                        fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                        fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
                    })
                  )
                })
              )
            )
          })
          
          # Download predictions
          output$flaml_model_pred <- downloadHandler(
            filename = function() {
              "flaml_ml_prediction.xlsx"
            },
            content = function(file) {

              if(vars()[number,"type"] == "Classification"){
                colsn <- unique(df2[,vars()[number,"target"]])
                y_train_true <- colsn[y_train + 1]
                y_train_pred <- colsn[flaml_model$predict(X_train) + 1]
                y_test_true <- colsn[y_test + 1]
                y_test_pred <- colsn[flaml_model$predict(X_test) + 1]
              } else {
                y_train_true <- y_train
                y_train_pred <- flaml_model$predict(X_train)
                y_test_true <- y_test
                y_test_pred <- flaml_model$predict(X_test)
              }
              
              genesengTools::geneseng_export_file(
                data = list(
                  train = data.frame(
                    true = y_train_true,
                    pred = y_train_pred
                  ),
                  test = data.frame(
                    true = y_test_true,
                    pred = y_test_pred
                  )
                ),
                filename = file
              )
              
            }
          )
          
          # Export best model
          output$flaml_model_download <- downloadHandler(
            filename = function() {
              "flaml_ml_model.pkl"
            },
            content = function(file) {
              reticulate::py_save_object(flaml_model, file)
            }
          )
          
          show("input_flaml")
          
        }
        
########################### H2o automl #########################################
        
        if(isFALSE(input$checkbox_h2o)){
          hide("input_h2o")
        } else {
          
          hide("input_h2o")
          
          h2o::h2o.init()
          h2o_train <- h2o::as.h2o(cbind(X_train, Target = y_train))
          h2o_test <- h2o::as.h2o(cbind(X_test, Target = y_test))
          
          if(vars()[number,"type"] == "Classification"){
            h2o_train[,"Target"] <- h2o::h2o.asfactor(h2o_train[,"Target"])
            h2o_metric <- "AUTO"
          } else {
            h2o_metric <- input$select_best_metrics
          }
          
          suppressWarnings({
            h2o_model <- h2o::h2o.automl(
              y = "Target",
              training_frame = h2o_train,
              sort_metric = h2o_metric,
              nfolds = max(2, input$h2o_cross_validation),
              max_models = input$h2o_max_models,
              max_runtime_secs = input$h2o_max_time,
              seed = input$tutorial_set_seed
            )
          })
          
          # Train score
          output$h2o_train_score <- renderUI({
            
            y_pred <- h2o::h2o.predict(h2o_model@leader, h2o_train)
            
            scoring <- switch(
              input$select_best_metrics, 
              Accuracy = sum(diag(table(y_train, as.vector(y_pred[,1]))))/sum(table(y_train, as.vector(y_pred[,1]))), 
              AUC = max(
                genesengApp::compute_roc_auc(y_train, as.vector(y_pred[,2])), 
                genesengApp::compute_roc_auc(y_train, as.vector(y_pred[,3]))
              ), 
              RMSE = sqrt(mean((y_train - as.numeric(as.vector(y_pred[,1])))^2, na.rm = TRUE)), 
              MAE = mean(abs(y_train - as.numeric(as.vector(y_pred[,1]))), na.rm = TRUE)
            )
            
            tags$h2(
              "Train set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Test score
          output$h2o_test_score <- renderUI({
    
            y_pred <- h2o::h2o.predict(h2o_model@leader, h2o_test)
            
            scoring <- switch(
              input$select_best_metrics, 
              Accuracy = sum(diag(table(y_test, as.vector(y_pred[,1]))))/sum(table(y_test, as.vector(y_pred[,1]))), 
              AUC = max(
                genesengApp::compute_roc_auc(y_test, as.vector(y_pred[,2])), 
                genesengApp::compute_roc_auc(y_test, as.vector(y_pred[,3]))
              ), 
              RMSE = sqrt(mean((y_test - as.numeric(as.vector(y_pred[,1])))^2, na.rm = TRUE)), 
              MAE = mean(abs(y_test - as.numeric(as.vector(y_pred[,1]))), na.rm = TRUE)
            )
            
            tags$h2(
              "Test set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Display metric used
          output$h2o_metric <- renderUI({
            tags$h4(
              tags$i(
                paste0("Metric: ", isolate(input$select_best_metrics))
              )
            )
          })
          
          # Show best model (+ parameters)
          observeEvent(input$h2o_model_info, {
            showModal(
              modalDialog(
                footer = NULL,
                size = "l",
                easyClose = TRUE,
                renderUI({
                  
                  learner <- data.frame(
                    Model = h2o_model@leader@parameters$model_id,
                    Max_models = isolate(input$h2o_max_models),
                    Max_time_to_fit = isolate(input$h2o_max_time),
                    Cross_validation = isolate(max(2,input$h2o_cross_validation)),
                    Random_state = isolate(input$tutorial_set_seed)
                  )
                  
                  # Best Features
                  lst <- lapply(1, function(i){
                    tmp <- lapply(1:ncol(learner), function(j){
                      argonTableItem(learner[i,j])
                    })
                    argonTableItems(
                      tagList(tmp)
                    )
                  })
                  
                  tagList(
                    tags$div(
                      style = "overflow-y:auto; height: 150px;",
                      argonTable(
                        cardWrap = TRUE,
                        headTitles = colnames(learner),
                        tagList(
                          lst
                        )
                      )
                    ),
                    renderfusionPlot({

                      permutated_score <- genesengApp::geneseng_feature_importance_h2o(
                        n = 50,
                        model = h2o_model,
                        X_train = h2o_train,
                        y_train = y_train,
                        metric = isolate(input$select_best_metrics)
                      )
                      
                      permutated_score %>%
                        melt() %>%
                        fusionPlot(x = "Var2", y = "value", type = "boxandwhisker2d") %>%
                        fusionCaption(caption = "[H2O] - Permutation Feature Importance") %>%
                        fusionSubcaption(subcaption = "Features were randomly shuffled 50 times") %>%
                        fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                        fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
                      
                    })
                  )
                })
              )
            )
          })
          
          # Download predictions
          output$h2o_model_pred <- downloadHandler(
            filename = function() {
              "h2o_ml_prediction.xlsx"
            },
            content = function(file) {
              
              if(vars()[number,"type"] == "Classification"){
                colsn <- unique(df2[,vars()[number,"target"]])
                y_train_true <- colsn[y_train + 1]
                y_train_pred <- colsn[as.numeric(as.vector(h2o::h2o.predict(h2o_model, h2o_train)[,1])) + 1]
                y_test_true <- colsn[y_test + 1]
                y_test_pred <- colsn[as.numeric(as.vector(h2o::h2o.predict(h2o_model, h2o_test)[,1])) + 1]
              } else {
                y_train_true <- y_train
                y_train_pred <- as.numeric(as.vector(h2o::h2o.predict(h2o_model, h2o_train)[,1]))
                y_test_true <- y_test
                y_test_pred <- as.numeric(as.vector(h2o::h2o.predict(h2o_model, h2o_test)[,1]))
              }
              
              genesengTools::geneseng_export_file(
                data = list(
                  train = data.frame(
                    true = y_train_true,
                    pred = y_train_pred
                  ),
                  test = data.frame(
                    true = y_test_true,
                    pred = y_test_pred
                  )
                ),
                filename = file
              )
        
            }
          )
          
          # Export best model
          output$h2o_model_download <- downloadHandler(
            filename = function() {
              "h2o_ml_model.rds"
            },
            content = function(file) {
              saveRDS(object = h2o_model, file = file)
            }
          )
          
          # h2o::h2o.shutdown(prompt = FALSE)
          show("input_h2o")
          
        }
        
############################ Auto-sklearn ######################################
        
        if(isFALSE(input$checkbox_sklearn)){
          hide("input_sklearn")
        } else {
          
          hide("input_sklearn")
          
          set.seed(input$tutorial_set_seed)
          
          sklearn_model <- sklearn_autoML(
            X_train = X_train,
            y_train = y_train,
            method = vars()[number,"type"],
            time_left_for_this_task = input$time_left_for_this_task, 
            per_run_time_limit = input$per_run_time_limit
          )
          
          # Train score
          output$sklearn_train_score <- renderUI({
            
            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_train, sklearn_model$predict(X_train)))) / sum(table(y_train, sklearn_model$predict(X_train))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_train, sklearn_model$predict_proba(X_train)[,1]),
                genesengApp::compute_roc_auc(y_train, sklearn_model$predict_proba(X_train)[,2])
              ),
              "RMSE" = sqrt(mean((y_train - sklearn_model$predict(X_train)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_train - sklearn_model$predict(X_train)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Train set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Test score
          output$sklearn_test_score <- renderUI({
            
            scoring <- isolate(switch(
              input$select_best_metrics,
              "Accuracy" = sum(diag(table(y_test, sklearn_model$predict(X_test)))) / sum(table(y_test, sklearn_model$predict(X_test))),
              "AUC" = max(
                genesengApp::compute_roc_auc(y_test, sklearn_model$predict_proba(X_test)[,1]),
                genesengApp::compute_roc_auc(y_test, sklearn_model$predict_proba(X_test)[,2])
              ),
              "RMSE" = sqrt(mean((y_test - sklearn_model$predict(X_test)) ^ 2, na.rm = TRUE)),
              "MAE" = mean(abs(y_test - sklearn_model$predict(X_test)), na.rm = TRUE)
            ))
            
            tags$h2(
              "Test set: ", 
              round(x = isolate(scoring), digits = 3)
            )
            
          })
          
          # Display metric used
          output$sklearn_metric <- renderUI({
            tags$h4(
              tags$i(
                paste0("Metric: ", isolate(input$select_best_metrics))
              )
            )
          })
          
          # Best Model
          observeEvent(input$sklearn_model_info, {
            showModal(
              modalDialog(
                footer = NULL,
                size = "l",
                easyClose = TRUE,
                renderUI({
                  
                  # Best Model & Tunes
                  hyper_parameters <- t(unlist(sklearn_model$get_params()))
                  
                  # learner <- cbind(Model, hyper_parameters)
                  learner <- hyper_parameters
                  
                  # Best Features
                  lst <- lapply(1, function(i){
                    tmp <- lapply(1:ncol(learner), function(j){
                      argonTableItem(learner[i,j])
                    })
                    argonTableItems(
                      tagList(tmp)
                    )
                  })
                  
                  tagList(
                    tags$div(
                      style = "overflow-y:auto; height: 150px;",
                      argonTable(
                        cardWrap = TRUE,
                        headTitles = colnames(learner),
                        tagList(
                          lst
                        )
                      )
                    ),
                    renderfusionPlot({
                      permutated_score <- genesengApp::geneseng_feature_importance(
                        n = 25,
                        model = sklearn_model,
                        X_train = X_train,
                        y_train = y_train,
                        metric = isolate(input$select_best_metrics)
                      )
                      
                      permutated_score %>%
                        melt() %>%
                        fusionPlot(x = "Var2", y = "value", type = "boxandwhisker2d") %>%
                        fusionCaption(caption = "[sklearn] - Permutation Feature Importance") %>%
                        fusionSubcaption(subcaption = "Features were randomly shuffled 25 times") %>%
                        fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                        fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
                    })
                  )
                })
              )
            )
          })
          
          # Download predictions
          output$sklearn_model_pred <- downloadHandler(
            filename = function() {
              "sklearn_ml_prediction.xlsx"
            },
            content = function(file) {
              
              # inverse_transform
              colsn <- unique(df2[,vars()[number,"target"]])
              
              if(vars()[number,"type"] == "Classification"){
                colsn <- unique(df2[,vars()[number,"target"]])
                y_train_true <- colsn[y_train + 1]
                y_train_pred <- colsn[sklearn_model$predict(X_train) + 1]
                y_test_true <- colsn[y_test + 1]
                y_test_pred <- colsn[sklearn_model$predict(X_test) + 1]
              } else {
                y_train_true <- y_train
                y_train_pred <- sklearn_model$predict(X_train)
                y_test_true <- y_test
                y_test_pred <- sklearn_model$predict(X_test)
              }
              
              genesengTools::geneseng_export_file(
                data = list(
                  train = data.frame(
                    true = y_train_true,
                    pred = y_train_pred
                  ),
                  test = data.frame(
                    true = y_test_true,
                    pred = y_test_pred
                  )
                ),
                filename = file
              )
              
            }
          )
          
          # Export best model
          output$sklearn_model_download <- downloadHandler(
            filename = function() {
              "sklearn_ml_model.pkl"
            },
            content = function(file) {
              reticulate::py_save_object(sklearn_model, file)
            }
          )
          
          show("input_sklearn")
          
        }

################################################################################  
          
          show("automl_scoring")

      }
    }
  )
})

