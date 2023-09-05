################################# Stats ########################################

################################ Univariate ####################################

### Display the univariate table (Continuous)
output$geneseng_tutorial_global_statistics <- renderUI ({
  
  df <- tutorial_stats()[[1]]
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      
      if(df$normality[i] == "no"){
        normality <- argonBadge(text = df$normality[i], status = "danger")
      } else {
        normality <- argonBadge(text = df$normality[i], status = "success")
      }
      
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$n[i]),
          argonTableItem(df$`n distinct`[i]),
          argonTableItem(df$min[i]),
          argonTableItem(df$median[i]),
          argonTableItem(df$mean[i]),
          argonTableItem(df$sd[i]),
          argonTableItem(df$iqr[i]),
          argonTableItem(df$max[i]),
          argonTableItem(df$`NA's`[i]),
          argonTableItem(df$`Shapiro's test`[i]),
          argonTableItem(normality)
        )
      )
    })
    
    argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
  }

})

### Display the univariate table (Categorical)
output$geneseng_tutorial_global_statistics2 <- renderUI ({
  
  df <- tutorial_stats()[[2]]
  
  if(!is.null(df)){
    
    lst <- lapply(1:nrow(df), function(i){
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$value[i]),
          argonTableItem(df$n[i]),
          argonTableItem(df$percent[i])
        )
      )
    })
    
    argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
  }
  
})

### Graphical distribution of continuous and categorical variables
observeEvent(input$modal_tutorial_global_statistics, {
  
  df <- tutorial()
  stats <- tutorial_stats()[[2]]
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        
        lapply(0:ncol(df), function(i){
          
          if(i == 0){
            
            newdata <- df[, apply(df, 2, function(x) length(unique(x))) > 7]
            newdata2 <- melt(newdata)
            newdata3 <- cbind(newdata2[,1:2], value = log10(newdata2$value))
            
            tagList(
              renderfusionPlot({
                fusionPlot(data = newdata2, x = "variable", y = "value", type = "boxandwhisker2d") %>%
                  fusionCaption(caption = "Global distribution") %>%
                  fusionSubcaption(subcaption = "Continuous variables") %>%
                  fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                  fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
              }),
              renderfusionPlot({
                fusionPlot(data = newdata3, x = "variable", y = "value", type = "boxandwhisker2d") %>%
                  fusionCaption(caption = "Global distribution") %>%
                  fusionSubcaption(subcaption = "Continuous variables") %>%
                  fusionAxis(xAxisName = "variable", yAxisName = "log (value)") %>%
                  fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
              })
            )
            
          } else if(length(unique(df[,i])) > 7){
            shap <- signif(shapiro.test(df[,i])$p.value, 2)
            dense <- density(na.omit(df[,i]))
            dense2 <- data.frame(var1 = dense[["x"]], var2 = dense[["y"]])
            renderfusionPlot({
              fusionPlot(data = dense2, x = "var1",y = "var2", type = "area2d") %>%
                fusionAxis(xAxisName = NULL, yAxisName = "Density") %>%
                fusionCustomAxis(showlabels = FALSE) %>%
                fusionCaption(caption = paste("Distribution of", names(df)[i])) %>%
                fusionSubcaption(subcaption = paste0("Shapiro's test p-value = ", shap, ", on ", length(na.omit(df[,i])), " obs.")) %>%
                fusionPalette(palettecolors = "#5E72E3")
            })
          } else {
            newdata <- stats[stats$biomarker == names(df)[i], ]
            renderfusionPlot({
              newdata %>%
                fusionPlot(
                  x = "value",
                  y = "n",
                  type = "column2d",
                ) %>% 
                fusionCaption(caption = paste("Distribution of", newdata$biomarker[1])) %>%
                fusionSubcaption(subcaption = paste("The variable is composed of", length(na.omit(df[,i])), "obs.")) %>%
                fusionAxis(xAxisName = "Groups", yAxisName = "Count") %>%
                fusionPalette(palettecolors =  c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9"))
            })
          }
        })
        
      )
    )
  )
  
})

### Documentation about Univariate analysis
observeEvent(input$univariate_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Univariate Analysis"),
        tags$br(),
        tags$br(),
        "Univariate analysis refers to a set of methods including statistical 
        metrics to describe singular variables present in a tabular dataset.",
        tags$br(),
        tags$br(),
        tags$ul(
          "For the continuous variables,",
          tags$li(tags$b("BIOMARKER"), ": the name of a continuous biomarker"),
          tags$li(tags$b("N"), ": the number of values present in the variable"),
          tags$li(tags$b("N DISTINCT"), ": the number of unique values present in the variable"),
          tags$li(tags$b("MIN"), ": the minimal value present in the variable"),
          tags$li(tags$b("MEDIAN"), ": the median value of the variable"),
          tags$li(tags$b("MEAN"), ": the mean value of the variable"),
          tags$li(tags$b("SD"), ": the standard deviation of the variable"),
          tags$li(tags$b("IQR"), ": the interquartile range of the variable"),
          tags$li(tags$b("NA's"), ": the number of missing values present in the variable"),
          tags$li(tags$b("MAX"), ": the maximal value present in the variable"),
          tags$li(tags$b("SHAPIRO'S TEST"), ": the", tags$b("p-value*")," of the Shapiro-Wilk test (Normality test)"),
          tags$li(tags$b("NORMALITY"), ": the color indicator of the normal distribution")
        ),
        tags$ul(
          "For the categorical variables,",
          tags$li(tags$b("BIOMARKER"), ": the name of a categorical biomarker"),
          tags$li(tags$b("VALUE"), ": the name of a category present in the variable"),
          tags$li(tags$b("N"), ": the count frequency distribution of a category"),
          tags$li(tags$b("PERCENT"), ": the absolute (%) frequency distribution of a category")
        ),
        tags$br(),
        tags$i("* A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})

################################ Bivariate #####################################

### Display the bivariate table (Continuous)
output$geneseng_tutorial_statistics_bygroup <- renderUI({
  
  number <- number()
  df <- tutorial_stats_group()[[1]]
  
  if(!is.null(df) & vars()[number,"type"] == "Classification"){
    
    lst <- lapply(1:nrow(df), function(i){
      
      if(df$normality[i] == "no"){
        normality <- argonBadge(text = df$normality[i], status = "danger")
      } else {
        normality <- argonBadge(text = df$normality[i], status = "success")
      }
      
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$group[i]),
          argonTableItem(df$n[i]),
          argonTableItem(df$`n distinct`[i]),
          argonTableItem(df$min[i]),
          argonTableItem(df$median[i]),
          argonTableItem(df$mean[i]),
          argonTableItem(df$sd[i]),
          argonTableItem(df$iqr[i]),
          argonTableItem(df$max[i]),
          argonTableItem(df$`NA's`[i]),
          argonTableItem(df$`Shapiro's test`[i]),
          argonTableItem(normality)
        )
      )
    })
    
    tbl <- argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
    return(tbl)
    
  } else {
    tags$p("No data available")
  }
  
})

### Display the bivariate table (Categorical)
output$geneseng_tutorial_statistics_bygroup02 <- renderUI({
  
  number <- number()
  df <- tutorial_stats_group()[[2]]
  
  if(!is.null(df) & vars()[number,"type"] == "Classification"){
    
    lst <- lapply(1:nrow(df), function(i){
      
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$group[i]),
          argonTableItem(df$value[i]),
          argonTableItem(df$n[i]),
          argonTableItem(df$percent[i])
        )
      )
    })
    
    tbl <- argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
    return(tbl)
    
  }
  
})


# Bivariate helper
observeEvent(input$bivariate_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Bivariate Analysis"),
        tags$br(),
        tags$br(),
        "Bivariate analysis refers to a set of methods including statistical 
        metrics to describe the relationship between two variables present 
        in a tabular dataset. In our case, all variables are evaluated with the 
        target variable.",
        tags$br(),
        tags$br(),
        tags$ul(
          "For the continuous variables,",
          tags$li(tags$b("BIOMARKER"), ": the name of a continuous biomarker"),
          tags$li(tags$b("GROUP"), ": the name of the target variable"),
          tags$li(tags$b("N"), ": the number of values present in the variable"),
          tags$li(tags$b("N DISTINCT"), ": the number of unique values present in the variable"),
          tags$li(tags$b("MIN"), ": the minimal value present in the variable"),
          tags$li(tags$b("MEDIAN"), ": the median value of the variable"),
          tags$li(tags$b("MEAN"), ": the mean value of the variable"),
          tags$li(tags$b("SD"), ": the standard deviation of the variable"),
          tags$li(tags$b("IQR"), ": the interquartile range of the variable"),
          tags$li(tags$b("NA's"), ": the number of missing values present in the variable"),
          tags$li(tags$b("MAX"), ": the maximal value present in the variable"),
          tags$li(tags$b("SHAPIRO'S TEST"), ": the", tags$b("p-value*")," of the Shapiro-Wilk test (Normality test)"),
          tags$li(tags$b("NORMALITY"), ": the color indicator of the normal distribution")
        ),
        tags$ul(
          "For the categorical variables,",
          tags$li(tags$b("BIOMARKER"), ": the name of a categorical biomarker"),
          tags$li(tags$b("GROUP"), ": the name of the target variable"),
          tags$li(tags$b("VALUE"), ": the name of a category present in the variable"),
          tags$li(tags$b("N"), ": the count frequency distribution of a category"),
          tags$li(tags$b("PERCENT"), ": the absolute (%) frequency distribution of a category")
        ),
        tags$br(),
        tags$i("* A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})




# Tutorial : Display Statistical by group graphics (Analysis > Statistics)
observeEvent(input$modal_tutorial_statistics_bygroup, {
  
  number <- number()
  df <- tutorial()
  metrics <- genesengStats::geneseng_summary_stats(data = df, group = vars()[number,"target"])
  
  fills <- rep(
    c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9"),
    length.out = length(unique(df[,vars()[number,"target"]]))
  )
  
  newdata <- split(x = df, f = df[,vars()[number,"target"]])
  
  # Compute the p-values
  #res <- NULL
  #for(i in 1:length(newdata)){
  #  tmp <- newdata[[i]]
  #  for(i in 1:ncol(tmp)){
  #    if(length(unique(df[,i])) > 7){
  #      pvalue <- signif(shapiro.test(tmp[,i])$p.value, 2)
  #      res <- c(res, pvalue)
  #    }
  #  }
  #}
  
  #mtx <- matrix(data = res, ncol = length(newdata), byrow = FALSE)
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(0:ncol(df), function(i){
          
          if(i == 0){
            
            newdf <- df[, apply(df, 2, function(x) length(unique(x))) > 7]
            newdf2 <- cbind(newdf, target = df[,vars()[number,"target"]])
            newdf3 <- reshape2::melt(newdf2, id = "target")
            newdf4 <- cbind(newdf3[,1:2], value = log10(newdf3$value))
            
            tagList(
              renderFusionMultiPlot({
                fusionMultiPlot(
                  data = newdf3, 
                  x = "variable", 
                  y = "value", 
                  col = "target", 
                  type = "boxandwhisker2d"
                ) %>%
                  fusionCaption(caption = "Global distribution") %>%
                  fusionSubcaption(subcaption = "Continuous variables") %>%
                  fusionAxis(xAxisName = "variable", yAxisName = "value") %>%
                  fusionPalette(palettecolors = c(rep(x = fills, each = 2)))
              }),
              renderFusionMultiPlot({
                fusionMultiPlot(
                  data = newdf4, 
                  x = "variable", 
                  y = "value", 
                  col = "target", 
                  type = "boxandwhisker2d"
                ) %>%
                  fusionCaption(caption = "Global distribution") %>%
                  fusionSubcaption(subcaption = "Continuous variables") %>%
                  fusionAxis(xAxisName = "variable", yAxisName = "log(value)") %>%
                  fusionPalette(palettecolors = c(rep(x = fills, each = 2)))
              })
            )
            
          } else if(length(unique(df[,i])) > 7 & names(df)[i] != vars()[number,"target"]){
            
            lst <- lapply(1:length(newdata), function(x){
              dense <- density(na.omit(newdata[[x]][,i]))
              data.frame(
                var1 = dense[["x"]], 
                var2 = dense[["y"]],
                col = unique(df[,vars()[number,"target"]])[x]
              )
            })
            
            renderFusionMultiPlot({
              do.call(rbind, lst) %>%
                fusionMultiPlot(
                  x = "var1", 
                  y = "var2",
                  col = "col",
                  type = "msarea"
                ) %>%
                fusionCaption(caption = paste("Distribution of", names(df)[i])) %>%
                fusionSubcaption(
                  subcaption = ""
                  #toString(
                  #  paste0(
                  #    levels(factor(df[, vars()[number,"target"]])), 
                  #    " - Shapiro's test p-value = ",
                  #    mtx[i,]
                  #    )
                  #  )
                ) %>%
                fusionCustomAxis(showlabels = FALSE) %>%
                fusionAxis(xAxisName = "", yAxisName = "Frequency") %>%
                fusionPalette(
                  palettecolors = c(
                    rep(
                      x = fills, 
                      length.out = length(unique(df[,vars()[number,"target"]]))
                    )
                  ),
                  plotFillAlpha = "80"
                )
              })
            
          } else {
            
            if(names(df)[i] != vars()[number,"target"]){
              
              df2 <- reshape2::melt(df, id = vars()[number,"target"]) %>%
                filter(variable == names(df)[i])
              
              df3 <- table(df2[,1], df2$value) %>%
                reshape2::melt()
              
              renderFusionMultiPlot({
                df3 %>%
                  fusionMultiPlot(
                    x = "Var1",
                    y = "value",
                    col = "Var2",
                    type = "mscolumn2d"
                  ) %>%
                  fusionPalette(
                    palettecolors = rep(
                      c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9"), 
                      length.out = nrow(df3)
                      )
                    ) %>%
                  fusionCaption(caption = paste("Distribution of", names(df)[i])) %>%
                  fusionSubcaption(subcaption = "") %>%
                  fusionAxis(xAxisName = "", yAxisName = "Frequency")
              })
            }
            
          }
        })
      )
    )
  )
})


################################ Multivariate ##################################

observeEvent(input$modal_tutorial_statistics_PCA, {
  
  # PCA
  df <- tutorial()
  res_pca <- tutorial_multivariate_pca()
  number <- number()
  target <- vars()[number,"target"]
  
  # Explained Variances
  eig_values <- res_pca$eig
  explained_var <- data.frame(comp = rownames(eig_values), var = eig_values[,2])
  
  # Individuals
  ind_pca <- res_pca[["ind"]]
  pos_ind_pca <- ind_pca$coord
  combs <- c(0, combn(x = ncol(pos_ind_pca), m = 2, simplify = FALSE))
  
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(combs, function(x){
          if(x == 0){
            renderfusionPlot({
              explained_var %>%
                fusionPlot(x = "comp", y = "var", type = "pareto2d") %>%
                fusionCaption(caption = "Explained Variances") %>%
                fusionSubcaption(subcaption = "Distribution of principal components") %>%
                fusionAxis(xAxisName = "Principal Components", yAxisName = "% of variances") %>%
                fusionPalette(palettecolors = c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9"))
            })
          } else {
            
            dimX <- paste(colnames(pos_ind_pca)[x[1]], paste0("(", round(eig_values[x[1],2], 2), "%)"))
            dimY <- paste(colnames(pos_ind_pca)[x[2]], paste0("(", round(eig_values[x[2],2], 2), "%)"))
            subcaption <- paste(paste0("Dim.", x[1]), "&", paste0("Dim.", x[2]))
            
            if(vars()[number,"type"] == "Classification"){
              newdata <- cbind.data.frame(pos_ind_pca[,x], target = df[,target])
              argonRow(
                argonColumn(
                  width = 12,
                  renderFusionMultiPlot({
                    newdata %>%
                      fusionMultiPlot(
                        x = colnames(newdata)[1], 
                        y = colnames(newdata)[2], 
                        col = "target", 
                        type = "scatter"
                      ) %>%
                      fusionCaption(caption = "PCA graph of individuals") %>%
                      fusionSubcaption(subcaption = subcaption) %>%
                      fusionAxis(xAxisName = dimX, yAxisName = dimY) %>%
                      fusionAnchors(anchorBgColor = "#5E72E3", anchorBorderColor = "#5E72E3")
                  })
                )
              )
            } else {
              newdata <- as.data.frame(pos_ind_pca[,x])
              argonRow(
                argonColumn(
                  width = 12,
                  renderfusionPlot({
                    newdata %>%
                      fusionPlot(
                        x = colnames(newdata)[1], 
                        y = colnames(newdata)[2],
                        type = "scatter"
                      ) %>%
                      fusionCaption(caption = "PCA graph of individuals") %>%
                      fusionSubcaption(subcaption = subcaption) %>%
                      fusionAxis(xAxisName = dimX, yAxisName = dimY) %>%
                      fusionAnchors(anchorBgColor = "#5E72E3", anchorBorderColor = "#5E72E3")
                  })
                )
              )
            }
          } 
        })
      )
    )
  )
})

# Multivariate helper
observeEvent(input$multivariate_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Multivariate Analysis"),
        tags$br(),
        tags$br(),
        "Multivariate analysis refers to a set of methods to describe multiple 
        variables present in a tabular dataset.",
        tags$br(),
        tags$br(),
        tags$b("Principal Component Analysis"),
        tags$br(),
        tags$br(),
        "Principal Component Analysis (PCA) is a technique for reducing the 
        dimensionality of a dataset, increasing interpretability and at the same 
        time minimizing information loss. The input variables generate new
        uncorrelated variables that successively maximize variance [1].",
        tags$br(),
        tags$br(),
        tags$b("References"),
        tags$br(),
        tags$br(),
        "[1] ", tags$b("Jolliffe et al "), "(2016): Principal component analysis: 
        a review and recent developments"
      )
    )
  )
})

# PCA
output$contribution_pca <- renderUI({
  
  res_pca <- tutorial_multivariate_pca()
  var_pca <- res_pca[["var"]]
  df <- apply(var_pca$contrib, 2, round, 2)
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(0:ncol(df), function(j){
        
        if(j == 0){
          argonTableItem(rownames(df)[i])
        } else {
          argonTableItem(df[i,j])
        }
        
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    tags$div(
      style = "overflow-y:auto; height: 500px;",
      argonTable(
        cardWrap = TRUE,
        headTitles = c(" ",colnames(df)),
        tagList(
          lst
        )
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})


#################################### Tests #####################################

# Tutorial : Display Statistical Tests (Analysis > Tests)
output$geneseng_tutorial_statistical_tests <- renderUI({
  
  df <- tutorial_tests()[[1]]
  
  if(!is.null(df)){
    
    lst <- lapply(1:nrow(df), function(i){
      
      # Binary class for continuous variables
      if(!("One-way Anova" %in% names(df))){
        if("Paired t-test" %in% names(df)){
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Mann-Whitney's test`[i]),
              argonTableItem(df$`F-test`[i]),
              argonTableItem(df$`T-test`[i]),
              argonTableItem(df$`Welch's t-test`[i]),
              argonTableItem(df$`Paired t-test`[i]),
              argonTableItem(df$`Wilcox's test`[i])
            )
          )
        } else {
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Mann-Whitney's test`[i]),
              argonTableItem(df$`F-test`[i]),
              argonTableItem(df$`T-test`[i]),
              argonTableItem(df$`Welch's t-test`[i])
            )
          )
        }

      } else {
        
        # Multiclass for continuous variables
        if("Friedman's test" %in% names(df)){
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Kruskal-Wallis's test`[i]),
              argonTableItem(df$`Bartlett's test`[i]),
              argonTableItem(df$`One-way Anova`[i]),
              argonTableItem(df$`Friedman's test`[i]),
              argonTableItem(df$`One-way Anova with RM`[i])
            )
          )
        } else {
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Kruskal-Wallis's test`[i]),
              argonTableItem(df$`Bartlett's test`[i]),
              argonTableItem(df$`One-way Anova`[i])
            )
          )
        }
      }
    })
    
    tbl <- argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
    return(tbl)
    
  } else {
    tags$p("No data available")
  }
  
})

# Tutorial : Display Statistical tests boxplot (Analysis > Tests)
observeEvent(input$modal_tutorial_statistical_tests, {
  number <- number()
  df <- tutorial()
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(1:ncol(df), function(i){
          if(length(unique(df[,i])) > 7){
            renderfusionPlot({
              df %>%
                fusionPlot(x = vars()[number,"target"], y = names(df[i]), type = "boxandwhisker2d") %>%
                fusionPalette(palettecolors = c("#5E72E3", "#5E72E3")) %>%
                fusionCaption(caption = paste0("Comparison (", names(df)[i], ")")) %>%
                fusionSubcaption(subcaption = NULL) %>%
                fusionAxis(xAxisName = vars()[number,"target"], yAxisName = names(df[i]))
            })
          }
        })
      )
    )
  )
})


# Analysis > Continuous variable > i
observeEvent(input$statistcal_tests_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Statistical tests for continuous data"),
        tags$br(),
        tags$br(),
        "A statistical test is a method of statistical inference used as a making-decision 
        tool to validate or invalidate a particular hypothesis. In our case, the statistical 
        test are used to :",
        tags$ul(
          tags$li("Evaluate the normal distribution of a sample."),
          tags$li("Evaluate the homoscedasticity of two or more samples."),
          tags$li("Compare the ranks or means of two or more samples.")
        ),
        tags$br(),
        tags$i("The p-value of the Shapiro's test can be retrieved at Analysis > 
        Statistics > Univariate."),
        tags$br(),
        tags$br(),
        tags$b("Tests for two samples"),
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("Mann-Whitney's test"), ": is a nonparametric test used to compare two independent samples."),
          tags$li(tags$b("Wilcox's test"), ": is a nonparametric test used to compare two linked samples."),
          tags$li(tags$b("F-test"), ": is used to evaluate the homoscedasticity (homogeneity of variances) of two samples."),
          tags$li(tags$b("T-test"), ": is a parametric test used to compare two independent samples with equal variances."),
          tags$li(tags$b("Welch T-test"), ": is a parametric test used to compare two independent samples with unequal variances."),
          tags$li(tags$b("Paired T-test"), ": is a parametric test used to compare two linked samples with equal variances.")
        ),
        tags$br(),
        tags$img(
          src = "assets/img/pipeline/statistical_test01.png", 
          height = "550px", 
          width = "100%"
        ),
        tags$br(),
        tags$b("Tests for two or more samples"),
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("Kruskal-Wallis's test"), ": is a non parametric test used to compare two or more independent samples."),
          tags$li(tags$b("Bartlett's test"), ": is the extension of the  F-test."),
          tags$li(tags$b("One-way Anova"), ": is a parametric test used to compare two or more independent samples."),
          tags$li(tags$b("Friedman's test"), ": is a non parametric test used to compare two or more linked samples."),
          tags$li(tags$b("One-way Anova with repeated measures"), ": is a parametric test used to compare two or more linked samples.")
        ),
        tags$br(),
        tags$img(
          src = "assets/img/pipeline/statistical_test02.png", 
          height = "550px", 
          width = "100%"
        ),
        tags$br(),
        tags$i("A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})


output$geneseng_tutorial_statistical_tests2 <- renderUI({
  
  df <- tutorial_tests()[[2]]
  
  if(!is.null(df)){
    
    lst <- lapply(1:nrow(df), function(i){
      
      if("McNemar's test" %in% names(df)){
        
        tmp <- tagList(
          argonTableItems(
            argonTableItem(df$biomarker[i]),
            argonTableItem(df$group[i]),
            argonTableItem(df$`Chi-squared test`[i]),
            argonTableItem(df$`Fisher's exact test`[i]),
            argonTableItem(df$`McNemar's test`[i])
          )
        )
        
      } else {
        
        if("Cochran's Q test" %in% names(df)){
          
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Chi-squared test`[i]),
              argonTableItem(df$`Cochran's Q test`[i])
            )
          )
          
        } else {
          
          tmp <- tagList(
            argonTableItems(
              argonTableItem(df$biomarker[i]),
              argonTableItem(df$group[i]),
              argonTableItem(df$`Chi-squared test`[i])
            )
          )
          
        }
      }
      
    })
    
    tbl <- argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
    return(tbl)
    
  } else {
    tags$p("No data available")
  }
  
})


# Categorical (Btn)
observeEvent(input$modal_tutorial_statistical_tests02, {
  
  df <- tutorial()
  number <- number()
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(1:ncol(df), function(i){
          if(length(unique(df[,i])) <= 7 & names(df)[i] != vars()[number,"target"]){
            renderFusionMultiPlot({
              df %>%
                fusionMultiPlot(x = vars()[number,"target"], y = names(df)[i], col = vars()[number,"target"], type = "mscolumn2d") %>%
                fusionPalette(palettecolors = c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9")) %>%
                fusionCaption(caption = paste("Distribution of", names(df)[i])) %>%
                fusionSubcaption(subcaption = NULL) %>%
                fusionAxis(xAxisName = vars()[number,"target"], yAxisName = names(df)[i])
            })
          }
        })
      )
    )
  )
})

# Analysis > Categorical variable > i
observeEvent(input$statistcal_tests02_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Statistical tests for categorical data"),
        tags$br(),
        tags$br(),
        "A statistical test is a method of statistical inference used as a making-decision 
        tool to validate or invalidate a particular hypothesis.",
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("Fisher's exact test"), ": is a parametric test used to evaluate the relationship of two independent samples."),
          tags$li(tags$b("Chi-squared test"), ": is a parametric test used to evaluate the relationship of two or more independent samples."),
          tags$li(tags$b("McNemar's test"), ": is a non parametric test used to evaluate the relationship of two paired samples."),
          tags$li(tags$b("Cochran's Q test"), ": is a non parametric test used to evaluate the relationship of two or more paired samples.")
        ),
        tags$img(
          src = "assets/img/pipeline/statistical_test03.png", 
          height = "500px", 
          width = "100%"
        ),
        tags$i("A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})

############################### Correlation ####################################

# Tutorial : Display Continuous Correlation (Analysis > Correlations)
output$geneseng_correlation <- renderUI({
  
  df <- tutorial_correlation()[[1]]
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(0:ncol(df), function(j){
        if(j == 0){
          argonTableItem(rownames(df)[i])
        } else {
          argonTableItem(df[i,j])
        }
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    tags$div(
      style = "overflow-y:auto; height: 500px;",
      argonTable(
        cardWrap = TRUE,
        headTitles = c(" ",colnames(df)),
        tagList(
          lst
        )
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})


# Tutorial : Display Continuous Correlation graphics (Analysis > Correlations)
observeEvent(input$modal_tutorial_correlation, {
  
  number <- number()
  df <- tutorial()
  target <- vars()[number,"target"]
  corr <- tutorial_correlation()[[1]]
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        
      lapply(0:nrow(corr), function(i){
        
        if(i == 0){
          
          df2 <- df[,unique(c(corr$Var1, corr$Var2))]
          spearman_corr <- cor(df2, method = "spearman", use = "pairwise.complete.obs")
          spearman_corr <- apply(spearman_corr, 2, round, 3)
          
          renderfusionPlot({
            fusionPlot(data = spearman_corr, type = "heatmapCorr") %>%
              fusionCaption(caption = "Correlation summary") %>%
              fusionSubcaption(subcaption = "Spearman's coefficient was used to establish relationships") %>%
              fusionAxis(xAxisName = NULL, yAxisName = NULL) %>%
              fusionLegend(legendCaption = "")
          })
          
        } else {
          
          if(vars()[number,"type"] == "Classification"){
            renderFusionMultiPlot({
              fusionMultiPlot(data = df, x = corr$Var1[i], y = corr$Var2[i], col = target, type = "scatter") %>%
                fusionCaption(caption = paste(corr$Var1[i], "&", corr$Var2[i])) %>%
                fusionSubcaption(subcaption = paste("Spearman's coefficient =", corr$coeff[i])) %>%
                fusionAnchors(anchorBgColor = "#5E72E3", anchorBorderColor = "#5E72E3") %>%
                fusionAxis(xAxisName = corr$Var1[i], yAxisName = corr$Var2[i]) %>%
                fusionTheme(theme = "fusion")
            })
          } else {
            renderFusionMultiPlot({
              fusionMultiPlot(data = df, x = corr$Var1[i], y = corr$Var2[i], col = target, type = "scatter") %>%
                fusionCaption(caption = paste(corr$Var1[i], "&", corr$Var2[i])) %>%
                fusionSubcaption(subcaption = paste("Spearman's coefficient =", corr$coeff[i])) %>%
                fusionAnchors(anchorBgColor = "#5E72E3", anchorBorderColor = "#5E72E3") %>%
                fusionAxis(xAxisName = corr$Var1[i], yAxisName = corr$Var2[i]) %>%
                fusionLegend(showLegend = FALSE) %>%
                fusionTheme(theme = "fusion")
            })
          }
        }
      })
      )
    )
  )
})

# Continuous correlation helper
observeEvent(input$continuous_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Continuous/Continuous relations"),
        tags$br(),
        tags$br(),
        "The Spearman's rank correlation coefficient is a nonparametric measure of 
        rank correlation used to evaluate the relationship between two variables. 
        The coefficient range is bound between -1 and +1 traducing negative, positive 
        or independent correlation.",
        tags$br(),
        tags$br(),
        "For continuous/continuous relationship:",
        tags$ul(
          tags$li(tags$b("Var1"), ": a continuous variable"),
          tags$li(tags$b("Var2"), ": a continuous variable"),
          tags$li(tags$b("Method"), ": the correlation coefficient method"),
          tags$li(tags$b("Coeff"), ": the Spearman's rank correlation coefficient"),
          tags$li(tags$b("Lower"), ": the lower bound of the confidence interval"),
          tags$li(tags$b("Upper"), ": the upper bound of the confidence interval"),
          tags$li(tags$b("Pvalue"), ": the ", tags$b("p-value*")," of the Spearman's rho test")
        ),
        tags$i("*A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})


# Tutorial : Display Continuous/Categorical Correlation (Analysis > Correlations)
output$geneseng_correlation2 <- renderUI({
  
  df <- tutorial_correlation()[[2]]
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(0:ncol(df), function(j){
        if(j == 0){
          argonTableItem(rownames(df)[i])
        } else {
          argonTableItem(df[i,j])
        }
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    tags$div(
      style = "overflow-y:auto; height: 500px;",
      argonTable(
        cardWrap = TRUE,
        headTitles = c(" ",colnames(df)),
        tagList(
          lst
        )
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})

# Tutorial : Display Continuous/Categorical Correlation graphics (Analysis > Correlations)
observeEvent(input$modal_tutorial_correlation2, {
  
  df <- tutorial()
  corr <- tutorial_correlation()[[2]]
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(0:nrow(corr), function(i){
          if(i == 0){
            
            df2 <- df[,unique(c(corr$Var1, corr$Var2))]
            
            for(i in unique(corr$Var1)){
              df2[,i] <- as.numeric(factor(df2[,i]))
            }
            
            spearman_corr <- cor(df2, method = "spearman", use = "pairwise.complete.obs")
            spearman_corr <- apply(spearman_corr, 2, round, 3)
            
            renderfusionPlot({
              fusionPlot(data = spearman_corr, type = "heatmapCorr") %>%
                fusionCaption(caption = "Correlation summary") %>%
                fusionSubcaption(subcaption = "Spearman's coefficient was used to establish relationships") %>%
                fusionAxis(xAxisName = NULL, yAxisName = NULL) %>%
                fusionLegend(legendCaption = "")
            })
            
            } else {
            renderfusionPlot({
              fusionPlot(
                data = df,
                x = corr$Var1[i],
                y = corr$Var2[i],
                type = "boxandwhisker2d"
              ) %>%
                fusionCustomBoxplot(drawmeanconnector = TRUE) %>%
                fusionCaption(caption = "Point biserial correlation") %>%
                fusionSubcaption(
                  subcaption = paste0(
                    "Spearman's coefficient (", corr$Var2[i], " & ", corr$Var1[i],
                    ") = ", corr$coeff[i]
                  )
                ) %>%
                fusionAxis(xAxisName = corr$Var1[i], yAxisName = corr$Var2[i]) %>%
                fusionPalette(palettecolors = c("#5E72E3", "#5E72E3"))
            })
          }
        })
      )
    )
  )
})

# Continuous/Categorical helper
observeEvent(input$continuous_categorical_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Continuous/Categorical relations"),
        tags$br(),
        tags$br(),
        "The Spearman's rank correlation coefficient is a nonparametric measure of 
        rank correlation used to evaluate the relationship between two variables. 
        The coefficient range is bound between -1 and +1 traducing negative, positive 
        or independent correlation.",
        tags$br(),
        tags$br(),
        "For continuous/categorical relationship:",
        tags$ul(
          tags$li(tags$b("Var1"), ": a categorical variable"),
          tags$li(tags$b("Var2"), ": a continuous variable"),
          tags$li(tags$b("Method"), ": the correlation coefficient method"),
          tags$li(tags$b("Coeff"), ": the Spearman's rank correlation coefficient"),
          tags$li(tags$b("Lower"), ": the lower bound of the confidence interval"),
          tags$li(tags$b("Upper"), ": the upper bound of the confidence interval"),
          tags$li(tags$b("Pvalue"), ": the ", tags$b("p-value*")," of the Spearman's rho test")
        ),
        tags$i("*A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})

# Categorical relationship
output$geneseng_correlation3 <- renderUI({
  
  df <- tutorial_correlation()[[3]]
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- lapply(0:ncol(df), function(j){
        if(j == 0){
          argonTableItem(rownames(df)[i])
        } else {
          argonTableItem(df[i,j])
        }
      })
      argonTableItems(
        tagList(tmp)
      )
    })
    
    tags$div(
      style = "overflow-y:auto; height: 500px;",
      argonTable(
        cardWrap = TRUE,
        headTitles = c(" ",colnames(df)),
        tagList(
          lst
        )
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})


observeEvent(input$modal_tutorial_correlation3, {
  
  df <- tutorial()
  corr <- tutorial_correlation()[[3]]
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      shinycssloaders::withSpinner(
        type = 3,
        color = getOption("spinner.color", default = "#5E72E3"),
        color.background = "white",
        lapply(1:nrow(corr), function(x){
          renderFusionMultiPlot({
            df %>%
              fusionMultiPlot(x = corr$Var1[x], y = corr$Var2[x], col = corr$Var1[x], type = "mscolumn2d") %>%
              fusionCaption(caption = paste(corr$Var1[x], "&", corr$Var2[x])) %>%
              fusionSubcaption(subcaption = paste("Chi-squared test p-value =", corr$pvalue[x])) %>%
              fusionAxis(xAxisName = corr$Var1[x], yAxisName = corr$Var2[x]) %>%
              fusionPalette(palettecolors =  c("#5E72E3", "#FF595E", "#FFCA3A", "#8AC926", "#FB5607", "#00bbf9")) %>%
              fusionTheme(theme = "fusion")
          })
        })
      )
    )
  )
})

# Categorical
observeEvent(input$categorical_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Categorical/Categorical relations"),
        tags$br(),
        tags$br(),
        "The chi-squared test is a statistical test used to evaluate the relationship 
        between two categorical variables. A Yates's continuity correction is 
        performed on very little samples.",
        tags$br(),
        tags$br(),
        "For categorical/categorical relationship:",
        tags$ul(
          tags$li(tags$b("Var1"), ": a categorical variable"),
          tags$li(tags$b("Var2"), ": a categorical variable"),
          tags$li(tags$b("Method"), ": the Chi-squared test with or without Yate's continuity correction"),
          tags$li(tags$b("parameter"), ": the degrees of freedom of the approximate chi-squared"),
          tags$li(tags$b("statistic"), ": the value of the chi-squared test statistic."),
          tags$li(tags$b("Pvalue"), ": the ", tags$b("p-value*")," of the Chi-squared test")
        ),
        tags$i("*A p-value less than 0.05 is statistically significant.")
      )
    )
  )
})


################################## Performances ################################

### Switch binary classificationto Regression
output$switch_ml_problem <- renderUI({
  
  number <- number()
  
  if(vars()[number,"type"] == "Classification"){
    argonH1("One vs One", display = 4)
  } else {
    argonH1("Regression", display = 4)
  }

})

### Display the best Features table
output$geneseng_prototype_best_features <- renderUI({
  
  df <- tutorial_best_features()
  
  if(!is.null(df)){
    if("class1" %in% names(df)){
      lst <- lapply(1:nrow(df), function(i){
        tmp <- tagList(
          argonTableItems(
            argonTableItem(df$biomarker[i]),
            argonTableItem(df$class1[i]),
            argonTableItem(df$class2[i]),
            argonTableItem(df$AIC[i]),
            argonTableItem(df$best.method[i]),
            argonTableItem(df$threshold[i]),
            argonTableItem(df$auc[i]),
            argonTableItem(df$auc_Lower[i]),
            argonTableItem(df$auc_Upper[i]),
            argonTableItem(df$`DeLong's test`[i])
          )
        )
      })

    } else {
      lst <- lapply(1:nrow(df), function(i){
        tmp <- tagList(
          argonTableItems(
            argonTableItem(df$biomarker[i]),
            argonTableItem(df$target[i]),
            argonTableItem(df$model[i]),
            argonTableItem(df$AIC[i]),
            argonTableItem(df$rmse[i]),
            argonTableItem(df$mse[i]),
            argonTableItem(df$mae[i])
          )
        )
      })
    }
    
    argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})



### Display the One vs One table
output$geneseng_tutorial_individual_performance <- renderUI({
  
  number <- number()
  df <- tutorial_performance()
  
  if(length(df) == 2){
    df <- df[[2]]
  }
  
  if(vars()[number,"type"] == "Classification"){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$class1[i]),
          argonTableItem(df$class2[i]),
          argonTableItem(df$model[i]),
          argonTableItem(df$logLoss[i]),
          argonTableItem(df$best.method[i]),
          argonTableItem(df$threshold[i]),
          argonTableItem(df$auc[i]),
          argonTableItem(df$sens[i]),
          argonTableItem(df$spe[i]),
          argonTableItem(df$PPV[i]),
          argonTableItem(df$NPV[i]),
          argonTableItem(df$accuracy[i]),
          argonTableItem(df$no.info.rate[i]),
          argonTableItem(df$balanced.accuracy[i]),
          argonTableItem(df$precision[i]),
          argonTableItem(df$f1[i]),
          argonTableItem(argonBadge(text = df$TP[i], status = "success")),
          argonTableItem(argonBadge(text = df$FP[i], status = "danger")),
          argonTableItem(argonBadge(text = df$TN[i], status = "success")),
          argonTableItem(argonBadge(text = df$FN[i], status = "danger"))
        )
      )
    })
    
  } else {
    lst <- lapply(1:nrow(df), function(i){
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$target[i]),
          argonTableItem(df$model[i]),
          argonTableItem(df$rmse[i]),
          argonTableItem(df$mse[i]),
          argonTableItem(df$mae[i])
        )
      )
    })
  }
  
  
  tbl <- argonTable(
    cardWrap = TRUE,
    headTitles = names(df),
    tagList(
      lst
    )
  )
  
  return(tbl)
  
})


### Display the One vs All table (only for Multiclass)
output$geneseng_tutorial_individual_performance02 <- renderUI({
  
  if(length(tutorial_performance()) != 2){
    df <- NULL
  } else {
    df <- tutorial_performance()[[1]]
  }
  
  if(!is.null(df)){
    lst <- lapply(1:nrow(df), function(i){
      tmp <- tagList(
        argonTableItems(
          argonTableItem(df$biomarker[i]),
          argonTableItem(df$class1[i]),
          argonTableItem(df$class2[i]),
          argonTableItem(df$model[i]),
          argonTableItem(df$multiLogLoss[i]),
          argonTableItem(df$sens[i]),
          argonTableItem(df$spe[i]),
          argonTableItem(df$PPV[i]),
          argonTableItem(df$NPV[i]),
          argonTableItem(df$accuracy[i]),
          argonTableItem(df$no.info.rate[i]),
          argonTableItem(df$balanced.accuracy[i]),
          argonTableItem(df$precision[i]),
          argonTableItem(df$f1[i]),
          argonTableItem(argonBadge(text = df$TP[i], status = "success")),
          argonTableItem(argonBadge(text = df$FP[i], status = "danger")),
          argonTableItem(argonBadge(text = df$TN[i], status = "success")),
          argonTableItem(argonBadge(text = df$FN[i], status = "danger"))
        )
      )
    })
    
    argonTable(
      cardWrap = TRUE,
      headTitles = names(df),
      tagList(
        lst
      )
    )
    
  } else {
    tags$p("No data available")
  }
  
})



### Display the ROC curves & Confusion Matrix (Only for Classification)
observeEvent(input$modal_tutorial_individual_Performance, {
  
  number <- number()
  df <- tutorial()
  group <- vars()[number,"target"]
  metrics <- tutorial_performance()
  
  if(length(metrics) == 2){
    metrics <- metrics[[2]]
  }
  
  showModal(
    modalDialog(
      footer = NULL,
      size = "l",
      easyClose = TRUE,
        lapply(0:length(metrics$biomarker), function(i){
          
          if(i ==  0 & length(unique(df[,vars()[number,"target"]])) == 2){
            
            best_features <- tutorial_best_features()
            
            df[, group] <- as.integer(factor(df[, group]))
            form <- as.formula(paste(group, "~", "."))
            glm_model <- glm(form, data = df)
            
            best_model <- step(object = glm_model, direction = "backward", trace = 0)
            best_model_roc <- pROC::roc(best_model$y, best_model$fitted.values)
            
            df_model <- data.frame(
              name = "Model", 
              sens = 1 - best_model_roc$sensitivities, 
              spec = best_model_roc$specificities
            )
            
            n <- names(best_model[["model"]])[-1]
            
            lst <- lapply(n, function(x) {
              singular_roc <- pROC::roc(
                response = df[, group], 
                predictor = df[, x], direction = best_model_roc[["direction"]]
              )
              data.frame(
                name = x, 
                sens = 1 - singular_roc$sensitivities, 
                spec = singular_roc$specificities
              )
            })
            
            dff <- do.call(rbind, lst)
            df2 <- rbind(df_model, dff)
            
            fluidRow(
              column(
                width = 12,
                renderFusionMultiPlot({
                  fusionMultiPlot(
                    data = df2, 
                    x = "sens", 
                    y = "spec", 
                    col = "name", 
                    type = "scatter"
                  ) %>%
                    fusionAxis(
                      xAxisName = "1 - Specificity", 
                      yAxisName = "Sensitivity"
                    ) %>%
                    fusionCaption(caption = "Model & Features") %>%
                    fusionSubcaption(
                      subcaption = paste0(
                        best_features$class1[1], 
                        " is used as positive case"
                        )
                      ) %>%
                    fusionCustomAxis(yAxisMaxValue = 1)
                  })
                )
              )
            
          } else if(i > 0) {
            
            df2 <- df[df[,vars()[number,"target"]] == metrics$class1[i] | df[,vars()[number,"target"]] == metrics$class2[i], ]
            df2[, vars()[number,"target"]] <- factor(df2[,vars()[number,"target"]])
            
            fluidRow(
              column(
                width = 6,
                renderfusionPlot({
                  
                  df.roc <- pROC::roc(
                    df2[,vars()[number,"target"]], 
                    df2[,as.character(metrics$biomarker[i])], 
                    ci = TRUE
                  )
                  
                  df2.roc <- data.frame(
                    spe = 1 - df.roc$specificities,
                    sens = df.roc$sensitivities
                  )
                  
                  fusionPlot(data = df2.roc, x = "spe", y = "sens", type = "scatter") %>%
                    fusionCaption(caption = paste0("ROC Curve (", metrics$biomarker[i], ")")) %>%
                    fusionSubcaption(
                      subcaption = paste0(
                        "AUC = ", 
                        round(x = df.roc$ci[2], digits = 3), 
                        " (", 
                        round(x = df.roc$ci[1], digits = 3), 
                        "-", 
                        round(x = df.roc$ci[3], digits = 3), 
                        ")"
                      )
                    ) %>%
                    fusionAxis(xAxisName = "1 - Specificity", yAxisName = "Sensitivity") %>%
                    fusionCustomAxis(yAxisMaxValue = 1) %>%
                    fusionAnchors(anchorBorderColor = "#5E72E3", anchorBgColor = "#5E72E3") %>%
                    fusionLegend(showLegend = FALSE)
                  
                })
              ),
              column(
                width = 6,
                renderfusionPlot({
                  
                  Y <- c(metrics[i, "TP"], metrics[i, "FP"], metrics[i, "FN"], metrics[i, "TN"])
                  mtx <- matrix(data = Y, ncol = 2)
                  colnames(mtx) <- rownames(mtx) <- c(metrics$class1[i], metrics$class2[i])
                  
                  fusionPlot(data = mtx, type = "confusionMatrix") %>%
                    fusionCaption(caption = "Confusion Matrix") %>%
                    fusionSubcaption(subcaption = paste(colnames(mtx)[1], "is used as positive case")) %>%
                    fusionAxis(xAxisName = "Actual", yAxisName = "Predicted") %>%
                    fusionAnchors(showvalues = TRUE) %>%
                    fusionTooltip(showToolTip = FALSE) %>%
                    fusionLegend(showLegend = FALSE)
                  
                })
              )
            )
            
          }
          
        })
      )
    )
})


### Download the best features  model
output$download_best_features_model <- downloadHandler(
  filename = function(){
    "best_features_model.rds"
  },
  content = function(file) {
    
    number <- number()
    df <- tutorial()
    target <- vars()[number,"target"]
    features <- genesengStats::geneseng_best_model(data = df, group = target)
    newdata <- df[,c(features$biomarker[-1], target)]

    if(vars()[number,"type"] == "Classification"){
      
      newdata[,target] <- as.integer(factor(newdata[, target]))
    
      best_model <- as.formula(paste(target, "~", ".")) %>% 
        glm(data = na.omit(newdata)) %>% 
        step(direction = "backward", trace = 0)
      
    } else {
      
      best_model <- as.formula(paste(target, "~", ".")) %>% 
        lm(data = na.omit(newdata)) %>% 
        step(direction = "backward", trace = 0)
      
    }
    
    saveRDS(object = best_model, file = file)
    
  }
)


### Performance helper
observeEvent(input$performance_helper, {
  showModal(
    modalDialog(
      title = NULL,
      footer = NULL,
      size = "l",
      easyClose = TRUE,
      tagList(
        tags$b("Performances"),
        tags$br(),
        tags$br(),
        "Biomarkers are quickly prototyped using Generalized Linear Mixed Models 
        (GLM) in classification or regression problems.",
        tags$br(),
        tags$br(),
        tags$b("Best Features"),
        tags$br(),
        tags$br(),
        "Best features (the best combination of biomarkers) are selected by an 
        automated algorithm called stepwise algorithm with backward elimination.
        During the process, the algorithm evaluates the best features by minimizing a 
        loss score called AIC.",
        tags$br(),
        tags$br(),
        tags$b("Classification"),
        tags$br(),
        tags$br(),
        "Classification predictive modeling involves assigning a class label to
        input examples. Binary classification consists of predicting one of two 
        classes and multi-class classification consists of predicting more than 
        two classes.",
        tags$br(),
        tags$br(),
        tags$b("Binary classification"),
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("biomarker"), ": the name of a biomarker"),
          tags$li(tags$b("class1"), ": the first category"),
          tags$li(tags$b("class2"), ": the second category"),
          tags$li(tags$b("model"), ": the logistic regression"),
          tags$li(tags$b("logLoss"), ": the log loss value"),
          tags$li(tags$b("best method"), ": Youden or Closest topleft index"),
          tags$li(tags$b("threshold"), ": the optimal threshold based on the best method"),
          tags$li(tags$b("auc"), ": the Area Under the Curve score"),
          tags$li(tags$b("sens"), ": the sensitivity value"),
          tags$li(tags$b("spe"), ": the specificity value"),
          tags$li(tags$b("PPV"), ": the Positive Predictive Value"),
          tags$li(tags$b("NPV"), ": the Negative Predictive Value"),
          tags$li(tags$b("accuracy"), ": the accuracy value"),
          tags$li(tags$b("no info rate"), ": the no information rate value"),
          tags$li(tags$b("balanced accuracy"), ": the balanced accuracy value"),
          tags$li(tags$b("precision"), ": the precision value"),
          tags$li(tags$b("F1"), ": the F1-score value"),
          tags$li(tags$b("TP"), ": the number of True Positive"),
          tags$li(tags$b("FP"), ": the number of  False Negative"),
          tags$li(tags$b("TN"), ": the number of True Negative"),
          tags$li(tags$b("FN"), ": the number of False Negative")
        ),
        tags$br(),
        tags$br(),
        tags$b("Multiclass - One vs One"),
        tags$br(),
        tags$br(),
        "The One vs One strategy involves training K (K − 1) / 2 binary classifiers 
        for a K-way multiclass problem.",
        tags$br(),
        tags$br(),
        tags$img(
          src = "assets/img/pipeline/One_vs_One.png", 
          height = "350px", 
          width = "100%"
        ),
        tags$i("K: the number of classes"),
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("biomarker"), ": the name of a biomarker"),
          tags$li(tags$b("class1"), ": the first category"),
          tags$li(tags$b("class2"), ": the second category"),
          tags$li(tags$b("model"), ": the logistic regression"),
          tags$li(tags$b("multilogLoss"), ": the multi log loss value"),
          tags$li(tags$b("sens"), ": the sensitivity value"),
          tags$li(tags$b("spe"), ": the specificity value"),
          tags$li(tags$b("PPV"), ": the Positive Predictive Value"),
          tags$li(tags$b("NPV"), ": the Negative Predictive Value"),
          tags$li(tags$b("accuracy"), ": the accuracy value"),
          tags$li(tags$b("no info rate"), ": the no information rate value"),
          tags$li(tags$b("precision"), ": the precision value"),
          tags$li(tags$b("balanced accuracy"), ": the balanced accuracy value"),
          tags$li(tags$b("F1"), ": the F1-score value"),
          tags$li(tags$b("TP"), ": the number of True Positive"),
          tags$li(tags$b("FP"), ": the number of  False Negative"),
          tags$li(tags$b("TN"), ": the number of True Negative"),
          tags$li(tags$b("FN"), ": the number of False Negative")
        ),
        tags$br(),
        tags$br(),
        tags$b("Multiclass - One vs All"),
        tags$br(),
        tags$br(),
        "The One vs All strategy involves training a single classifier per class, 
        with the samples of that class as positive samples and all other samples 
        as negatives.",
        tags$br(),
        tags$br(),
        tags$img(
          src = "assets/img/pipeline/One_vs_All.png", 
          height = "350px", 
          width = "100%"
        ),
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("biomarker"), ": the name of a biomarker"),
          tags$li(tags$b("class1"), ": the first category"),
          tags$li(tags$b("class2"), ": the second category"),
          tags$li(tags$b("model"), ": the multi logistic regression classifier"),
          tags$li(tags$b("logLoss"), ": the log loss value"),
          tags$li(tags$b("best method"), ": Youden or Closest topleft index"),
          tags$li(tags$b("threshold"), ": the optimal threshold based on the best method"),
          tags$li(tags$b("auc"), ": the Area Under the Curve score"),
          tags$li(tags$b("sens"), ": the sensitivity value"),
          tags$li(tags$b("spe"), ": the specificity value"),
          tags$li(tags$b("PPV"), ": the Positive Predictive Value"),
          tags$li(tags$b("NPV"), ": the Negative Predictive Value"),
          tags$li(tags$b("accuracy"), ": the accuracy value"),
          tags$li(tags$b("no info rate"), ": the no information rate value"),
          tags$li(tags$b("precision"), ": the precision value"),
          tags$li(tags$b("balanced accuracy"), ": the balanced accuracy value"),
          tags$li(tags$b("F1"), ": the F1-score value"),
          tags$li(tags$b("TP"), ": the number of True Positive"),
          tags$li(tags$b("FP"), ": the number of  False Negative"),
          tags$li(tags$b("TN"), ": the number of True Negative"),
          tags$li(tags$b("FN"), ": the number of False Negative")
        ),
        tags$br(),
        tags$br(),
        tags$b("Regression"),
        tags$br(),
        tags$br(),
        "Regression is a statistical method used to predict continuous values.",
        tags$br(),
        tags$br(),
        tags$ul(
          tags$li(tags$b("biomarker"), ": the name of a biomarker"),
          tags$li(tags$b("target"), ": the name of the target variable"),
          tags$li(tags$b("model"), ": the linear regression"),
          tags$li(tags$b("rmse"), ": the Root Mean Square Error value"),
          tags$li(tags$b("mse"), ": the Mean Square Error value"),
          tags$li(tags$b("mae"), ": the Mean Absolute Error value")
        )
      )
    )
  )
})

