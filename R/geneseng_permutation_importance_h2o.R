#' Perform Permutation Feature Importance for H2o
#' 
#' @param n Number of replicates
#' @param model ML model (from Python libraries)
#' @param X_train Features
#' @param y_train Target 
#' @param metric scoring
#' 
#' @import h2o
#' 
#' @export
geneseng_feature_importance_h2o <- function(n, model, X_train, y_train, metric){
  
  # 1: Compute loss function for original model (e.i. Accuracy, RMSE)
  original_loss <- err_loss_h2o(model, X_train = X_train, y_train = y_train, metric = metric)
  
  # 2: for variable i in {1, ..., p} do
  # Randomize values
  # Estimate loss function
  # Compute feature importance
  n_ratios <- replicate(n, ratio_loss_h2o(model, X_train, y_train, original_loss, metric))
  rownames(n_ratios) <- names(X_train)
  colnames(n_ratios) <- 1:n
  
  return(t(n_ratios))
  
}

#' @rdname geneseng_feature_importance_h2o
#' @export
err_loss_h2o <- function (model, X_train, y_train, metric = c("Accuracy", "AUC", "RMSE", "MAE")){
  
  metric <- match.arg(metric)
  
  y_true <- y_train
  y_pred <- h2o::h2o.predict(model, X_train)
  
  err <- switch(
    metric, 
    Accuracy = sum(diag(table( y_true, as.vector(y_pred[,1]))))/sum(table(y_true, as.vector(y_pred[,1]))), 
    AUC = max(
      compute_roc_auc(y_true, as.vector(y_pred[,2])), 
      compute_roc_auc(y_true, as.vector(y_pred[,3]))
    ), 
    RMSE = sqrt(mean((y_true - as.numeric(as.vector(y_pred[,1])))^2, na.rm = TRUE)), 
    MAE = mean(abs(y_true - as.numeric(as.vector(y_pred[,1]))), na.rm = TRUE)
  )
  
  return(err)
  
}

#' @rdname geneseng_feature_importance_h2o
#' @export
ratio_loss_h2o <- function(model, X_train, y_train, original_loss, metric){
  
  # 2: for variable i in {1, ..., p} do
  calc_for_j <- function(j){
    
    # Randomize values
    Z_train <- as.data.frame(X_train)
    Z_train[,j] <- sample(as.data.frame(X_train)[,j])
    
    # Estimate loss function
    permuted_loss <- err_loss_h2o(model, X_train = h2o::as.h2o(Z_train), y_train = y_train, metric = metric)
    
    # Compute feature importance
    return(permuted_loss / original_loss)
    
  }
  
  ratios <- sapply(1:ncol(X_train), calc_for_j)
  
  return(ratios)
  
}





