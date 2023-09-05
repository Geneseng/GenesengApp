#' Perform Permutation Feature Importance
#' 
#' @param n Number of replicates
#' @param model ML model (from Python libraries)
#' @param X_train Features
#' @param y_train Target 
#' @param metric scoring
#' 
#' 
#' @export
geneseng_feature_importance <- function(n, model, X_train, y_train, metric){
  
  # 1: Compute loss function for original model (e.i. Accuracy, RMSE)
  original_loss <- err_loss(model, X_train = X_train, y_train = y_train, metric = metric)
  
  # 2: for variable i in {1, ..., p} do
  # Randomize values
  # Estimate loss function
  # Compute feature importance
  n_ratios <- replicate(n, ratio_loss(model, X_train, y_train, original_loss, metric))
  rownames(n_ratios) <- names(X_train)
  colnames(n_ratios) <- 1:n
  
  return(t(n_ratios))
  
}

#' @rdname geneseng_feature_importance
#' @export
err_loss <- function(model, X_train, y_train, metric = c("Accuracy", "AUC", "RMSE", "MAE")){
  
  metric <- match.arg(metric)
  
  # 1: Compute loss function for original model (e.i. Accuracy, RMSE)
  
  err <- switch(
    metric,
    "Accuracy" = sum(diag(table(y_train, model$predict(X_train)))) / sum(table(y_train, model$predict(X_train))),
    "AUC" = max(
      compute_roc_auc(y_train, model$predict_proba(X_train)[,1]),
      compute_roc_auc(y_train, model$predict_proba(X_train)[,2])
    ),
    "RMSE" = sqrt(mean((y_train - model$predict(X_train)) ^ 2, na.rm = TRUE)),
    "MAE" = mean(abs(y_train - model$predict(X_train)), na.rm = TRUE)
  )
  
  return(err)
  
}

#' @rdname geneseng_feature_importance
#' @export
ratio_loss <- function(model, X_train, y_train, original_loss, metric){
  
  # 2: for variable i in {1, ..., p} do
  calc_for_j <- function(j){
    
    # Randomize values
    Z_train <- X_train
    Z_train[,j] <- sample(X_train[,j])
    
    # Estimate loss function
    permuted_loss <- err_loss(model, X_train = Z_train, y_train = y_train, metric = metric)
    
    # Compute feature importance
    return(permuted_loss / original_loss)
    
  }
  
  ratios <- sapply(1:ncol(X_train), calc_for_j)
  
  return(ratios)
  
}

#' Compute AUC
#'
#' @param y_true Observation
#' @param y_pred Prediction
#'
#' @export
compute_roc_auc <- function (y_true, y_pred) {
  
  n_pos <- sum(y_true == 1)
  n_neg <- sum(y_true == 0)
  y_pred <- rank(y_pred)
  
  score <- (sum(y_pred[y_true == 1]) - n_pos * (n_pos + 1)/2)/(n_pos * n_neg)
  
  return(score)
  
}
