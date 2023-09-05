from flaml import AutoML

def flaml_autoML(X_train, y_train, time_budget = 30):
  
  ## Train the model
  automl = AutoML()
  automl.fit(X_train, y_train, time_budget=int(time_budget))
  
  return automl
