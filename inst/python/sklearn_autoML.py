import tempfile
import datetime
from autosklearn.classification import AutoSklearnClassifier
from autosklearn.regression import AutoSklearnRegressor

def sklearn_autoML(X_train, y_train, method, time_left_for_this_task = 120, per_run_time_limit = 30):
  
  today = datetime.date.today()
  path_timestamp = tempfile.TemporaryDirectory().name + "/Autosklearn/sk-"  + today.strftime("%m_%d_%Y_%s")
  
  if method == "Classification":
    cls = AutoSklearnClassifier(
      time_left_for_this_task = int(time_left_for_this_task),
      per_run_time_limit = int(per_run_time_limit),
      tmp_folder = path_timestamp,
      smac_scenario_args = {
        "deterministic": "true"
      }
    )
  elif method == "Regression":
    cls = AutoSklearnRegressor(
      time_left_for_this_task = int(time_left_for_this_task),
      per_run_time_limit = int(per_run_time_limit),
      tmp_folder = path_timestamp,
      smac_scenario_args = {
        "deterministic": "true"
      }
    )
    
  cls.fit(X_train, y_train)
  
  return cls
