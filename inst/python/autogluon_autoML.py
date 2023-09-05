import tempfile
import datetime
import autogluon.tabular

def autogluon_autoML(train, y_label, time_limit, presets):
  
  ## Train the model
  today = datetime.date.today()
  path_timestamp = tempfile.TemporaryDirectory().name + "/AutogluonModels/ag-"  + today.strftime("%m_%d_%Y_%s")
  predictor = autogluon.tabular.TabularPredictor(label = y_label, path = path_timestamp).fit(train, time_limit = time_limit, presets = presets)
  
  return predictor


