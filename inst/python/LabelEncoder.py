from sklearn.preprocessing import LabelEncoder

def MakeLabelEncoder(data):
  col_list = data.select_dtypes(include = "object").columns
  for colsn in col_list:
    data[colsn] = LabelEncoder().fit_transform(data[colsn])
  
  return data

