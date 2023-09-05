import numpy as np

def split_train_test_set(data, y, train_size = 0.6, seed = 42):
   
   np.random.seed(int(seed))
   shuffle_df = data.sample(frac = 1)
   split = int(train_size * len(shuffle_df))
   train = shuffle_df[:split]
   test = shuffle_df[split:]
   
   results =  {
     "X_train": train.drop(y, axis = 1),
     "y_train": train[y],
     "X_test": test.drop(y, axis = 1),
     "y_test": test[y]
   }
   
   return results

