import pandas as pd
import pdb
from datetime import datetime

from math import sqrt
from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
# from sklearn.preprocessing import MinMaxScaler
# from sklearn.preprocessing import LabelEncoder
# from sklearn.metrics import mean_squared_error
# from keras.models import Sequential
# from keras.layers import Dense
# from keras.layers import LSTM


# load data
training_input = read_csv('data/trainingInRNN.csv')

training_input.drop('X', axis=1, inplace=True)
training_input.drop('X.1', axis=1, inplace=True)
training_input.drop('Unnamed: 0', axis=1, inplace=True)

training_output = read_csv('data/trainingOutRNN.csv')

training_output.drop('X', axis=1, inplace=True)
training_output.drop('Unnamed: 0', axis=1, inplace=True)

def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
  pdb.set_trace()
  n_vars = 1 if type(data) is list else data.shape[1]
  df = DataFrame(data)
  cols, names = list(), list()
  # input sequence (t-n, ... t-1)
  for i in range(n_in, 0, -1):
    cols.append(df.shift(i))
    names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
  # forecast sequence (t, t+1, ... t+n)
  for i in range(0, n_out):
    cols.append(df.shift(-i))
    if i == 0:
      names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
    else:
      names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
  # put it all together
  agg = concat(cols, axis=1)
  agg.columns = names
  # drop rows with NaN values
  if dropnan:
    agg.dropna(inplace=True)
  return agg


# model = Sequential()
# model.add(LSTM(50, input_shape=(train_X.shape[1], train_X.shape[2])))
# model.add(Dense(1))
# model.compile(loss='mae', optimizer='adam')


# history = model.fit(train_X, train_y, epochs=50, batch_size=72, validation_data=(test_X, test_y), verbose=2, shuffle=False)


# Loop through each unique patient
for ptid in training_input.PTID_Key.unique():
  patient_training_input = training_input.loc[training_input['PTID_Key'] == ptid]
  patient_training_output = training_output.loc[training_output['PTID_Key'] == ptid] 

  print(patient_training_input.iloc[:,2])
  print(patient_training_input.shape[0])

  supervised_training_input = series_to_supervised(patient_training_input.iloc[:,2:-1], patient_training_input.shape[0], 1)

  print(supervised_training_input.shape)
  break
















