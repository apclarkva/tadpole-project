import pandas as pd
import pdb
import numpy
from datetime import datetime

from math import sqrt
from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras import backend as K

number_of_observations = 2

# load data
training_input = read_csv('data/trainingInFinal.csv')
validation_input = read_csv('data/validationInFinal.csv')

training_output = read_csv('data/trainingOutFinal.csv')
validation_output = read_csv('data/validationOutFinal.csv')

validation_output_df = validation_output

baseline_validation = read_csv('data/baseline_data.csv')


def clean_input(input_data):
  input_data.drop('X', axis=1, inplace=True)
  input_data.drop('X.1', axis=1, inplace=True)
  input_data.drop('Unnamed: 0', axis=1, inplace=True)
  input_data['Date'] = pd.to_datetime(input_data['Date'])
  input_data = input_data.sort_values(by = ['PTID_Key', 'Date'])
  return(input_data)


baseline_validation.drop('Unnamed: 0', axis=1, inplace=True)
baseline_validation['Date'] = pd.to_datetime(baseline_validation['Date'])
baseline_validation = baseline_validation.sort_values(by = ['PTID_Key', 'Date'])


def clean_output(output_data):
  output_data.drop('X', axis=1, inplace=True)
  output_data.drop('Unnamed: 0', axis=1, inplace=True)

  output_data['Date'] = pd.to_datetime(output_data['Date'])
  output_data = output_data.sort_values(by = ['PTID_Key', 'Date'])

  return(output_data)



training_input = clean_input(training_input)
validation_input = clean_input(validation_input)

training_output = clean_output(training_output)
validation_output = clean_output(validation_output)
correct_values = validation_output.values

# Print head of the input data
# Need to do min/max on days with output and input


def output_days(input_data, output_data):
  patients_in = numpy.unique(input_data.values[:,1])
  patients_out = numpy.unique(input_data.values[:,1])

  values_in = input_data.values # access days at 10
  values_out = output_data.values # access days at 8

  day_array_scaled = []

  for patient in patients_in:
    days_until_next_in = values_in[values_in[:,1] == patient, 10]
    days_until_next_out = values_out[values_out[:,1] == patient, 8]

    length_in = len(days_until_next_in)
    length_out = len(days_until_next_out)
    
    for i in range(0, length_out):
      
      last_in = days_until_next_in[length_in - 1]
      if (i == 0):
        days_until_next_out[i] = last_in + days_until_next_out[i]
      else:
        days_until_next_out[i] = days_until_next_out[i] + days_until_next_out[i - 1]


    all_days = numpy.concatenate((days_until_next_in, days_until_next_out))

    # scaled_days = all_days / numpy.amax(all_days)

    days_out = all_days[length_in:]

    values_out[values_out[:,1] == patient, 8] = days_out

  return(values_out)


training_output = output_days(training_input, training_output)
validation_output = output_days(validation_input, validation_output)


def encode_and_scale_input(input_data):
  # Encode categorical data and set variable, inputs equal to everything in
  input_data_values = input_data.values
  encoder = LabelEncoder()
  input_data_values[:,8] = encoder.fit_transform(input_data_values[:,8])

  # scale the input values between 0 and 1
  inputs_to_normalize = input_data_values[:,2:]
  inputs_to_normalize = inputs_to_normalize.astype('float32')

  # normalize features
  scaler_0 = MinMaxScaler(feature_range=(0, 1))
  scaler_1 = MinMaxScaler(feature_range=(0, 1))
  scaler_2 = MinMaxScaler(feature_range=(0, 1))
  scaler_3 = MinMaxScaler(feature_range=(0, 1))
  scaler_4 = MinMaxScaler(feature_range=(0, 1))
  scaler_5 = MinMaxScaler(feature_range=(0, 1))
  scaler_6 = MinMaxScaler(feature_range=(0, 1))
  scaler_7 = MinMaxScaler(feature_range=(0, 1))
  scaler_8 = MinMaxScaler(feature_range=(0, 1))


  inputs_scaled = input_data_values
  inputs_scaled[:,2] = scaler_0.fit_transform(inputs_to_normalize[:,0].reshape(-1,1)).flatten()
  inputs_scaled[:,3] = scaler_1.fit_transform(inputs_to_normalize[:,1].reshape(-1,1)).flatten()
  inputs_scaled[:,4] = scaler_2.fit_transform(inputs_to_normalize[:,2].reshape(-1,1)).flatten()
  inputs_scaled[:,5] = scaler_3.fit_transform(inputs_to_normalize[:,3].reshape(-1,1)).flatten()
  inputs_scaled[:,6] = scaler_4.fit_transform(inputs_to_normalize[:,4].reshape(-1,1)).flatten()
  inputs_scaled[:,7] = scaler_5.fit_transform(inputs_to_normalize[:,5].reshape(-1,1)).flatten()
  inputs_scaled[:,8] = scaler_6.fit_transform(inputs_to_normalize[:,6].reshape(-1,1)).flatten()
  inputs_scaled[:,9] = scaler_7.fit_transform(inputs_to_normalize[:,7].reshape(-1,1)).flatten()
  inputs_scaled[:,10] = scaler_8.fit_transform(inputs_to_normalize[:,8].reshape(-1,1)).flatten()

  return(inputs_scaled)

inputs_scaled_training = encode_and_scale_input(training_input)
inputs_scaled_validation = encode_and_scale_input(validation_input)

scaler_adas = MinMaxScaler(feature_range=(0, 1))
scaler_vent_norm = MinMaxScaler(feature_range=(0, 1))
scaler_mmse = MinMaxScaler(feature_range=(0, 1))

def encode_and_scale_output(output_data):

  outputs_to_normalize = output_data[:,5:8]
  outputs_scaled = output_data

  # outputs_scaled[:,5] = scaler_adas.fit_transform(outputs_to_normalize[:,0].reshape(-1,1)).flatten()
  # outputs_scaled[:,6] = scaler_vent_norm.fit_transform(outputs_to_normalize[:,1].reshape(-1,1)).flatten()
  # outputs_scaled[:,7] = scaler_mmse.fit_transform(outputs_to_normalize[:,2].reshape(-1,1)).flatten()

  return(outputs_scaled)

outputs_scaled_training = encode_and_scale_output(training_output)
outputs_scaled_validation = encode_and_scale_output(validation_output)
baseline_validation_scaled = encode_and_scale_output(baseline_validation.values)

def reshape_inputs(input_data):
  # subtract one, because I want to remove the date column
  patients_in = numpy.unique(input_data[:,1], return_counts=True)

  reshaped_inputs = numpy.empty((len(patients_in[0]), 15, input_data.shape[1] - 2))
  reshaped_inputs[:] = numpy.nan

  # index to be used in for loop
  patient_index = 0

  for patient in patients_in[0]:
    # This reverses the order
    patient_observations = input_data[input_data[:,1] == patient][::-1,]
    number_observations = len(patient_observations[:,2])
    

    reshaped_inputs[patient_index,0:number_observations,:] = patient_observations[:,2:]

    patient_index += 1

  return(reshaped_inputs)

inputs_reshaped_training = reshape_inputs(inputs_scaled_training)
inputs_reshaped_testing = reshape_inputs(inputs_scaled_validation)


def outputs_input_to_obtain(inputs_reshaped, empty_in, output_data):
  new_array = empty_in
  patient_output_observation_frequency = numpy.unique(output_data[:,1], return_counts=True)
  
  patient_index = 0
  
  patients = patient_output_observation_frequency[0]

  for patient in patients:
    input_values = inputs_reshaped[patient_index,:,:]
    output_values = output_data[output_data[:,1] == patient,]


    for num_output in range(0, patient_output_observation_frequency[1][patient_index]):
      # if(patient == 8):
      
      if (num_output == 0): 
        new_in = input_values
      else:
        new_in = input_values
        new_in[0,8] = output_values[num_output-1,8]

      new_in = numpy.reshape(new_in, (1,15,9))

      new_array = numpy.concatenate((new_array, new_in), axis = 0)

    patient_index += 1

  return(new_array)


array_in = numpy.empty((0,15,9))

array_for_model_training = outputs_input_to_obtain(inputs_reshaped_training, array_in, outputs_scaled_training)
array_for_model_validation = outputs_input_to_obtain(inputs_reshaped_testing, array_in, outputs_scaled_validation)


# Which patients am I training on?
# I am using the output data saved to *outputs_scaled*
# I am using the input data saved to *array_for_model*
# The patients_indices array finds the patients who I will be using


def find_indices_to_use(input_data, outputs_scaled, num_observations):


  patients_in = numpy.unique(input_data[:,1], return_counts=True)

  indices_not_nan = numpy.array(outputs_scaled[:,3], dtype=numpy.float)
  indices_not_nan = numpy.logical_not(numpy.isnan(indices_not_nan))

  patients_for_training = patients_in[0][patients_in[1] > num_observations]
  # Something like the following: 
  indices_enough_observations = numpy.isin(outputs_scaled[:,1], patients_for_training)

  indices_to_use = numpy.logical_and(indices_not_nan, indices_enough_observations)

  return(indices_to_use)




indices_to_use_training = find_indices_to_use(inputs_scaled_training, outputs_scaled_training, number_of_observations)
indices_to_use_validation = find_indices_to_use(inputs_scaled_validation, outputs_scaled_validation, number_of_observations)

model = Sequential()
# The following sets the input layer to 50 neurons, and the input shape to 
# a 1 x 8 input. The 1 is timesteps and the 8 is number of features.
model.add(LSTM(50, input_shape=(number_of_observations, array_for_model_training.shape[2])))
# The following sets the dimensionality of the output space to three and uses
# a softmax function to predict probability of each output.
# the activation of dense to K.tanh: see https://keras.io/activations/
# model.add(Dense(50, activation='relu'))

# model.add(Dense(30, activation="relu"))
activation_type = "relu" #Alt - relu
model.add(Dense(100, activation=activation_type))
model.add(Dense(75, activation=activation_type))
model.add(Dense(50, activation=activation_type))
model.add(Dense(40, activation=activation_type))
model.add(Dense(30, activation=activation_type))

# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))
# model.add(Dense(20, activation="relu"))

model.add(Dense(3, activation='softmax'))

# Sets the loss function to mean average loss.
# Use mae or mean average loss for continuous variables
# Use "categorical_crossentropy" for categorical with softmax
# Use mae for loss if continuous
model.compile(loss='categorical_crossentropy', optimizer='adam')

# outputs_scaled = numpy.reshape(outputs_scaled, (outputs_scaled.shape[0], 1,outputs_scaled.shape[1]))

# The following is 50 ephochs with a batch size of 72. I'll be doing
# validation_data=(test_X, test_y),
epoch_num = 50
batch_num = 15 #Best yet: 23 epochs and batch 10 -- .832 -- got lucky
history = model.fit(array_for_model_training[indices_to_use_training,0:number_of_observations,:], outputs_scaled_training[indices_to_use_training,2:5], validation_data=(array_for_model_validation[indices_to_use_validation,0:number_of_observations,:], outputs_scaled_validation[indices_to_use_validation,2:5]), epochs=epoch_num, batch_size=batch_num, verbose=2, shuffle=False)

def return_prediction_categorical(model, history, validation_inputs):
  yhat = model.predict(validation_inputs)
  return(yhat)



yhat = return_prediction_categorical(model, history, array_for_model_validation[indices_to_use_validation,0:number_of_observations,:])

yhat_cat = yhat

baseline_total = baseline_validation.values
baseline_validation_scaled[indices_to_use_validation,2:5] = yhat
yhat_total = baseline_validation_scaled[:,2:5]

diagnosticCountYTotal = 0
diagnosticCountBaselineTotal = 0

yhat_total = (yhat_total == yhat_total.max(axis=1)[:,None]).astype(int)


baseline_total = baseline_total[~numpy.isnan(numpy.array(correct_values[:,2], dtype=numpy.float)),]
yhat_total = yhat_total[~numpy.isnan(numpy.array(correct_values[:,2], dtype=numpy.float)),]
correct_values_less = correct_values[~numpy.isnan(numpy.array(correct_values[:,2], dtype=numpy.float)),]



for index in range(0, baseline_total.shape[0]):
  if ((correct_values_less[index,2] == yhat_total[index,0]) & (correct_values_less[index,3] == yhat_total[index,1]) & (correct_values_less[index,4] == yhat_total[index,2])):
    diagnosticCountYTotal = diagnosticCountYTotal + 1

  if ((correct_values_less[index,2] == baseline_total[index,2]) & (correct_values_less[index,3] == baseline_total[index,3]) & (correct_values_less[index,4] == baseline_total[index,4])):
    diagnosticCountBaselineTotal = diagnosticCountBaselineTotal + 1

percent_yhat = diagnosticCountYTotal/baseline_total.shape[0]
percent_baseline = diagnosticCountBaselineTotal/baseline_total.shape[0]

pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()

pdb.set_trace()





# model_adas = Sequential()
# model_adas.add(LSTM(50, input_shape=(number_of_observations, array_for_model_training.shape[2])))

# # model_adas.add(Dense(50, activation="relu"))

# model_adas.add(Dense(1))
# model_adas.compile(loss='mae', optimizer='adam')

# # history_adas = model_adas.fit(array_for_model_training[indices_to_use_training,0:number_of_observations,:], outputs_scaled_training[indices_to_use_training,5], validation_data=(array_for_model_validation[indices_to_use_validation,0:number_of_observations,:], outputs_scaled_validation[indices_to_use_validation,5]), epochs=80, batch_size=20, verbose=2, shuffle=False)

# def return_prediction_continuous(model, history, validation_inputs):
#   yhat = model.predict(validation_inputs)

#   return(yhat)

# # scaler.inverse_transform(inputs_to_normalize)

# model_ventricular_norm = Sequential()
# model_ventricular_norm.add(LSTM(50, input_shape=(number_of_observations, array_for_model_training.shape[2])))

# # model_ventricular_norm.add(Dense(50, activation="relu"))

# model_ventricular_norm.add(Dense(1))
# model_ventricular_norm.compile(loss='mae', optimizer='adam')


# # history_ventricular_norm = model_ventricular_norm.fit(array_for_model_training[indices_to_use_training,0:number_of_observations,:], outputs_scaled_training[indices_to_use_training,6], validation_data=(array_for_model_validation[indices_to_use_validation,0:number_of_observations,:], outputs_scaled_validation[indices_to_use_validation,6]), epochs=80, batch_size=30, verbose=2, shuffle=False)


# model_mmse = Sequential()
# model_mmse.add(LSTM(50, input_shape=(number_of_observations, array_for_model_training.shape[2])))

# # model_mmse.add(Dense(50, activation="relu"))

# model_mmse.add(Dense(1))
# model_mmse.compile(loss='mae', optimizer='adam')

# history_mmse = model_mmse.fit(array_for_model_training[indices_to_use_training,0:number_of_observations,:], outputs_scaled_training[indices_to_use_training,7], validation_data=(array_for_model_validation[indices_to_use_validation,0:number_of_observations,:], outputs_scaled_validation[indices_to_use_validation,7]), epochs=80, batch_size=30, verbose=2, shuffle=False)

# yhat_adas = return_prediction_continuous(model_adas, history_adas, array_for_model_validation[indices_to_use_validation,0:number_of_observations,:])


# baseline_validation_values = baseline_validation.values


# # yhat_adas = scaler_adas.inverse_transform(yhat_adas)
# baseline_validation_values[indices_to_use_validation, 5] = yhat_adas.flatten()


# yhat_ventricular_norm = return_prediction_continuous(model_ventricular_norm, history_ventricular_norm, array_for_model_validation[indices_to_use_validation,0:number_of_observations,:])
# # yhat_ventricular_norm = scaler_vent_norm.inverse_transform(yhat_ventricular_norm)
# baseline_validation_values[indices_to_use_validation, 6] = yhat_ventricular_norm.flatten()

# yhat_mmse = return_prediction_continuous(model_mmse, history_mmse, array_for_model_validation[indices_to_use_validation,0:number_of_observations,:])
# # yhat_mmse = scaler_mmse.inverse_transform(yhat_mmse)
# baseline_validation_values[indices_to_use_validation, 7] = yhat_mmse.flatten()

# yhat_final = baseline_validation_values
baseline_validation_values = baseline_validation.values



# yhat_final_reduced = yhat_final[indices_to_use_validation,:]
baseline_validation_values_reduced = baseline_validation_values[indices_to_use_validation,:]
correct_values_reduced = correct_values[indices_to_use_validation,:]



# curr_frame = 5
# indices_final = numpy.logical_not(numpy.isnan(numpy.array(yhat_final[:,curr_frame], dtype=numpy.float)))
# indices_baseline = numpy.logical_not(numpy.isnan(numpy.array(baseline_validation_values[:,curr_frame], dtype=numpy.float)))



# rmse_rnn_total = sqrt(mean_squared_error(yhat_final[indices_final,curr_frame], correct_values[indices_final,curr_frame]))
# rmse_baseline_total = sqrt(mean_squared_error(baseline_validation_values[indices_baseline,curr_frame], correct_values[indices_baseline,curr_frame]))

# indices_final_reduced = numpy.logical_not(numpy.isnan(numpy.array(yhat_final_reduced[:,curr_frame], dtype=numpy.float)))
# indices_baseline_reduced = numpy.logical_not(numpy.isnan(numpy.array(baseline_validation_values_reduced[:,curr_frame], dtype=numpy.float)))
# rmse_rnn_total_reduced = sqrt(mean_squared_error(yhat_final_reduced[indices_final_reduced,curr_frame], correct_values_reduced[indices_final_reduced,curr_frame]))
# rmse_baseline_total_reduced = sqrt(mean_squared_error(baseline_validation_values_reduced[indices_baseline_reduced,curr_frame], correct_values_reduced[indices_baseline_reduced,curr_frame]))






# final_df = pd.DataFrame({'Date': validation_output_df.values[:,0], 'PTID_Key': validation_output_df.values[:,1], 'CN_Diag': yhat_final[:,2], 'MCI_Diag': yhat_final[:,3], 'AD_Diag': yhat_final[:,4], 'ADAS13': yhat_final[:,5],'Ventricles_Norm': yhat_final[:,6], 'MMSE': yhat_final[:,7]})

# yhat_final = yhat_final_reduced
# final_df_reduced = pd.DataFrame({'Date': validation_output_df.values[indices_to_use_validation,0], 'PTID_Key': validation_output_df.values[indices_to_use_validation,1], 'CN_Diag': yhat_final[:,2], 'MCI_Diag': yhat_final[:,3], 'AD_Diag': yhat_final[:,4], 'ADAS13': yhat_final[:,5],'Ventricles_Norm': yhat_final[:,6], 'MMSE': yhat_final[:,7]})

# yhat_final = baseline_validation_values_reduced
# final_baseline_reduced = pd.DataFrame({'Date': validation_output_df.values[indices_to_use_validation,0], 'PTID_Key': validation_output_df.values[indices_to_use_validation,1], 'CN_Diag': yhat_final[:,2], 'MCI_Diag': yhat_final[:,3], 'AD_Diag': yhat_final[:,4], 'ADAS13': yhat_final[:,5],'Ventricles_Norm': yhat_final[:,6], 'MMSE': yhat_final[:,7]})

# yhat_final = correct_values_reduced
# final_correct_reduced = pd.DataFrame({'Date': validation_output_df.values[indices_to_use_validation,0], 'PTID_Key': validation_output_df.values[indices_to_use_validation,1], 'CN_Diag': yhat_final[:,2], 'MCI_Diag': yhat_final[:,3], 'AD_Diag': yhat_final[:,4], 'ADAS13': yhat_final[:,5],'Ventricles_Norm': yhat_final[:,6], 'MMSE': yhat_final[:,7]})



correct_values_reduced[:,2:5] = correct_values_reduced[:,2:5].astype(int)

diagnosticCountyHat = 0
diagnosticCountBaseline = 0

diagnosticCountYTotal = 0
diagnosticCountBaselineTotal

yhat_cat = (yhat_cat == yhat_cat.max(axis=1)[:,None]).astype(int)

for index in range(0, correct_values_reduced.shape[0]):
  if ((correct_values_reduced[index,2] == yhat_cat[index,0]) & (correct_values_reduced[index,3] == yhat_cat[index,1]) & (correct_values_reduced[index,4] == yhat_cat[index,2])):
    diagnosticCountyHat = diagnosticCountyHat + 1

  if ((correct_values_reduced[index,2] == baseline_validation_values_reduced[index,2]) & (correct_values_reduced[index,3] == baseline_validation_values_reduced[index,3]) & (correct_values_reduced[index,4] == baseline_validation_values_reduced[index,4])):
    diagnosticCountBaseline = diagnosticCountBaseline + 1

percent_yhat = diagnosticCountyHat/correct_values_reduced.shape[0]
percent_baseline = diagnosticCountBaseline/correct_values_reduced.shape[0]

pdb.set_trace()

final_df.to_csv("data/final_df.csv")
final_df_reduced.to_csv("data/final_df_reduced.csv")
final_baseline_reduced.to_csv("data/final_baseline_reduced.csv")
final_correct_reduced.to_csv("data/final_correct_reduced.csv")








