setwd(modeldir)
source('auto_ml_model_docker.R')
train_model <- auto_ml_model(file='train.csv', mode, param_set=3)
save(train_model, file='train_model.Rdata')

## файл data_gov_conv.csv!!!
