suppressPackageStartupMessages(library("optparse"))

parser <- OptionParser()

parser <- add_option(parser, c("-m", "--mode"), default="classification", help="Script mode - classification or regression")
parser <- add_option(parser, c("-t", "--train-csv"), help="Trainging CSV")
parser <- add_option(parser, c("-s", "--test-csv"),  help="Test CSV")
parser <- add_option(parser, c("-d", "--model-dir"), help="Model directory")

args <- commandArgs(trailingOnly=TRUE)

options = parse_args(parser, args = args)

# print(options$`mode`)
# print(options$`test-csv`)
# print(options$`train-csv`)
# print(options$`model-dir`)

# setwd(modeldir)
source('auto_ml_model_docker.R')

train_model <- auto_ml_model(file=options$`test-csv`, param_set=3)
train_model_path <- file.path(options$`model-dir`, 'train_model.Rdata')
save(train_model, file=train_model_path)

## файл data_gov_conv.csv!!!

