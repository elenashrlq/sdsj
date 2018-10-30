suppressPackageStartupMessages(library("optparse"))

parser <- OptionParser()

parser <- add_option(parser, c("-m", "--mode"), default="classification", help="Script mode - classification or regression")
parser <- add_option(parser, c("-t", "--train-csv"), help="Training CSV")
parser <- add_option(parser, c("-d", "--model-dir"), help="Model directory")
parser <- add_option(parser, c("-p", "--param-set"), default=1, type='numeric', help="Parameters set")

args <- commandArgs(trailingOnly=TRUE)

options = parse_args(parser, args = args)

print(options$`mode`)
print(options$`train-csv`)
print(options$`model-dir`)
## print(options$`param-set`) больше не нужен

source('src/auto_ml_model_docker.R')

train_model <- auto_ml_model(file=options$`train-csv`)

train_model_path <- file.path(options$`model-dir`, 'train_model.Rdata')

save(train_model, file=train_model_path)

print('Training success')
