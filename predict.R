suppressPackageStartupMessages(library("optparse"))

parser <- OptionParser()

parser <- add_option(parser, c("-p", "--prediction-csv"), help="Prediction CSV")
parser <- add_option(parser, c("-s", "--test-csv"),  help="Test CSV")
parser <- add_option(parser, c("-d", "--model-dir"), help="Model directory")

args <- commandArgs(trailingOnly=TRUE)

options = parse_args(parser, args = args)

print(options$`prediction-csv`)
print(options$`test-csv`)
print(options$`model-dir`)

source('src/get_prediction_docker.R')

new_ds_pr <- get_pr(test_csv_file_path=options$`test-csv`,
					model_dir=options$`model-dir`,
					prediction_csv_file_path=options$`prediction-csv`)

print('Prediction Success')
