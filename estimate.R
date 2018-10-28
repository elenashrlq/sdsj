suppressPackageStartupMessages(library("optparse"))

parser <- OptionParser()

parser <- add_option(parser, c("-m", "--mode"), default="classification", help="Script mode - classification or regression")
parser <- add_option(parser, c("-t", "--test-target-csv"), help="Training CSV")
parser <- add_option(parser, c("-p", "--prediction-csv"), help="Prediction CSV")

args <- commandArgs(trailingOnly=TRUE)

options = parse_args(parser, args = args)

print(options$`mode`)
print(options$`test-target-csv`)
print(options$`prediction-csv`)

source('src/estimate_pr_docker.R')

estimation <- estimate_pr(true_value_file_path=options$`test-target-csv`,
						  prediction_file_path=options$`prediction-csv`,
						  mode=options$`mode`)

print(estimation)

print('Estimation success')
