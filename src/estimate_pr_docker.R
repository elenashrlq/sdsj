## делаем оценку модели
estimate_pr <- function(true_value_file_path, prediction_file_path, mode) {
  #-----открываем--------------
  test_target_1 <- read.csv(true_value_file_path,
                            encoding = 'UTF-8',
                            na.strings=c("","NA"))
  
  target_predictions_true <- test_target_1$target[order(test_target_1$line_id)]
  prediction_data <- read.csv(prediction_file_path)

  #-----выводим данные------------
  if (mode != 'classification') {
    a <- sum((prediction_data$target - target_predictions_true) ^ 2)
    b <- length(prediction_data$target)

    testMetric <- sqrt(a / b)
  }
  else {
    a <- table(prediction_data$target, target_predictions_true)[1] + table(prediction_data$target, target_predictions_true)[4]
    b <- sum(table(prediction_data$target, target_predictions_true)[1:4])

    testMetric <- a / b
  }

  testMetric
}
