## делаем оценку модели
estimate_pr <- function(true_value_file_path, prediction_file_path,
                        mode, x){
  #-----открываем--------------
  test_target_1 <- read.csv(true_value_file_path,
                            encoding = 'UTF-8',
                            na.strings=c("","NA"))
  target_predictions_true <- test_target_1$target[order(test_target_1$line_id)]
  prediction_data <- read.csv(prediction_file_path)
  #-----выводим данные------------
  if (mode != 'classification') {
    testMetric <- sqrt(sum((predictions$target -
                              target_predictions_true)^2)/
                         length(predictions$target))

    } else {

    testMetric <- (table(prediction_data$target,
                         target_predictions_true)[1]+
                     table(prediction_data$target,
                           target_predictions_true)[4])/
      sum(table(prediction_data$target,
                target_predictions_true)[1:4])
  }

  list(testMetric=testMetric,
       test2Metric=test2Metric)
}
