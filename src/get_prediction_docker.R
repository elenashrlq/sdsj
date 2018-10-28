## ???????? ???????????? (?? ?????? ????? ? ?????? ???????)
get_pr <- function(test_csv_file_path, model_dir, prediction_csv_file_path) {
  #-----?????????, ??????, ?????? ??????? ?????? --------------
  library(lubridate)
  library(dplyr)
  library(caret)
  
  source('src/convert_fac_to_bin.R')
  source('src/convert_num_to_bin.R')
  source('src/convert_dates.R')
  source('src/add_seas_feat.R')
  source('src/delete_na_cols.R')

  test_1 <- read.csv(test_csv_file_path,
                     encoding = 'UTF-8',
                     na.strings=c("","NA"))
  data_gov <- read.csv("data/data_gov_conv.csv")
  
  model_file_path <- file.path(model_dir, 'train_model.Rdata')
  load(model_file_path)

  # ----??????????? ???????? ??????--------------------------
  
  test <- test_1
  
  test <- convert_fac_to_bin(test, train_model$fac_binary_feat)
  
  test <- convert_num_to_bin(test, train_model$binary_feat1)
  
  test <- convert_dates(test, train_model$date_feat)

  # ----???????? ??????? ??????????--------------------------
  if (length(train_model$date_feat)>0){
    if (length(train_model$ts_feat)==1) {
      test <- add_seas_feat(test, ts_feat=train_model$ts_feat, data_gov=data_gov)
      if (train_model$target_class=='binary') {
        season_predictions_test <- numeric(0)
      } else {
        model_for_season <- train_model$train_date[!duplicated(
          train_model$train_date[,c('season_fac0', train_model$id_feat,'season')]),
          c('season_fac0', train_model$id_feat, 'season')]
        season_predictions_test <- merge(test,
                                         model_for_season,
                                         by=c('season_fac0',
                                              train_model$id_feat),
                                         all.x = T)
        season_predictions_test <- 
          season_predictions_test[order(season_predictions_test[,'line_id']),]
        season_predictions_test <- season_predictions_test$season
      }
    } else {
      season_predictions_test <- numeric(0)
    }
  } else {
    season_predictions_test <- numeric(0)
  }
  # ----???????? ??????? ???????????--------------------------
  if (train_model$target_class=='numeric'|train_model$target_class=='integer') {
    if (length(train_model$binary_feat)>0) {
      test_for_random_bin <- test[,c(train_model$binary_feat, 'line_id')]
      random_predictions_for_bin <- predict(train_model$best_for_random_bin_model, test)
    } else {
      random_predictions_for_bin <- numeric(0)
    }

    if (length(c(train_model$numeric_feat, train_model$integer_feat))>0) {
      test_for_random_num <- predict(train_model$preobj, test)
      random_predictions_for_num <- predict(train_model$best_for_random_num_model,
                                            test_for_random_num)
    } else {random_predictions_for_num <- numeric(0)}

    if (length(train_model$fac_feat)>0) {
      test_for_random_fac <-
        merge(x=test,
              y=train_model$train_for_random_fac[!duplicated(
                train_model$train_for_random_fac[,train_model$fac_feat]),
                -which(grepl('random|line_id',names(train_model$train_for_random_fac)))],
              by=c(train_model$fac_feat),
              all.x = TRUE)

      test_for_random_fac <- predict(train_model$preobj_for_random_fac, test_for_random_fac)
      random_predictions_for_fac <- predict(train_model$model_for_random_fac, test_for_random_fac)
    } else {random_predictions_for_fac <- numeric(0)}

    list_test_combined <- list(best_for_random_bin_prediction=
                                 random_predictions_for_bin,
                               best_for_random_num_prediction=
                                 random_predictions_for_num,
                               best_for_random_fac_prediction=
                                 random_predictions_for_fac)

    chosen_test_combined <- do.call(cbind,
                                    list_test_combined[which(
                                      sapply(list_test_combined, length)>0)])

    test_combined <- cbind.data.frame(chosen_test_combined)

    random_predictions_test <- predict(train_model$combined_random_model, newdata=test_combined)

  } else {random_predictions_test <- numeric(0)}

  # ----???????? ??????? ?????????--------------------------
  if (train_model$zero_share>0) {
    if (train_model$target_class=='numeric'|train_model$target_class=='integer') {
      test_for_cat <-
        cbind.data.frame(test_for_random_num[,-which(
          grepl('random|line_id', names(test_for_random_num)))],
          test_for_random_bin[,-which(
            grepl('random|line_id',names(test_for_random_bin)))]) ## !! ???????? fac
    } else {
      test_for_cat <- test[,-c(which(grepl('line_id', names(test))),
                               which(names(test) %in% train_model$date_feat))] ##!!???????? ????????? ?????????? date_feat
      test_for_cat <- predict(train_model$preobj_for_cat, test_for_cat)
      
      test_for_cat <- delete_na_cols(test_for_cat, train_model$na_var)
      test_for_cat <- test_for_cat[,-which(names(test_for_cat) %in% train_model$highCor)]
      test_for_cat <- predict(train_model$preobj_for_cat2, test_for_cat)
    }
    if (length(train_model$binary_feat)>0) {
      ## ?????? ????? (??????) ?????? ?? 0
      if (length(which(test_for_cat[,which(names(test_for_cat) %in%
                                           train_model$binary_feat)]!='1' &
                       test_for_cat[,which(names(test_for_cat) %in%
                                           train_model$binary_feat)]!='0')) > 0) {
        a <- numeric(0)
        for (i in which(names(test_for_cat) %in% train_model$binary_feat)) {
          a <- test_for_cat[,i]
          levels(test_for_cat[,i])[levels(test_for_cat[,i])!='0' &
                                     levels(test_for_cat[,i])!='1'] <- '0'
        }
      }
    }
    cat_predictions_test <- predict(train_model$best_for_cat_model, test_for_cat)
  } else cat_predictions_test <- integer(0)

  # ----???????????? ?????? ?? ?????? ???? ??????? ??????????--------------------------
  if (length(season_predictions_test)>0 & length(cat_predictions_test)>0) {
    target_predictions_test=(random_predictions_test+season_predictions_test)
    target_predictions_test[cat_predictions_test==0] <- 0
  } else if (length(season_predictions_test)>0) {
    target_predictions_test=random_predictions_test+season_predictions_test
  } else if (length(cat_predictions_test)>0){
    target_predictions_test=random_predictions_test
    target_predictions_test[cat_predictions_test==0] <- 0
    target_predictions_test[is.na(target_predictions_test)] <- 1
  } else target_predictions_test=random_predictions_test

  list(season_predictions_test=season_predictions_test,
       random_predictions_test=random_predictions_test,
       cat_predictions_test=cat_predictions_test,
       target_predictions_test=target_predictions_test)

  prediction_data = data.frame(line_id=test$line_id,
                               target=target_predictions_test)
  
  write.csv(prediction_data, file = prediction_csv_file_path)
}
