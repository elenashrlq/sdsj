## тренируем модель (на основе файла с данными для обучения)
auto_ml_model <- function(file, param_set) {
  library(lubridate)
  library(dplyr)
  library(caret)

  source('src/params.R')
  source('src/delete_na_cols.R')
  source('src/convert_dates.R')
  source('src/convert_num_to_bin.R')
  source('src/convert_fac_to_bin.R')
  source('src/add_seas_feat.R')
  
  #-----открываем, чистим, меняем форматы данных --------------
  train_1 <- read.csv(file,
                      encoding = 'UTF-8',
                      na.strings=c("","NA"))
  data_gov <- read.csv("data/data_gov_conv.csv")

  target_class = character()
  zero_share = 0
  date_feat = character()
  train_date = data.frame()
  numeric_feat = character()
  binary_feat1 = character()
  fac_feat1 = character()
  fac_binary_feat=character()
  id_feat=character()
  integer_feat=character()
  ts_feat=character()
  trainMetric=numeric()
  train2Metric=numeric()
  na_var=character()
  highCor=character()
  preobj=character()
  preobj_for_random_fac=character()
  preobj_for_cat=character()
  preobj_for_cat2=character()
  best_for_random_num_model=character()
  best_for_random_bin_model=character()
  best_for_cat_model=character()
  combined_random_model=character()
  model_for_random_fac = character()
  train_for_random_fac = data.frame()
  season_predictions=numeric()
  random_predictions=numeric()
  cat_predictions=numeric()
  target_predictions=numeric()

  #------чистим данные, добавляем новые переменные, меняем классы объектов-----
  default_date <- as_date(0)
  train <- train_1

  target <- names(train)[grep('target', names(train))]
  lineid_feat <- names(train)[grep('line_id', names(train))]
  id_feat <- names(train)[grep('id', names(train))]
  id_feat <- setdiff(id_feat, lineid_feat)

  date_feat <- names(train)[grep('datetime_', names(train))]

  
  train <- convert_dates(train, date_feat)

  feat_classes <- sapply(train, class)
  numeric_feat <- names(train[feat_classes=='numeric'])
  integer_feat <- names(train[feat_classes=='integer'])

  fac_feat1 <- names(train[feat_classes=='factor'  &
                             !(names(train) %in% date_feat)])

  fac_binary_feat <- fac_feat1[sapply(train[fac_feat1], n_distinct)==2]

  train <- convert_fac_to_bin(train, fac_binary_feat)

  binary_feat1 <-
    c(integer_feat, numeric_feat)[which(
      sapply(train[c(integer_feat,numeric_feat)],n_distinct)==2 &
        sapply(train[c(integer_feat,numeric_feat)],max, na.rm=T)==1 &
        sapply(train[c(integer_feat,numeric_feat)],min, na.rm=T)==0)]

  train <- convert_num_to_bin(train, binary_feat1)

  binary_feat1 <- c(binary_feat1, fac_binary_feat)

  numeric_feat <- setdiff(numeric_feat, c(binary_feat1, lineid_feat))
  integer_feat <- setdiff(integer_feat, c(binary_feat1, lineid_feat))
  fac_feat1 <- setdiff(fac_feat1, c(fac_binary_feat))

  ##вычитаем таргет и определяем класс таргета
  if (length(which(numeric_feat %in% target))>0) {numeric_feat <-
    setdiff(numeric_feat, target)
  target_class = 'numeric'}

  if (length(which(integer_feat %in% target))>0) {integer_feat <-
    setdiff(integer_feat, target)
  target_class = 'integer'}

  if (length(which(fac_feat1 %in% target))>0) {fac_feat1 <-
    setdiff(fac_feat1, target)
  target_class = 'fac'}

  if (length(which(binary_feat1 %in% target))>0) {binary_feat1 <-
    setdiff(binary_feat1, target)
  target_class = 'binary'}

  ##сортируем
  train <- train[order(train$line_id),]
  target_class

  zero_share <- length(train$target[train$target==0])/length(train$target)

  #------проверяем наличие дат и форматируем-----
  if (length(date_feat)>0){
    ## находим временные ряды
    ts_feat <- date_feat[which((sapply(train[date_feat], max, na.rm=T)-
                                  sapply(train[date_feat], min, na.rm=T) + 1)/
                                 sapply(train[date_feat], n_distinct, na.rm=T)==1)]

    ## если есть вр.ряды, добавляем сезонные переменные
    if (length(ts_feat)==1) {
      train <- add_seas_feat(train, ts_feat=ts_feat, data_gov=data_gov)

      binary_feat <- c(binary_feat1, 'season_fac2', 'season_fac4', 'season_fac5')
      fac_feat <- c(fac_feat1, 'season_fac1', 'season_fac3') ## season_fac0 нет специально, т.к. он учитывается в сезонности

      ## если есть вр.ряды и таргет бинарный, вектор сезонности нулевой
      if (target_class=='binary'){
        season_predictions <- numeric(0)
        train_date <- train[,c(id_feat, lineid_feat, target)]
        train_date$season <- rep(0, dim(train)[1])
        train_date$random <- train[,target]
      }
      else {
        ## если есть вр.ряды и таргет числовой, делаем проверку на наличие id и
        ## вычисляем сезонность
        if(length(id_feat)>0){
          train_date <- train[,c(ts_feat,
                                 id_feat,
                                 lineid_feat,
                                 target,
                                 'season_fac0')]
          train_date <- train_date[order(train_date[,ts_feat]),]
          train_date_list <- split(train_date, train_date[,id_feat])
          for (i in 1:length(unique(train_date[,id_feat]))) {
            train_date_list[[i]] <- train_date_list[[i]]
            target_daily_ts <- ts(train_date_list[[i]][target], frequency=7)
            target_daily_decompose <- decompose(target_daily_ts,
                                                filter = 0)
            train_date_list[[i]] <- mutate(
              train_date_list[[i]],
              season=as.numeric(target_daily_decompose$seasonal),
              random=as.numeric(target_daily_decompose$random))
          }
          train_date <- do.call(rbind, train_date_list)
          train_date <- train_date[order(train_date[,lineid_feat]),]

        } else {
          train_date <- train[,c(ts_feat,
                                 id_feat,
                                 lineid_feat,
                                 target,
                                 'season_fac0')]
          train_date <- train_date[order(train_date[,ts_feat]),]
          target_daily_ts <- ts(train_date[target], frequency=7)
          target_daily_decompose <- decompose(target_daily_ts,
                                              filter = 0)
          train_date$season <- as.numeric(target_daily_decompose$seasonal)
          train_date$random <- as.numeric(target_daily_decompose$random)
          train_date <- train_date[order(train_date[,lineid_feat]),]
        }
        season_predictions <- train_date$season
      }
    }
    else {
      ## если есть несколько дат, но нет одного временного ряда, вектор сезонности
      ## нулевой
      season_predictions <- numeric(0)
      train_date <- train[,c(id_feat, lineid_feat, target)]
      train_date$season <- rep(0, dim(train)[1])
      train_date$random <- train[,target]
      binary_feat <- binary_feat1
      fac_feat <- fac_feat1
      ##!! сюда добавить переменные
    }
  } else {
    ## если нет дат, то вектор сезонности нулевой
    ts_feat <- numeric(0)
    season_predictions <- numeric(0)
    train_date <- train[,c(id_feat, lineid_feat, target)]
    train_date$season <- rep(0, dim(train)[1])
    train_date$random <- train[,target]
    binary_feat <- binary_feat1
    fac_feat <- fac_feat1
  }

  # ----строим модели для прогноза рандомности -----
  if (target_class=='numeric'|target_class=='integer') {

    ## ------строим модель 1 для прогноза рандомности ------------------
    if (length(c(numeric_feat, integer_feat))>0) {
      ## вычисляем рандоность, если таргет числовой
      train_for_random <- cbind.data.frame(
        train[,-c(which(names(train)=='target'))],
        random=train_date['random'],
        row.names = 1:length(train[,1]))

      train_for_random_num <- train_for_random[c('random',
                                                 'line_id',
                                                 numeric_feat,
                                                 integer_feat)]
      preobj <- preProcess(
        train_for_random_num[,-c(which(names(train_for_random_num)=='random'),
                                 which(names(train_for_random_num)=='line_id'))],
        method = c('center', 'scale', 'nzv', 'medianImpute','pca')) ##убрала 'knnImpute'

      train_for_random_num <- predict(preobj,
                                      train_for_random_num)
     set.seed(123)
      fit3_for_random_num <- train(as.numeric(random)~.,data=train_for_random_num,
                                   method=params[[param_set]]$method[1],
                                   trControl=trainControl(method = "cv",
                                                          number = params[[param_set]]$cv[1]))
      predictions3 <- predict(fit3_for_random_num, train_for_random_num)
      rmse3_for_random_num <- min(fit3_for_random_num$results$RMSE)

      ## выбираем
      best_for_num_choice <- which(c(##--rmse1,
        ##--rmse2,
        rmse3_for_random_num,
        use.names = FALSE)==
          min(c(##--rmse1,
            ##--rmse2,
            rmse3_for_random_num
          )))

      best_for_random_num_prediction <- list(##--predictions1,
        ##--predictions2,
        predictions3
      )[[best_for_num_choice]]

      best_for_random_num_model <- list(##--fit1_for_random_num,
        ##--fit2_for_random_num,
        fit3_for_random_num)[[best_for_num_choice]]
    } else {
      ## если таргет бинарный, то рандомность - нулевой вектор
      best_for_random_num_prediction <- numeric(0)
    }

    ## ------строим модель 2 для прогноза рандомности -----
    if (length(binary_feat)>0) {
      ## если есть бинарные переменные, то прогнозируем на основе них
      train_for_random_bin <- train_for_random[,c(binary_feat,
                                                  'random',
                                                  'line_id')]
      set.seed(123)
      fit1_for_random_bin <-
        train(as.numeric(random)~.,
              data=train_for_random_bin[,-which(
                names(train_for_random_bin)=='line_id')],
              method=params[[param_set]]$method[2], ## было treebag glmnet
              trControl=trainControl(method = "cv",number = params[[param_set]]$cv[2])) ## заменила 40 на 5
      predictions1 <- predict(fit1_for_random_bin, train_for_random_bin)
      rmse1_for_random_bin <- min(fit1_for_random_bin$results$RMSE)

      ## выбираем
      best_for_random_bin_choice <- which(c(rmse1_for_random_bin,
                                            ##--rmse2,
                                            ##--rmse3,
                                            use.names = FALSE)==
                                            min(c(rmse1_for_random_bin
                                                  ##--,
                                                  ##--rmse2,
                                                  ##--rmse3
                                            )))

      best_for_random_bin_prediction <- list(predictions1##--,
                                             ##--predictions2,
                                             ##--predictions3
      )[[best_for_random_bin_choice]]

      best_for_random_bin_model <- list(fit1_for_random_bin##--,
                                        ##--fit2_for_random_fac,
                                        ##--fit3_for_random_fac
      )[[best_for_random_bin_choice]]
    } else {
      ## если нет бинарных переменных, то получаем нулевой вектор
      best_for_random_bin_prediction <- numeric(0)}

    ## ------строим модель 3 для прогноза рандомности -----
    if (length(fac_feat)>0) {
      ## если есть факторы, то прогнозируем на основе них
      train_for_random_fac <- train_for_random[,c(fac_feat,
                                                  'random',
                                                  'line_id')]
      random_fac <- as.list(0)
      tab <- as.list(0)
      model_for_random_fac <- as.list(0)
      prediction_for_random_fac <- as.list(0)

      for (i in 1:length(fac_feat)) {
        random_fac[[i]] <- sapply(split(train_for_random_fac[,'random'],
                                        train_for_random_fac[,fac_feat[i]]),
                                  median)
        tab[[i]] <- data.frame(x=names(random_fac[[i]]),
                               y=random_fac[[i]])
        model_for_random_fac[[i]] <-
          merge(train_for_random[c('line_id', 'random', fac_feat[i])],
                tab[[i]], by.x=3,by.y=1)
        model_for_random_fac[[i]] <-
          model_for_random_fac[[i]][order(model_for_random_fac[[i]][,'line_id']),]
        prediction_for_random_fac[[i]] <- model_for_random_fac[[i]][,c('y')]
      }

      train_for_random_fac <- data.frame(do.call(cbind,prediction_for_random_fac),
                                         train_for_random_fac)
      preobj_for_random_fac <- preProcess(train_for_random_fac,
                                          method = "medianImpute")
      train_for_random_fac <- predict(preobj_for_random_fac,
                                      train_for_random_fac)

      model_for_random_fac <-
        train(as.numeric(random)~.,
              data=train_for_random_fac[,-which(
                grepl('line_id',names(train_for_random_fac)) |
                  names(train_for_random_fac) %in% fac_feat)],
              method=params[[param_set]]$method[3],
              trControl=trainControl(method = "cv",number = params[[param_set]]$cv[3]))

      best_for_random_fac_prediction <- predict(model_for_random_fac,
                                                train_for_random_fac)
    } else {
      ## если нет факторов, то получаем нулевой вектор
      best_for_random_fac_prediction <- numeric(0)
    }

    ## ------строим обобщенную модель для прогноза рандомности-------------

    list_train_combined <- list(best_for_random_bin_prediction=
                                  best_for_random_bin_prediction,
                                best_for_random_num_prediction=
                                  best_for_random_num_prediction,
                                best_for_random_fac_prediction=
                                  best_for_random_fac_prediction)

    chosen_train_combined <- do.call(cbind,
                                     list_train_combined[which(
                                       sapply(list_train_combined, length)>0)])

    train_combined <- cbind.data.frame(chosen_train_combined,
                                       random=train_for_random[,'random']) ## все данные должны быть предварительно отсортированы по line_id

    set.seed(123)
    combined_random_model <-
      train(as.numeric(random)~.,
            data=train_combined,
            method=params[[param_set]]$method[4], ##verbose=FALSE, ## с лассо по обычн. формуле хуже и наоборот
            trControl=trainControl(method = "cv", number = params[[param_set]]$cv[4])) ##лучший результат - n=30 для gmb; для glm от 20
    rmse1_combined_random <- min(combined_random_model$results$RMSE)
    random_predictions <- predict(combined_random_model, train_combined)

  } else {random_predictions <- numeric(0)}

  # ----строим модель для прогноза категории таргета--------------------------
  if (zero_share>0) {
    ## если в таргете есть нулевые значения, находим прогноз категории
    if (target_class=='numeric'|target_class=='integer') {
      ## обрабатываем данные, если таргет числовой
      train_for_cat <-
        cbind.data.frame(train_for_random_num[,-which(
          grepl('random|line_id', names(train_for_random_num)))],
          train_for_random_bin[,-which(grepl('random|line_id',
                                             names(train_for_random_bin)))],
          target=train[,'target'])
      train_for_cat$target[train_for_cat$target!=0] <- 1
      train_for_cat$target[train_for_cat$target==0] <- 0
      train_for_cat$target <- as.factor(train_for_cat$target)
    } else {
      ## обрабатываем данные, если таргет бинарный
      train_for_cat <- train[,-c(which(grepl('line_id',names(train))),
                                 which(names(train) %in% date_feat))] ##!!добавить обработку переменных date_feat
      preobj_for_cat <- preProcess(train_for_cat[,-which(
        names(train_for_cat)=='target')],
        method=c('medianImpute', 'zv', 'nzv'))

      train_for_cat <- predict(preobj_for_cat, train_for_cat)

      t_train_for_cat <- data.frame(t(train_for_cat))
      na_var <- rownames(t_train_for_cat[!complete.cases(t_train_for_cat),])

      train_for_cat <- delete_na_cols(train_for_cat, na_var)

      highCor <- findCorrelation(cor(train_for_cat[,which(
        names(train_for_cat) %in% c(numeric_feat, integer_feat))]),
        cutoff = params[[param_set]]$corlim[5], names = TRUE)

      train_for_cat <- train_for_cat[,-which(
        names(train_for_cat) %in% highCor)]

      preobj_for_cat2 <- preProcess(train_for_cat[,which(
        names(train_for_cat) %in% c(numeric_feat, integer_feat))],
        method=c('pca'), prcomp=20)

      train_for_cat <- predict(preobj_for_cat2, train_for_cat)
    }

    ## строим модель на основе обработанных данных
    fit1_for_cat <- train(target~.,
                          data=train_for_cat,
                          method=params[[param_set]]$method[5], ##4-й датасет работал с glm
                          trControl=trainControl(method = "cv", ##попробовать summaryFunction=max
                                                 number = params[[param_set]]$cv[5])) ## заменила round(dim(train_for_cat)[1]/30,0) на 10
    predictions1 <- predict(fit1_for_cat, train_for_cat)
    acc1_for_cat <- max(fit1_for_cat$results$Accuracy)

    ## выбираем
    best_for_cat_choice <- which(c(acc1_for_cat,
                                   ##--acc2,
                                   use.names = FALSE)==
                                   max(c(acc1_for_cat##--,
                                         ##--acc2
                                   )))

    cat_predictions <- list(predictions1##--,
                            ##--predictions2
    )[[best_for_cat_choice]]

    best_for_cat_model <- list(fit1_for_cat##--,
                               ##--fit2_for_cat
    )[[best_for_cat_choice]]

  } else{
    ## если в таргете нет нулевых значений, то получаем нулевой вектор
    cat_predictions <- integer(0)
  }

  # ----прогнозируем таргет на основе трех целевых переменных--------------------------
  if (length(season_predictions)>0 & length(cat_predictions)>0) {
    target_predictions=(random_predictions+season_predictions)
    target_predictions[cat_predictions==0] <- 0
  } else if (length(season_predictions)>0) {
    target_predictions=random_predictions+season_predictions
  } else if (length(cat_predictions)>0){
    target_predictions=random_predictions
    target_predictions[cat_predictions==0] <- 0
    target_predictions[is.na(target_predictions)] <- 1
  } else target_predictions=random_predictions
  ## -----выводим переменные------------
  if (target_class != 'binary') {
    trainMetric <- sqrt(sum((target_predictions -
                               train$target)^2)/
                          length(target_predictions))

    if (length(season_predictions)>0 & length(random_predictions) >0) {
      train2Metric <- sqrt(sum((season_predictions+random_predictions-
                                  train$target)^2)/
                             length(target_predictions))
    } else {
      train2Metric <- sqrt(sum((random_predictions-
                                  train$target)^2)/
                             length(target_predictions))
    }

    if (length(numeric_feat)==0 )  {rmse3_for_random_num <- NA}
    ##best_for_random_num_model
    if (length(binary_feat)==0 ) {rmse1_for_random_bin <- NA}
    ##best_for_random_bin_model
    if (length(fac_feat) >0 ) {
      rmse_for_random_fac <- min(model_for_random_fac$results$RMSE)
    } else {
      rmse_for_random_fac <- NA
    }

    if (length(cat_predictions)==0 ) {acc1_for_cat <- NA}

  } else {
    trainMetric <- (table(target_predictions, train$target)[1]+
                      table(target_predictions, train$target)[4])/
      sum(table(target_predictions, train$target)[1:4])

    train2Metric <- (table(cat_predictions, train$target)[1]+
                       table(cat_predictions, train$target)[4])/
      sum(table(cat_predictions, train$target)[1:4])  }

  final_model <- list(target_class=target_class,
         zero_share=zero_share,
         date_feat=date_feat,
         train_date=train_date,
         numeric_feat=numeric_feat,
         binary_feat1=binary_feat1,
         binary_feat=binary_feat,
         fac_feat1=fac_feat1,
         fac_feat=fac_feat,
         fac_binary_feat=fac_binary_feat,
         id_feat=id_feat,
         integer_feat=integer_feat,
         ts_feat=ts_feat,
         trainMetric=trainMetric,
         train2Metric=train2Metric,
         highCor=highCor,
         na_var=na_var,
         preobj=preobj,
         preobj_for_random_fac=preobj_for_random_fac,
         preobj_for_cat=preobj_for_cat,
         preobj_for_cat2=preobj_for_cat2,
         best_for_random_num_model=best_for_random_num_model,
         best_for_random_bin_model=best_for_random_bin_model,
         model_for_random_fac=model_for_random_fac,
         combined_random_model=combined_random_model,
         train_for_random_fac=train_for_random_fac,
         best_for_cat_model=best_for_cat_model,
         season_predictions=season_predictions,
         random_predictions=random_predictions,
         cat_predictions=cat_predictions,
         target_predictions=target_predictions)

}
