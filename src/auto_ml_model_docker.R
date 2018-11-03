## тренируем модель (на основе файла с данными для обучения)
auto_ml_model <- function(file) {
  library(lubridate)
  library(dplyr)
  library(caret)
  ##library(doParallel)

  source('src/params.R')
  source('src/delete_na_cols.R')
  source('src/convert_dates.R')
  source('src/convert_num_to_bin.R')
  source('src/convert_fac_to_bin.R')
  source('src/add_seas_feat.R')

  #-----открываем, чистим, меняем форматы данных --------------
  train_1 <- read.csv(file,
                      encoding = 'UTF-8',
                      na.strings=c("","NaN","NA"))
  data_gov <- read.csv("data/data_gov_conv.csv")

  target_class=character()
  zero_share=0
  ts_feat=character()
  trainMetric=numeric()
  na_varS=character()
  highCor_B=character()
  highCor_N=character()
  preobj=character()
  preobj_prepr=character()
  preobj_for_cat2=character()
  best_for_random_num_model=character()
  best_for_random_bin_model=character()
  best_for_cat_model=character()
  combined_random_model=character()
  model_for_random_fac=character()
  train_date=data.frame()
  train_for_random_fac=data.frame()
  season_predictions=numeric()
  best_for_random_num_prediction=numeric()
  best_for_random_bin_prediction=numeric()
  best_for_random_fac_prediction=numeric()
  random_predictions=numeric()
  cat_predictions=numeric()
  target_predictions=numeric()

  #------разделяем переменные по типу данных, определяем тип таргета-----
  train <- train_1

  feat_classes <- sapply(train, class)

  target <- names(train)[grep('target', names(train))]

  lineid_feat <- names(train)[grep('line_id', names(train))]

  id_feat <- names(train)[grep('id', names(train))]
  id_feat <- setdiff(id_feat, lineid_feat)

  date_feat_scr <- names(train)[grep('datetime_', names(train))]
  numeric_feat_scr <- names(train[feat_classes=='numeric'])
  integer_feat_scr <- names(train[feat_classes=='integer'])
  fac_feat_scr <- names(train[feat_classes=='factor'  &
                                !(names(train) %in% date_feat_scr)])

  fac_binary_feat <- fac_feat_scr[sapply(train[fac_feat_scr], n_distinct)==2]
  num_binary_feat <- c(integer_feat_scr, numeric_feat_scr)[which(
    sapply(train[c(integer_feat_scr,numeric_feat_scr)],n_distinct)==2 &
      sapply(train[c(integer_feat_scr,numeric_feat_scr)],max, na.rm=T)==1 &
      sapply(train[c(integer_feat_scr,numeric_feat_scr)],min, na.rm=T)==0)]

  binary_feat_scr <- c(num_binary_feat, fac_binary_feat)

  numeric_feat_scr <- setdiff(numeric_feat_scr,
                              c(binary_feat_scr, lineid_feat))
  integer_feat_scr <- setdiff(integer_feat_scr,
                              c(binary_feat_scr, lineid_feat))
  fac_feat_scr <- setdiff(fac_feat_scr, c(fac_binary_feat))

  ## конвертируем числовые и бинарные данные
  train <- convert_fac_to_bin(train, fac_binary_feat)
  train <- convert_num_to_bin(train, num_binary_feat)

  ## вычитаем таргет и определяем класс таргета
  if (length(which(numeric_feat_scr %in% target))>0) {
    numeric_numeric_feat_scrfeat1 <- setdiff(numeric_feat_scr, target)
    target_class = 'numeric'}

  if (length(which(integer_feat_scr %in% target))>0) {
    integer_feat_scr <- setdiff(integer_feat_scr, target)
    target_class = 'integer'}

  if (length(which(fac_feat_scr %in% target))>0) {
    fac_feat_scr <- setdiff(fac_feat_scr, target)
    target_class = 'fac'}

  if (length(which(binary_feat_scr %in% target))>0) {
    num_binary_feat <- setdiff(num_binary_feat, target)
    fac_binary_feat <- setdiff(fac_binary_feat, target)
    binary_feat_scr <- setdiff(binary_feat_scr, target)
    target_class = 'binary'}

  ## сортируем
  train <- train[order(train$line_id),]
  target_class

  zero_share <- length(train$target[train$target==0])/length(train$target)

  #------удаляем NA------
  train_prepr <- train[,-c(which(grepl('line_id',names(train))),
                           which(names(train) %in% c(target, id_feat))
  )]

 ## cl <- makePSOCKcluster(4)
 ## registerDoParallel(cl)

  preobj_prepr <- preProcess(train_prepr,
                             method=c('medianImpute', 'zv', 'nzv')
  )

 ## registerDoSEQ()

  train_prepr <- predict(preobj_prepr, train_prepr)

  ##--t_train_prepr <- data.frame(t(train_prepr))
  ##--na_varS <- rownames(t_train_prepr[!complete.cases(t_train_prepr),])

  ##--train_prepr <- delete_na_cols(train_prepr, na_varS)

  train_prepr <- train_prepr[, colSums(is.na(train_prepr)) == 0]

  ## обновляем переменные после возможного удаления
  date_feat <- names(train_prepr)[which(names(train_prepr) %in% date_feat_scr)]

  fac_feat_without_seas <- names(train_prepr)[
    which(names(train_prepr) %in% fac_feat_scr)
    ]

  # ----- выбираем параметры для тюнинга ----
  if (target_class=='binary') {
    if (length(c(binary_feat_scr,numeric_feat_scr))*nrow(train)>20*10^6) {
      param_set=7
    } else {
      param_set=4
    }
  } else {
    if (length(c(binary_feat_scr,numeric_feat_scr))*nrow(train)>5*10^6) {
      param_set=3
    } else {
      if (length(date_feat)>0) {
        param_set=1
      } else {
        param_set=2
      }
    }
  }
  # ----- удаляем коррелирующие переменные ------
  train_num_prep <- train_prepr[,which(names(train_prepr) %in%
                                         c(numeric_feat_scr, integer_feat_scr))]

  highCor_N <- findCorrelation(cor(train_num_prep),
                               cutoff = params[[param_set]]$corlim[1],
                               names = TRUE)

  train_bin_prep <- train_prepr[,which(names(train_prepr) %in%
                                         c(binary_feat_scr))]

  highCor_B_info <- findLinearCombos(train_bin_prep)

  highCor_B <- names(train_bin_prep)[highCor_B_info$remove]

  if (length(c(highCor_N, highCor_B)>0)) {
    train_prepr <- train_prepr[,-which(names(train_prepr) %in%
                                         c(highCor_N, highCor_B))
                               ]
  }

  ## обновляем переменные после возможного удаления
  numeric_feat <- names(train_prepr)[
    which(names(train_prepr) %in% numeric_feat_scr)
    ]

  integer_feat <- names(train_prepr)[
    which(names(train_prepr) %in% integer_feat_scr)
    ]

  binary_feat_without_seas <- names(train_prepr)[
    which(names(train_prepr) %in% binary_feat_scr)
    ]

  train <- data.frame(train[,c(target, id_feat, lineid_feat)],
                      train_prepr)

  # ----- проверяем наличие дат и форматируем их -----
  if (length(date_feat)>0){
    ## конвертируем даты в формат дат
    default_date <- as_date(0)
    train <- convert_dates(train, date_feat)

    ## находим временные ряды
    ts_feat <- date_feat[which(
      (sapply(train[date_feat], max, na.rm=T) -
         sapply(train[date_feat], min, na.rm=T) + 1) /
        sapply(train[date_feat], n_distinct, na.rm=T)==1
    )]

    ## если есть вр.ряды, добавляем сезонные переменные
    if (length(ts_feat)==1) {
      train <- add_seas_feat(train, ts_feat=ts_feat, data_gov=data_gov)

      binary_feat <- c(binary_feat_without_seas, 'season_fac2',
                       'season_fac4', 'season_fac5')
      fac_feat <- c(fac_feat_without_seas, 'season_fac1', 'season_fac3')

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
            target_daily_decompose <- decompose(target_daily_ts, filter = 0)

            train_date_list[[i]] <- mutate(
              train_date_list[[i]],
              season=as.numeric(target_daily_decompose$seasonal),
              random=as.numeric(target_daily_decompose$random))
          }
          train_date <- do.call(rbind, train_date_list)
          train_date <- train_date[order(train_date[,lineid_feat]),]
        }
        else {train_date <- train[,c(ts_feat,
                                     id_feat,
                                     lineid_feat,
                                     target,
                                     'season_fac0')]

        train_date <- train_date[order(train_date[,ts_feat]),]
        target_daily_ts <- ts(train_date[target], frequency=7)
        target_daily_decompose <- decompose(target_daily_ts, filter = 0)
        train_date$season <- as.numeric(target_daily_decompose$seasonal)
        train_date$random <- as.numeric(target_daily_decompose$random)
        train_date <- train_date[order(train_date[,lineid_feat]),]
        }

        season_predictions <- train_date$season
      }
    } else {
      ## если есть несколько дат, но нет одного временного ряда
      binary_feat <- binary_feat_without_seas
      fac_feat <- fac_feat_without_seas
    }
  } else {
    binary_feat <- binary_feat_without_seas
    fac_feat <- fac_feat_without_seas
    train_date <- train[,c(id_feat, lineid_feat, target)]
    train_date$season <- rep(0, dim(train)[1])
    train_date$random <- train[,target]
  }

  # ---- строим модели для прогноза рандомности -----
  if (target_class=='numeric'|target_class=='integer') {

   ## cl <- makePSOCKcluster(4)
   ## registerDoParallel(cl)

    ## ------ строим модель 1 для прогноза рандомности -----
    ## вычисляем рандоность, если таргет числовой
    if (length(c(numeric_feat, integer_feat))>0) {

      train_for_random <- cbind.data.frame(
        train[,-c(which(names(train)=='target'))],
        random=train_date['random'],
        row.names = 1:length(train[,1]))

      train_for_random_num <- train_for_random[c('random',
                                                 'line_id',
                                                 numeric_feat,
                                                 integer_feat)
                                               ]
      preobj <- preProcess(
        train_for_random_num[,-c(which(names(train_for_random_num)=='random'),
                                 which(names(train_for_random_num)=='line_id'))
                             ], method = c('pca'))

      train_for_random_num <- predict(preobj, train_for_random_num)

      set.seed(123)
      best_for_random_num_model <- train(as.numeric(random)~.,
                                         data=train_for_random_num,
                                         method=params[[param_set]]$method[1],
                                         trControl=trainControl(method = "cv",
                                                                number = params[[param_set]]$cv[1]))

      best_for_random_num_prediction <- predict(best_for_random_num_model,
                                                train_for_random_num)

    }

    ## ------строим модель 2 для прогноза рандомности -----
    ## если есть бинарные переменные, то прогнозируем на основе них
    if (length(binary_feat)>0) {

      train_for_random_bin <- train_for_random[,c(binary_feat,
                                                  'random',
                                                  'line_id')]
      set.seed(123)
      best_for_random_bin_model <-
        train(as.numeric(random)~.,
              data=train_for_random_bin[
                ,-which(names(train_for_random_bin)=='line_id')]
              ,
              method=params[[param_set]]$method[2], ## было treebag glmnet
              trControl=trainControl(method = "cv",
                                     number = params[[param_set]]$cv[2]))

      best_for_random_bin_prediction <- predict(best_for_random_bin_model,
                                                train_for_random_bin)

    }

    ## ------строим модель 3 для прогноза рандомности -----
    ## если есть факторы, то прогнозируем на основе них
    if (length(fac_feat)>0) {

      train_for_random_fac <- train_for_random[,c(fac_feat,'random','line_id')]
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
          model_for_random_fac[[i]][order(model_for_random_fac[[i]][,'line_id'])
                                    ,]
        prediction_for_random_fac[[i]] <- model_for_random_fac[[i]][,c('y')]
      }

      train_for_random_fac <- data.frame(do.call(cbind,
                                                 prediction_for_random_fac),
                                         train_for_random_fac)

      model_for_random_fac <- train(
        as.numeric(random)~.,
        data=train_for_random_fac[,-which(grepl('line_id',names(train_for_random_fac)) |
                                            names(train_for_random_fac) %in% fac_feat)
                                  ],
        method=params[[param_set]]$method[3],
        trControl=trainControl(method = "cv",
                               number = params[[param_set]]$cv[3]))

      best_for_random_fac_prediction <- predict(model_for_random_fac,
                                                train_for_random_fac)
    }


    ##registerDoSEQ()

    ## ------строим обобщенную модель для прогноза рандомности-------------
    list_train_combined <- list(
      best_for_random_bin_prediction=best_for_random_bin_prediction,
      best_for_random_num_prediction=best_for_random_num_prediction,
      best_for_random_fac_prediction=best_for_random_fac_prediction
    )

    chosen_train_combined <- do.call(
      cbind,
      list_train_combined[which(sapply(list_train_combined, length)>0)]
    )

    train_combined <- cbind.data.frame(chosen_train_combined,
                                       random=train_for_random[,'random'])

    combined_random_model <-
      train(as.numeric(random)~.,
            data=train_combined,
            method=params[[param_set]]$method[4], ##verbose=FALSE
            trControl=trainControl(method = "cv",
                                   number = params[[param_set]]$cv[4]))

    random_predictions <- predict(combined_random_model, train_combined)

  }

  # ----строим модель для прогноза категории таргета--------------------------
  ## если в таргете есть нулевые значения, находим прогноз категории
  if (zero_share>0) {

    ## обрабатываем данные, если таргет числовой
    if (target_class=='numeric'|target_class=='integer') {

      train_for_cat <-
        cbind.data.frame(
          train_for_random_num[,-which(grepl('random|line_id',
                                             names(train_for_random_num)))
                               ],
          train_for_random_bin[,-which(grepl('random|line_id',
                                             names(train_for_random_bin)))
                               ],
          target=train[,'target'])

      train_for_cat$target[train_for_cat$target!=0] <- 1
      train_for_cat$target[train_for_cat$target==0] <- 0
      train_for_cat$target <- as.factor(train_for_cat$target)

    } else {
      ## обрабатываем данные, если таргет бинарный
      train_for_cat <- train[,-which(grepl('line_id', names(train)))
                             ]

      preobj_for_cat2 <- preProcess(
        train_for_cat[,which(names(train_for_cat) %in%
                               c(numeric_feat, integer_feat))
                      ],
        method=c('pca'), na.remove = TRUE)

      train_for_cat <- predict(preobj_for_cat2, train_for_cat)

    }

    ## меняем названия уровней для построения модели (требуется в R)
    levels(train_for_cat$target) <- c('zero_class', 'one_class')

    ## строим модель на основе обработанных данных
    ##cl <- makePSOCKcluster(4)
    ##registerDoParallel(cl)

    best_for_cat_model <- train(target~.,
                                data=train_for_cat,
                                method=params[[param_set]]$method[5],
                                metric='ROC',
                                trControl=trainControl(method = "cv",
                                                       classProbs = TRUE,
                                                       ##summaryFunction=max,
                                                       summaryFunction = twoClassSummary,
                                                       number = params[[param_set]]$cv[5]))

    ##registerDoSEQ()

    cat_predictions <- predict(best_for_cat_model, train_for_cat)

    ## возвращаем названия уровней
    levels(cat_predictions) <- c(0,1)

  } else {
    ## если в таргете нет нулевых значений, то получаем нулевой вектор
    cat_predictions <- integer(0)

  }

  # ----прогнозируем таргет на основе трех целевых переменных----
  if (length(season_predictions)>0 & length(cat_predictions)>0) {

    target_predictions=(random_predictions+season_predictions)
    target_predictions[cat_predictions==0] <- 0

  } else if (length(season_predictions)>0) {

    target_predictions=random_predictions+season_predictions

  } else if (length(cat_predictions)>0) {

    target_predictions=random_predictions
    target_predictions[cat_predictions==0] <- 0
    target_predictions[is.na(target_predictions)] <- 1

  } else target_predictions=random_predictions

  ## ----оцениваем ошибку и выводим переменные----
  if (target_class != 'binary') {

    trainMetric <- sqrt(
      sum((target_predictions - train$target)^2) /
        length(target_predictions)
    )

  } else {

    trainMetric <- table(target_predictions, train$target)
  }

  list(fac_binary_feat=fac_binary_feat,
       num_binary_feat=num_binary_feat,
       id_feat=id_feat,
       lineid_feat=lineid_feat,
       date_feat=date_feat,
       binary_feat=binary_feat,
       fac_feat=fac_feat,
       numeric_feat=numeric_feat,
       integer_feat=integer_feat,
       ts_feat=ts_feat,
       target_class=target_class,
       train_date=train_date,
       train_for_random_fac=train_for_random_fac,
       zero_share=zero_share,
       trainMetric=trainMetric,
       highCor_N=highCor_N,
       highCor_B=highCor_B,
       na_varS=na_varS,
       preobj_prepr=preobj_prepr,
       preobj=preobj,
       preobj_for_cat2=preobj_for_cat2,
       best_for_random_num_model=best_for_random_num_model,
       best_for_random_bin_model=best_for_random_bin_model,
       model_for_random_fac=model_for_random_fac,
       combined_random_model=combined_random_model,
       train_for_random_fac=train_for_random_fac,
       best_for_cat_model=best_for_cat_model)

}
