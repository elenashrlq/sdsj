add_seas_feat <- function(x, ts_feat, data_gov) {
  x$season_fac0 <- as.factor(wday(x[,ts_feat]))
  x$season_fac1 <- as.factor(month(x[,ts_feat]))
  x$season_fac2 = rep(0, length(x[,ts_feat]))
  x$season_fac2[wday(x[,ts_feat])==7 | wday(x[,ts_feat])==1] <- 1
  x$season_fac2 <- as.factor(x$season_fac2)
  x$season_fac3 <- as.factor(day(x[,ts_feat]))
  x$season_fac4 <- rep(0, dim(x)[1])
  x$season_fac4[x[,ts_feat] %in%
                  as.Date(data_gov$datetime[data_gov$status=='holiday']) &
                  !(weekdays(x[,ts_feat]) %in% c("суббота",
                                                 "воскресенье"))] <- 1
  x$season_fac4 <- as.factor(x$season_fac4)
  x$season_fac5 <- rep(0, dim(x)[1])
  x$season_fac5[x[,ts_feat] %in%
                  as.Date(data_gov$datetime[data_gov$status==
                                              'shortday'])] <- 1
  x$season_fac5 <- as.factor(x$season_fac5)
  x
}
