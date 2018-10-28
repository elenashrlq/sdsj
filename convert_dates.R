## меняем форматирование всех дат
convert_dates <- function(x, date_feat=date_feat) {
  if (length(date_feat)>0) {
    a <- list()
    for (i in 1:length(date_feat)){
      a[[i]] <- as.Date(x[,date_feat[i]], origin=default_date, format='%Y-%m-%d')
      x[,date_feat[i]] <- a[[i]]
    }
  }
  x
}
