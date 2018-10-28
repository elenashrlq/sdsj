##убираем колонки, в которых не удалось восстановить с NA
delete_na_cols <- function(x, na_var) {
  if (length(na_var)>0) {
    x <- x[,-which(names(x) %in% na_var)]
  }
  x
}