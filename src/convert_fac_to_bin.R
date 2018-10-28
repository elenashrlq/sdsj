## конвертируем факторы с двумя значениями в бинарные данные
convert_fac_to_bin <- function(x, fac_binary_feat=fac_binary_feat){
  if (length(fac_binary_feat)>0) {
    for (i in 1:length(fac_binary_feat)) {
      levels(x[,fac_binary_feat[i]]) <- c(1,0)
    }
  }
  x
}
