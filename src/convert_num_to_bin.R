## конвертируем числовые данные со значениями 0 и 1 в бинарные данные
convert_num_to_bin <- function(x, binary_feat1=binary_feat1) {
  x[binary_feat1] <- as.data.frame(sapply(x[binary_feat1], as.factor))
  x
}
