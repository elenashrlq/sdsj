## конвертируем числовые данные со значениями 0 и 1 в бинарные данные
convert_num_to_bin <- function(x, num_binary_feat=num_binary_feat) {
  x[num_binary_feat] <- as.data.frame(sapply(x[num_binary_feat], as.factor))
  x
}
