params <- list(
  a=list(cv=c(20,30,20,20,12),
         method=c('lasso', 'bstTree', 'glm', 'gbm', 'rpart'),
         corlim=rep(.7,5)),
  b=list(cv=c(20,20,20,20,20),
         method=c('lasso', 'treebag', 'lasso', 'glm', 'nnet'),
         corlim=rep(.7,5)),
  c=list(cv=c(30,30,30,30,30),
         method=c('gbm', 'bstTree', 'glm', 'glm', 'svmRadial'),
         corlim=rep(.7,5))
)
