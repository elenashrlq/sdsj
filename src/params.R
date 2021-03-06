params <- list(

  one=list(cv=c(30,30,20,20,20),
           method=c('gbm', 'bstTree', 'glm', 'glm', 'nnet'),
           corlim=c(.7)),

  nine=list(cv=c(10,10,10,10,10),
            method=c('gbm', 'treebag', 'glm', 'glm', 'rpart'),
            corlim=c(.7)),

  three=list(cv=c(5,5,5,5,5),
             method=c('lasso', 'glmnet', 'glm', 'glm', 'rpart'), ##можно заменить на 20
             corlim=c(.85)),

  four=list(cv=c(0,0,0,0,20),
            method=c('', '', '', '', 'lda'),
            corlim=c(.95)),

  five=list(cv=c(0,0,0,0,20),
            method=c('', '', '', '', 'lda'),
            corlim=c(.95)),

  six=list(cv=c(0,0,0,0,20),
           method=c('', '', '', '', 'lda'),
           corlim=c(.95)),

  seven=list(cv=c(0,0,0,0,15),
             method=c('', '', '', '', 'lda'),
             corlim=c(.75)), ##можно заменить на .95

  eight=list(cv=c(0,0,0,0,15),
             method=c('', '', '', '', 'lda'),
             corlim=c(.75)),
  
  nine=list(cv=c(10,10,10,10,10),
            method=c('gbm', 'treebag', 'glm', 'glm', 'rpart'),
            corlim=c(.7))
)

##two=list(cv=c(20,20,20,20,20),
##         method=c('gbm', 'treebag', 'glm', 'glm', 'nnet'),
##         corlim=c(.7)),
