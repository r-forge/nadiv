LRTest<-function(lambda = NULL, full = NULL, reduced = NULL, df = 1){
    if(is.null(lambda)) lambda<-(-2*(full-reduced))
    lrtP <- pchisq(-lambda, df=df, lower.tail = TRUE, log.p = TRUE)
 list(lambda = lambda, Pval = lrtP)
}

