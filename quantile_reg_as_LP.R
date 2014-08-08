#My own quantile regression function

rm(list=ls())
cat("\014") 

qr_i<-function(y,x,tau=0.5){
    require(SparseM)
    require(Matrix)
    require(Rmosek)
    n<-nrow(x)
    p<-ncol(x)
    b<-(1-tau)*t(X)*rep(1,n)
    LP <- list()
    LP$sense <- "max"
    LP$dparam$intpnt_nl_tol_rel_gap <- rtol
    LP$c <- -y
    LP$A <- t(x)
    LP$bc <- rbind(blc = b, buc = b) 
    LP$bx <- rbind(blx = rep(0,n)), bux = rep(1,n))
    r <- mosek(QP, opts = list(verbose = verb))
    fit <- r$sol$itr$slc - r$sol$itr$suc    
}


