makeDomEpi <- function(pedigree, output = c("AD", "DD", "both"), Dinverse = FALSE)
{
  type <- match.arg(output)
  Dout <- makeD(pedigree, invertD=Dinverse)


  if(type == "AD"){
    AD <- Dout$A * Dout$D
    ADinv <- Matrix(solve(AD), sparse=TRUE)
    listADinv <-sm2list(ADinv, rownames=pedigree[,1], colnames=c("row", "column", "ADinverse"))
    ADinv <- as(ADinv, "dgCMatrix")
    DD <- NULL
    DDinv <- NULL
    listDDinv <- NULL
    }      

  if(type == "DD"){
    DD <- Dout$D * Dout$D
    DDinv.tmp <- Matrix(solve(DD), sparse=TRUE)
    listDDinv<-sm2list(DDinv.tmp, rownames=pedigree[,1], colnames=c("row", "column", "DDinverse"))
    DDinv <- as(DDinv.tmp, "dgCMatrix")
    AD <- NULL
    ADinv <- NULL
    listADinv <- NULL
   }

  if(type == "both"){
    AD <- Dout$A * Dout$D
    ADinv <- Matrix(solve(AD), sparse=TRUE)
    listADinv <-sm2list(ADinv, rownames=pedigree[,1], colnames=c("row", "column", "ADinverse"))
    ADinv <- as(ADinv, "dgCMatrix")
    DD <- Dout$D * Dout$D
    DDinv.tmp <- Matrix(solve(DD), sparse=TRUE)
    listDDinv<-sm2list(DDinv.tmp, rownames=pedigree[,1], colnames=c("row", "column", "DDinverse"))
    DDinv <- as(DDinv.tmp, "dgCMatrix")
    }
return(list(D=Dout$D, AD=AD, DD=DD, Dinv=Dout$Dinv, ADinv=ADinv, DDinv=DDinv, listDinv=Dout$listDinv, listADinv=listADinv, listDDinv=listDDinv))

}

