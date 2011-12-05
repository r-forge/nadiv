makeA<-function (pedigree) 
{
  numeric.pedigree <- numPed(pedigree) 
  N<-length(numeric.pedigree[,1])
  ped<-t(numeric.pedigree)
  ped2<-rbind(ped, genAssign(numeric.pedigree))
  nonzero<-ped2[1:3, ped2[4,]>0]
  offset<-nonzero[1,1]-1
  increment<-round(((ceiling(dim(nonzero)[2]*0.05)-(dim(nonzero)[2]*0.05))*20)+0.1)
  if(!increment==0) {
    addition<-matrix((nonzero[1, dim(nonzero)[2]]+1), nrow=3, ncol=increment)
    nonzero<-cbind(nonzero, addition)
    N<-N+increment
  } 
  kmat <- diag(0.5,N)

  mykin_out<-.Fortran(make_a, kmat_n=as.integer(N), 
	kmat=as.single(kmat),
	ped_n=as.integer(dim(nonzero)[2]),
	ped=as.integer(nonzero),
	offset=as.integer(offset))

  A<-matrix(mykin_out$kmat, nrow=N, ncol=N)
  if(!increment==0) A<-A[1:length(numeric.pedigree[,1]), 1:length(numeric.pedigree[,1])]
  dimnames(A) <- list(pedigree[,1], pedigree[,1])
A
}

