makeA<-function (pedigree) 
{
A <- zapsmall(solve(inverseA(pedigree)$Ainv), 10)
A
}

