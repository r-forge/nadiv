makeA<-function (pedigree) 
{
A <- zapsmall(solve(inverseA(pedigree)$Ainv), 8)
A
}

