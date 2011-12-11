makeA<-function (pedigree) 
{
Asparse <- zapsmall(solve(inverseA(pedigree)$Ainv), 10)
A <- as(Asparse, "matrix")
A
}

