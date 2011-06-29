derivPsiBiSquare<-function(r,c){
true<-abs(r)<=c
false<-abs(r)>c
psi<-(1-r^2/c^2)*(1-(5*r^2)/c^2)*true+false*0
return(psi)
}
