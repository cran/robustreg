psiBiSquare<-function(r,c){
true<-abs(r)<=c
false<-abs(r)>c
psi<-true*(r*(1-(r/c)^2)^2)+false*0
return(psi)
}
