psiHuber<-function(r,c){
middle<-abs(r)<=c
high<- r>c
low<-r<(-c) 
h<-middle*r + high*c + low*-c
return(h)}
