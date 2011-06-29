derivPsiHuber<-function(r,c){
true<-abs(r)<=c
false<-(r<c*-1 || r>c)
dph<-true*1 +false*0
return(dph)
}
