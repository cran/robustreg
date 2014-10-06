robustRegBS<-function(formula,data,tune=4.685,m=TRUE,max.it=1000,tol=1e-10){

psiBiSquare<-function(r,c){
true<-abs(r)<=c
false<-abs(r)>c
psi<-true*(r*(1-(r/c)^2)^2)+false*0
return(psi)
}

derivPsiBiSquare<-function(r,c){
true<-abs(r)<=c
false<-abs(r)>c
psi<-(1-r^2/c^2)*(1-(5*r^2)/c^2)*true+false*0
return(psi)
}
	
r3<-function(x){return(round(x,3))}
r2<-function(x){return(round(x,2))}

bi<-FALSE
if(m==FALSE){bi<-TRUE}


modelFrame=model.frame(formula,data)
X=model.matrix(formula,data)
y=model.extract(modelFrame,"response")

beta=lm(formula,data)$coefficients
n<-length(y)
p<-length(beta)

b<-beta
if(bi){
 tune<-(tune*sqrt(2*p*n))/(n-2*p)
 H<-X%*%solve(a=t(X)%*%X)%*%t(X)
 hii<-diag(H)
 pi<-(1-hii)/sqrt(hii)}

convergence<-FALSE
for(i in 1:max.it){
 b_temp<-b
 r<-y-X%*%b
 s<-median(abs(r-median(r)))/.6745

 if(m){rstar<-(r/s)}
 if(bi){rstar<-r/(s*pi)}
 
 psiBS<-psiBiSquare(rstar,tune)
 w<-psiBS/rstar  
 W<-diag(w[1:length(w),])  
 C<-solve(a=t(X)%*%W%*%X)
 b<-C%*%t(X)%*%W%*%y 
 if(i>10){
  if(sum(abs(b-b_temp))<tol){
   cat("\nRobust Regression with Bisquare Function\n")
   cat("Convergence achieved after:",i,"iterations\n")
   convergence<-TRUE
   break}}
}

if(convergence==FALSE){b<-NULL}
if(convergence){
 ybarw<-sum(y*w)/sum(w)
 ytild<-X%*%b
 ssreg<-sum(w*(ytild-ybarw)^2)
 sserr<-sum(w*(y-ytild)^2)
 sstot<-sum(w*(y-ybarw)^2)
 dfr<-length(b)-1
 dferr<-length(y)-dfr-1
 dftot<-length(y)-1
 msr<-ssreg/dfr
 mse<-sserr/dferr
 sbsq<-(s^2*(n^2/(n-length(b)))*sum(psiBS^2))/sum(derivPsiBiSquare(rstar,tune))^2
 F<-msr/sbsq
 c<-diag(C)
 sec<-sqrt(sbsq*c)
 t<-b/sec
 cat("source","\t","SS","\t","\t","df","\t","MS","\t","\t","F","\n")
 cat("model","\t",r2(ssreg),"\t",dfr,"\t",r2(msr),"\t",r2(F),"\n")
 cat("error","\t",r2(sserr),"\t",dferr,"\t",r2(mse),"\n")
 cat("tot","\t",r2(sstot),"\t",dftot,"\n")
 cat("rsquared ",r2(ssreg/sstot),"\n","\n")
 row.names(b)[1]<-"intercept"
 estimates<-r2(cbind(b,sec,t))
 colnames(estimates)<-c("estimate","se","t")
 print(estimates)
 }
 return(b)
  
}

