robustRegH<-function(y,X,tune=1.345,beta,m=TRUE,max.it=1000,tol=1e-10){
r3<-function(x){return(round(x,3))}
bi<-FALSE
if(m==FALSE){bi<-TRUE}
n<-length(y)
p<-length(beta)
X<-as.matrix(X)
j<-rep(1,times=length(y))
X<-cbind(j,X)
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
 
 psiH<-psiHuber(rstar,tune)
 w<-psiH/rstar  
 W<-diag(w[1:length(w),])  
 C<-solve(a=t(X)%*%W%*%X)
 b<-C%*%t(X)%*%W%*%y 
 if(i>10){
  if(sum(abs(b-b_temp))<tol){
   cat("\nRobust Regression with Huber Function\n")
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
 sbsq<-(s^2*(n^2/(n-length(b)))*sum(psiH^2))/sum(derivPsiHuber(rstar,tune))^2
 F<-msr/sbsq
 c<-diag(C)
 sec<-sqrt(sbsq*c)
 t<-b/sec
 cat("source","\t","SS","\t","\t","df","\t","MS","\t","\t","F","\n")
 cat("model","\t",r3(ssreg),"\t",dfr,"\t",r3(msr),"\t",r3(F),"\n")
 cat("error","\t",sserr,"\t",dferr,"\t",r3(mse),"\n")
 cat("tot","\t",r3(sstot),"\t",dftot,"\n")
 cat("rsquared ",r3(ssreg/sstot),"\n","\n")
 row.names(b)[1]<-"intercept"
 estimates<-r3(cbind(b,sec,t))
 colnames(estimates)<-c("estimate","se","t")
 print(estimates)
 }
 return(b)
  
}

