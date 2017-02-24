k_mean<-function(D,k){
  n<-nrow(D);
  m<-ncol(D);
  index<-sample(n,size=k);
  Ck<-D[index,];
  while(TRUE){
    dist<-matrix(numeric(0),nrow=n,ncol=0);
    for(j in c(1:k)){
      tmp<-sqrt((D[,1]-D[j,1])^2+(D[,2]-D[j,2])^2);
      tmp<-as.matrix(tmp);
      dist<-cbind(dist,tmp);
      }
    clas<-apply(dist,1,function(t)return(which(t==min(t))));
    
    Ck1<-matrix(numeric(0),nrow=0,ncol=m);
  for(i in c(1:k)){
    v<-which(clas==i);
    c<-apply(D[v,],2,mean);
    Ck1<-rbind(Ck1,c);
  }
  if(all(abs(Ck1-Ck)<=0.1)){
    
    
    return (list(clas,Ck));
  }
   
  Ck<-Ck1;
  
  }
  
}