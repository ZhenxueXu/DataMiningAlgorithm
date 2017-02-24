dbscan<-function(D,r,minpts){
  m<-nrow(D);
  n<-ncol(D);
  cla<-rep(0,time=m);
  c_tag<-0;
  dist<-matrix(numeric(0),nrow=m,ncol=0);
  for(i in 1:m){
    t<-sqrt((D[,1]-D[i,1])^2+(D[,2]-D[i,2])^2);
    t<-as.matrix(t);
    dist<-cbind(dist,t);
  }
  isvisited<-rep(FALSE,time=m);
  
  while(TRUE){
    v_index<-which(isvisited==FALSE);
    p_index<-sample(v_index,size=1);
    p<-D[p_index,];
    isvisited[p_index]<-TRUE;
    pr_index<-which(dist[p_index,]<=r);
    if(length(pr_index)>=minpts){
      c_tag<-c_tag+1;
      cla[p_index]<-c_tag;
      N<-pr_index;
      j<-1;
      while(j<=length(N)){
        pp<-N[j];
        if(!isvisited[pp]){
          isvisited[pp]<-TRUE;
          ppr<-which(dist[pp,]<=r);
          if(length(ppr)>=minpts){
            N<-c(N,ppr); 
            if(cla[pp]==0){
              cla[pp]=c_tag;
            }
          }
         
        }
        
        j<-j+1;
          
      }
    }
    else{
      cla[p]=-1;
    }
    if(all(isvisited)==TRUE){
      return (cla);
    }
      
  }
  return(cla);
    
}