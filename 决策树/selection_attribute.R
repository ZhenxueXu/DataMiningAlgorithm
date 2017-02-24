attribute_selection<-function (D,attribute_list){
  infoD<-0;
  for(k in as.matrix(unique(D["buys_computer"]))){
    a=length(D["buys_computer"][D["buys_computer"]==k])/nrow(D)
    infoD<-infoD-a*log2(a);
  }
  max_gain<-0;
  for (i in attribute_list){
    
    v<-as.matrix(unique(D[i]));
    info<-0;
    for (j in v){
      total<-0;
      t<-length(D[i][D[i]==j])/nrow(D);
      for(k in as.matrix(unique(D["buys_computer"]))){
        r<-length(D[i][D[i]==j&D["buys_computer"]==k])/length(D[i][D[i]==j]);
        ifelse(r==0,total<-total-0,total<-total-r*log2(r));
      }
      info=info+t*total;
    }
    gain=infoD-info;
    if(gain>max_gain){
      max_gain<-gain;
      att<-i;
      
    }
      
    
  }
  return (att);
}