tree_predict<-function(tree,x,node){
  a<-which(tree[,1]==node);
  if(a==NULL){
    return(cla);
  }
  for(i in a){
    if(tree[i,2]==x[node]){
      cla<-tree_predict(tree,x,tree[i,3]);
    }
    return(cla);
  }
}