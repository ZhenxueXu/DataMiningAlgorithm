create_deci_tree<-function(D,attribute_list,tree){
  if(nrow(unique(D["buys_computer"]))==1){
    node_next<-unique(D["buys_computer"])[1,1];
    return(list(node_next,tree));
  }
  if(length(attribute_list)==0){
    t<-0;
    for (i in as.matrix(unique(D["buys_computer"]))) {
      if(length(D["buys_computer"][D["buys_computer"]==i])>t){
        t<-length(D["buys_computer"][D["buys_computer"]==i]);
        node_next<-i;
      }
    }
    return(list(node_next,tree));
  }
  
  
   father_name<-attribute_selection(D,attribute_list);
   node_next<-father_name;
   values<-as.matrix(unique(D[father_name]));
   for(j in values){
     Dj<-D[D[father_name]==j,];
     v<-names(Dj)==father_name;
     Dj<-Dj[!v];
     if(is.null(Dj)){
       t<-0;
       for (i in as.matrix(unique(D["buys_computer"]))) {
         if(length(D["buys_computer"][D["buys_computer"]==i])>t){
           t<-length(D["buys_computer"][D["buys_computer"]==i]);
           node_next<-i;
         }
       }
     }else{
       att<-attribute_list[!attribute_list==father_name];
       node<-create_deci_tree(Dj,att,tree);
       node_next<-node[[1]];
       tree<-node[[2]];
     }
     t<-data.frame(father_name,value=j,node_next)
     tree<-rbind(tree,t);
      
   }
   return(list(father_name,tree));
  
}