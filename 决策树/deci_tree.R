deci_tree<-function(D,attribute_list){
  decision_tree<-data.frame(father_name=character(0),value=character(0),next_node=character(0));
  r<-create_deci_tree(D,attribute_list,decision_tree);
  result<-r[[2]];
  return(result);
}