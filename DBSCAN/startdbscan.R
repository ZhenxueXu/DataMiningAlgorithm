sstartdbscan<-function(data,r,min){
  ac<-dbscan(data,r,min);
  train<-cbind(data,ac);
  train<-as.data.frame(train);
  names(train)<-c("x","y","class");
  train$class<-as.factor(train$class);
  p<-ggplot(data = train, mapping = aes(x = x, y = y,colour=factor(class)));
  p + geom_point();
  
}