startkmeans<-function(D,k){
  a<-k_mean(D,k);
  D<-rbind(D,a[[2]]);
  a[[1]]<-c(a[[1]],rep(0,nrow(a[[2]])));
  train<-cbind(D,a[[1]]);
  train<-as.data.frame(train);
  names(train)<-c("x","y","class");
  train$class<-as.factor(train$class);
  p<-ggplot(data = train, mapping = aes(x = x, y = y,shape=class,colour=factor(class)));
  p + geom_point();

}