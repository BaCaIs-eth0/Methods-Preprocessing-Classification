
# Distance function
distance <- function(i, j, data){
  sum <- 0
  for(f in 1:dim(data)[2]){
    if(is.factor(data[,f])){ # nominal feature
      if(data[i,f] != data[j,f]){
        sum <- sum + 1
      }
    } else {
      sum <- sum + (data[i,f] - data[j,f]) * (data[i,f] - data[j,f])
    }
  }
  sum <- sqrt(sum)
  return(sum)
}


#random undersampling
RUS<-function(datos){
  tipo <- unique(datos[,dim(datos)[2]])
  vec1<- datos[datos[,dim(datos)[2]]==tipo[1],]
  vec2<- datos[datos[,dim(datos)[2]]==tipo[2],]
  
  diff<-abs((dim(vec1)[1]-dim(vec2)[1]))
  
  if(dim(vec1)[1]>dim(vec2)[1]){
    posiciones<-sample(dim(vec1)[1],dim(vec1)[1]-diff,replace = T)
    elementos<-vec1[posiciones,]
  }else{
    posiciones<-sample(dim(vec2)[1],dim(vec2)[1]-diff,replace = T)
    elementos<-vec2[posiciones,]
  }
  dat<-rbind(vec1,elementos)
}
