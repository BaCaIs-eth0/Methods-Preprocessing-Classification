
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

getNeighbors <- function(x, minority.instances, train){  
  dis<-sapply(1:dim(minority.instances)[1],
              function(index) distance(x,index,train))
  orden<-order(dis,decreasing = T)
  return(minority.instances[orden[1:5],])
}


syntheticInstance<- function(x, vecinos){
  index<-runif(1,1,dim(vecinos)[1])
  vecino<-vecinos[index,]
  synt<-cbind(rbind(sapply(1:(length(vecino)-1),function(n) randVec(n,x,vecino) )),vecino$Class)
  return(synt)
}

randVec<-function(n,x,vecino){
  if(vecino[n]<x[n]){
    max=as.numeric(x[n])
    min=as.numeric(vecino[n]) 
  }else{
    max=as.numeric(vecino[n])
    min=as.numeric(x[n])
  }
  punt<-runif(1,min,max)
  return(punt)
} 

ImputSynthetic<- function(){}


SMOTE<-function(datos,semilla=0){
  if(semilla!=0){
    set.seed(semilla)
  }
  tipo <- unique(datos[,dim(datos)[2]])
  vec1<- datos[datos[,dim(datos)[2]]==tipo[1],]
  vec2<- datos[datos[,dim(datos)[2]]==tipo[2],]
  
  diff<-abs((dim(vec1)[1]-dim(vec2)[1]))
  
  if(dim(vec1)[1]>dim(vec2)[1]){
    posiciones<-sample(dim(vec2)[1],diff,replace = T)    
    Vecindario<-lapply(1:length(posiciones),function(x)getNeighbors(posiciones[x],vec2,train = datos))  
    elementos<-t(sapply(1:length(posiciones),function(x)syntheticInstance(vec2[posiciones[x],],Vecindario[[x]]))) 
    
  }else{
    posiciones<-sample(dim(vec1)[1],diff,replace = T)
    Vecindario<-lapply(1:length(posiciones),function(x)getNeighbors(posiciones[x],vec1,train = datos))  
    elementos<-t(sapply(1:length(posiciones),function(x)syntheticInstance(vec1[posiciones[x],],Vecindario[[x]])))
  }
  elementos<-as.data.frame(elementos)
  names(elementos)<-names(datos)
  dat<-rbind(vec1,vec2,elementos)
}
