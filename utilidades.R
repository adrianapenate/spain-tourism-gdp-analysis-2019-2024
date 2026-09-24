
# Función que hace un "left join" entre dos vectores de strings
# sV1 y sV2 usando la función adist() de la librería utils para
# comparar strings. Devuelve un tibble con los siguientes campos:
#   pos1         : la posición en sV1 con el string más cercano a sV2
#   pos2         : la posición en sV2 con el string más cercano a sV1
#   left_value  : el valor de sV1
#   right_value : el valor de sV2 más cercano a sV1
#   dis         : las distancia entre sV1 y su valor más cercano en sV2
# Nota : hay que tener en cuenta que la comparación puede fallar
# y por tanto es conveniente verificar los resultados
library(utils)
LeftJoinNearestString <- function(u,v,ignore_case=FALSE,partial_=FALSE){

  res <- tibble(
    pos1 =1:nrow(u),
    pos2 = rep(0,nrow(u)),
    value1 = character(nrow(u)),
    value2 = character(nrow(u)),
    dis = rep(100000,nrow(u))
  )

  

  jz <- rep(0,nrow(v))

  for(i in 1:nrow(u)){
    for(j in 1:nrow(v)){
      if(jz[j]==1) next
      for(n in 1:ncol(u)){
        if(is.na(u[i,n])==TRUE) next
        uin <- as.character(u[i,n])
        for(m in 1:ncol(v)){
          if(res$dis[i]==0) break
          if(is.na(v[j,m])==TRUE) next
          vjm <- as.character(v[j,m])
          dis_ij <- adist(uin,vjm,ignore.case=ignore_case,partial=partial_)
          if(dis_ij<res$dis[i]){
            res$dis[i] <- dis_ij
            res$pos2[i] <- j
            res$value1[i] <- uin
            res$value2[i] <- vjm
          }
        }
      }
      if(res$dis[i]==0){
        jz[res$pos2[i]] <- 1
        break
      }
    }
  }
  return(res)

}


# Dada una variable en formato de fecha, Date, esta función calcula la fecha del
# último del mes corriente
ultimoDiaMes <-function(date){
  date2 <- as.Date(date)
  while(
    as.numeric(format(date2+1,"%d"))>
    as.numeric(format(date2,"%d"))
  ){
    date2=date2+1
  }
  return(date2)
}

# Calcula la derivada de una serie temporal
derivada <-function(data){
  res <- numeric(length(data))
  res[1] <- NA
  for(i in 2:length(res) ){
    res[i]=data[i]-data[i-1]
  }
  return(res)
}

