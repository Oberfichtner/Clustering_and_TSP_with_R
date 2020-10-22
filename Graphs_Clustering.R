#depending on the number of k we need to adjustice some things
#PAM######################################################################
#k=2#### 
  k=2
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
  m

#k=3#### 
  k=3
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
#k=4#### 
  k=4
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 

  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
#k=5####  
  k=5
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
#k=6####  
  k=6
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
#k=7####  
  k=7
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
#k=8####  
  k=8
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
#k=9####  
  k=9
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
#k=10####  
  k=10
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
#k=11####  
  k=11
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
   
#k=12####  
  k=12
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()

  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
#k=13####  
  k=13
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}

  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 

  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
#k=14####  
  k=14
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 

  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
#k=15####  
  k=15
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);

  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
#k=16####
  k=16
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
#k=17####
  k=17
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
#k=18######
  k=18
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
#k=19##################
  k=19
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
#k=20############################################################################
k=20
  pam.res<-pam(D,k,diss=TRUE)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 

#fastPAM###################################################################
  #k=2#### 
  k=2
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
    m

  #k=3#### 
  k=3
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
  #k=4#### 
  k=4
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
  #k=5####  
  k=5
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
  #k=6####  
  k=6
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
  #k=7####  
  k=7
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
  #k=8####  
  k=8
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
  #k=9####  
  k=9
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
  #k=10####  
  k=10
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
  #k=11####  
  k=11
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
  
  #k=12####  
  k=12
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
  #k=13####  
  k=13
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
  #k=14####  
  k=14
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
  #k=15####  
  k=15
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
  #k=16####
  k=16
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
  #k=17####
  k=17
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
  #k=18######
  k=18
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
  #k=19##################
  k=19
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
  #k=20####
  k=20
  pam.res<-pam(D,k,diss=TRUE,pamonce = 5)
  cluster <- as.numeric(pam.res$clustering)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 

  
#fastkmed###################################################################
  #k=2#### 
  k=2
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
  m
  
  #k=3#### 
  k=3
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
  #k=4#### 
  k=4
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
  #k=5####  
  k=5
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
  #k=6####  
  k=6
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
  #k=7####  
  k=7
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
  #k=8####  
  k=8
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
  #k=9####  
  k=9
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
  #k=10####  
  k=10
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
  #k=11####  
  k=11
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
  
  #k=12####  
  k=12
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
  #k=13####  
  k=13
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
  #k=14####  
  k=14
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
  #k=15####  
  k=15
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
  #k=16####
  k=16
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
  #k=17####
  k=17
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
  #k=18#####
  k=18
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
  #k=19####
  k=19
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
  #k=20####
  k=20
  fast.res <- fastkmed(D, k, iterate = 200)
  cluster <- as.numeric(fast.res$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 
  
  
#rankmed###################################################################
  #k=2#### 
  k=2
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
  m
  
  #k=3#### 
  k=3
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
  #k=4#### 
  k=4
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
  #k=5####  
  k=5
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
  #k=6####  
  k=6
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
  #k=7####  
  k=7
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
  #k=8####  
  k=8
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
  #k=9####  
  k=9
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
  #k=10####  
  k=10
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
  #k=11####  
  k=11
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
  
  #k=12####  
  k=12
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
  #k=13####  
  k=13
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
  #k=14####  
  k=14
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
  #k=15####  
  k=15
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
  #k=16####
  k=16
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
  #k=17####
  k=17
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
  #k=18#####
  k=18
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
  #k=19####
  k=19
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
  #k=20####
  k=20
  rkm <- rankkmed(D, ncluster = k, m = 5, iterate = 50)
  cluster <- as.numeric(rkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 
  
  
  
#inckmed###################################################################
  #k=2#### 
  k=2
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
  m
  
  #k=3#### 
  k=3
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
  #k=4#### 
  k=4
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
  #k=5####  
  k=5
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
  #k=6####  
  k=6
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
  #k=7####  
  k=7
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
  #k=8####  
  k=8
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
  #k=9####  
  k=9
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
  #k=10####  
  k=10
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
  #k=11####  
  k=11
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
  
  #k=12####  
  k=12
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
  #k=13####  
  k=13
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
  #k=14####  
  k=14
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
  #k=15####  
  k=15
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
  #k=16####
  k=16
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
  #k=17####
  k=17
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
  #k=18#####
  k=18
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
  #k=19####
  k=19
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
  #k=20####
  k=20
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 
  
  #inckmed#alpha=2.5#########################################################
  #k=2#### 
  k=2
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)
  m
  
  #k=3#### 
  k=3
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list();  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)
  m
  #k=4#### 
  k=4
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4)
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)
  m
  #k=5####  
  k=5
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)
  m
  #k=6####  
  k=6
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)
  m
  
  #k=7####  
  k=7
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)
  m
  
  #k=8####  
  k=8
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}  
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)
  m
  #k=9####  
  k=9
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)
  m
  
  #k=10####  
  k=10
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)
  m
  
  #k=11####  
  k=11
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c()
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)
  m
  
  #k=12####  
  k=12
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12) 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)
  m
  
  #k=13####  
  k=13
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); 
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)
  m
  #k=14####  
  k=14
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); 
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)
  m
  
  
  #k=15####  
  k=15
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)
  m
  
  #k=16####
  k=16
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)
  m
  
  #k=17####
  k=17
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)
  m
  
  #k=18#####
  k=18
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)
  m
  
  #k=19####
  k=19
  inkm <- inckmed(D, ncluster = k, alpha = 2.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19)
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)
  m
  
  #k=20####
  k=20
  inkm <- inckmed(D, ncluster = k, alpha = 1.5, iterate = 200)
  cluster <- as.numeric(inkm$cluster)
  c1 <- list(); c2 <- list(); c3 <- list(); c4 <- list(); c5 <- list()
  c6 <- list(); c7 <- list(); c8 <- list(); c9 <- list(); c10 <- list()
  c11 <- list(); c12 <- list(); c13 <- list(); c14 <- list(); c15 <- list()
  c16 <- list(); c17 <- list(); c18 <- list(); c19 <- list(); c20 <- list()
  for (i in 1:n){
    if (cluster[i] == 1){c1 = append(c1, i)}
    if (cluster[i] == 2){c2 = append(c2, i)}
    if (cluster[i] == 3){c3 = append(c3, i)} 
    if (cluster[i] == 4){c4 = append(c4, i)} 
    if (cluster[i] == 5){c5 = append(c5, i)}
    if (cluster[i] == 6){c6 = append(c6, i)}
    if (cluster[i] == 7){c7 = append(c7, i)} 
    if (cluster[i] == 8){c8 = append(c8, i)} 
    if (cluster[i] == 9){c9 = append(c9, i)}
    if (cluster[i] == 10){c10 = append(c10, i)}
    if (cluster[i] == 11){c11 = append(c11, i)} 
    if (cluster[i] == 12){c12 = append(c12, i)} 
    if (cluster[i] == 13){c13 = append(c13, i)}
    if (cluster[i] == 14){c14 = append(c14, i)}
    if (cluster[i] == 15){c15 = append(c15, i)} 
    if (cluster[i] == 16){c16 = append(c16, i)} 
    if (cluster[i] == 17){c17 = append(c17, i)}
    if (cluster[i] == 18){c18 = append(c18, i)}
    if (cluster[i] == 19){c19 = append(c19, i)} 
    if (cluster[i] == 20){c20 = append(c20, i)} 
  }
  c1 = as.numeric(c1); c2 = as.numeric(c2); c3 = as.numeric(c3); c4 = as.numeric(c4); c5 = as.numeric(c5);
  c6 = as.numeric(c6); c7 = as.numeric(c7); c8 = as.numeric(c8); c9 = as.numeric(c9); c10 = as.numeric(c10);
  c11 = as.numeric(c11); c12 = as.numeric(c12); c13 = as.numeric(c13); c14 = as.numeric(c14); c15 = as.numeric(c15);
  c16 = as.numeric(c16); c17 = as.numeric(c17); c18 = as.numeric(c18); c19 = as.numeric(c19); c20 = as.numeric(c20);
  
  yc1=c();xc1=c(); dc1=c(); yc2=c();xc2=c(); dc2=c()
  yc3=c();xc3=c(); dc3=c();  yc4=c();xc4=c(); dc4=c() 
  yc5=c();xc5=c(); dc5=c();  yc6=c();xc6=c(); dc6=c()
  yc7=c();xc7=c(); dc7=c();  yc8=c();xc8=c(); dc8=c()  
  yc9=c();xc9=c(); dc9=c();  yc10=c();xc10=c(); dc10=c() 
  yc11=c();xc11=c(); dc11=c();  yc12=c();xc12=c(); dc12=c()
  yc13=c();xc13=c(); dc13=c();  yc14=c();xc14=c(); dc14=c() 
  yc15=c();xc15=c(); dc15=c();  yc16=c();xc16=c(); dc16=c()
  yc17=c();xc17=c(); dc17=c();  yc18=c();xc18=c(); dc18=c()  
  yc19=c();xc19=c(); dc19=c();  yc20=c();xc20=c(); dc20=c()
  for (i in c1){yc1=append(yc1,data$LATITUDE[i]);xc1=append(xc1,data$LONGITUDE[i])}
  for (i in c2){yc2=append(yc2,data$LATITUDE[i]);xc2=append(xc2,data$LONGITUDE[i])}
  for (i in c3){yc3=append(yc3,data$LATITUDE[i]);xc3=append(xc3,data$LONGITUDE[i])}
  for (i in c4){yc4=append(yc4,data$LATITUDE[i]);xc4=append(xc4,data$LONGITUDE[i])}
  for (i in c5){yc5=append(yc5,data$LATITUDE[i]);xc5=append(xc5,data$LONGITUDE[i])}
  for (i in c6){yc6=append(yc6,data$LATITUDE[i]);xc6=append(xc6,data$LONGITUDE[i])}
  for (i in c7){yc7=append(yc7,data$LATITUDE[i]);xc7=append(xc7,data$LONGITUDE[i])}
  for (i in c8){yc8=append(yc8,data$LATITUDE[i]);xc8=append(xc8,data$LONGITUDE[i])}
  for (i in c9){yc9=append(yc9,data$LATITUDE[i]);xc9=append(xc9,data$LONGITUDE[i])}
  for (i in c10){yc10=append(yc10,data$LATITUDE[i]);xc10=append(xc10,data$LONGITUDE[i])}
  for (i in c11){yc11=append(yc11,data$LATITUDE[i]);xc11=append(xc11,data$LONGITUDE[i])}
  for (i in c12){yc12=append(yc12,data$LATITUDE[i]);xc12=append(xc12,data$LONGITUDE[i])}
  for (i in c13){yc13=append(yc13,data$LATITUDE[i]);xc13=append(xc13,data$LONGITUDE[i])}
  for (i in c14){yc14=append(yc14,data$LATITUDE[i]);xc14=append(xc14,data$LONGITUDE[i])}
  for (i in c15){yc15=append(yc15,data$LATITUDE[i]);xc15=append(xc15,data$LONGITUDE[i])}
  for (i in c16){yc16=append(yc16,data$LATITUDE[i]);xc16=append(xc16,data$LONGITUDE[i])}
  for (i in c17){yc17=append(yc17,data$LATITUDE[i]);xc17=append(xc17,data$LONGITUDE[i])}
  for (i in c18){yc18=append(yc18,data$LATITUDE[i]);xc18=append(xc18,data$LONGITUDE[i])}
  for (i in c19){yc19=append(yc19,data$LATITUDE[i]);xc19=append(xc19,data$LONGITUDE[i])}
  for (i in c20){yc20=append(yc20,data$LATITUDE[i]);xc20=append(xc20,data$LONGITUDE[i])}
  dc1=as.data.frame(cbind(xc1,yc1))
  dc2=as.data.frame(cbind(xc2,yc2))
  dc3=as.data.frame(cbind(xc3,yc3)) 
  dc4=as.data.frame(cbind(xc4,yc4)) 
  dc5=as.data.frame(cbind(xc5,yc5))  
  dc6=as.data.frame(cbind(xc6,yc6))  
  dc7=as.data.frame(cbind(xc7,yc7))  
  dc8=as.data.frame(cbind(xc8,yc8))  
  dc9=as.data.frame(cbind(xc9,yc9))  
  dc10=as.data.frame(cbind(xc10,yc10))
  dc11=as.data.frame(cbind(xc11,yc11))
  dc12=as.data.frame(cbind(xc12,yc12))
  dc13=as.data.frame(cbind(xc13,yc13)) 
  dc14=as.data.frame(cbind(xc14,yc14)) 
  dc15=as.data.frame(cbind(xc15,yc15))  
  dc16=as.data.frame(cbind(xc16,yc16))  
  dc17=as.data.frame(cbind(xc17,yc17))  
  dc18=as.data.frame(cbind(xc18,yc18))  
  dc19=as.data.frame(cbind(xc19,yc19))  
  dc20=as.data.frame(cbind(xc20,yc20)) 
  m <-leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(dc1, lat = ~yc1,lng = ~xc1, radius = 2, stroke = FALSE,
                     fillColor = "red", fillOpacity = 1)%>%
    addCircleMarkers(dc2, lat = ~yc2,lng = ~xc2, radius = 2, stroke = FALSE,
                     fillColor = "green", fillOpacity = 1)%>%
    addCircleMarkers(dc3, lat = ~yc3,lng = ~xc3, radius = 2, stroke = FALSE,
                     fillColor = "blue", fillOpacity = 1)%>%
    addCircleMarkers(dc4, lat = ~yc4,lng = ~xc4, radius = 2, stroke = FALSE,
                     fillColor = "yellow", fillOpacity = 1)%>%
    addCircleMarkers(dc5, lat = ~yc5,lng = ~xc5, radius = 2, stroke = FALSE,
                     fillColor = "brown", fillOpacity = 1)%>%
    addCircleMarkers(dc6, lat = ~yc6,lng = ~xc6, radius = 2, stroke = FALSE,
                     fillColor = "lime", fillOpacity = 1)%>%
    addCircleMarkers(dc7, lat = ~yc7,lng = ~xc7, radius = 2, stroke = FALSE,
                     fillColor = "olive", fillOpacity = 1)%>%
    addCircleMarkers(dc8, lat = ~yc8,lng = ~xc8, radius = 2, stroke = FALSE,
                     fillColor = "orange", fillOpacity = 1)%>%
    addCircleMarkers(dc9, lat = ~yc9,lng = ~xc9, radius = 2, stroke = FALSE,
                     fillColor = "magenta", fillOpacity = 1)%>%
    addCircleMarkers(dc10, lat = ~yc10,lng = ~xc10, radius = 2, stroke = FALSE,
                     fillColor = "teal", fillOpacity = 1)%>%
    addCircleMarkers(dc11, lat = ~yc11,lng = ~xc11, radius = 2, stroke = FALSE,
                     fillColor = "darkgray", fillOpacity = 1)%>%
    addCircleMarkers(dc12, lat = ~yc12,lng = ~xc12, radius = 2, stroke = FALSE,
                     fillColor = "violet", fillOpacity = 1)%>%
    addCircleMarkers(dc13, lat = ~yc13,lng = ~xc13, radius = 2, stroke = FALSE,
                     fillColor = "cyan", fillOpacity = 1)%>%
    addCircleMarkers(dc14, lat = ~yc14,lng = ~xc14, radius = 2, stroke = FALSE,
                     fillColor = "pink", fillOpacity = 1)%>%
    addCircleMarkers(dc15, lat = ~yc15,lng = ~xc15, radius = 2, stroke = FALSE,
                     fillColor = "gray", fillOpacity = 1)%>%
    addCircleMarkers(dc16, lat = ~yc16,lng = ~xc16, radius = 2, stroke = FALSE,
                     fillColor = "purple", fillOpacity = 1)%>%
    addCircleMarkers(dc17, lat = ~yc17,lng = ~xc17, radius = 2, stroke = FALSE,
                     fillColor = "black", fillOpacity = 1)%>%
    addCircleMarkers(dc18, lat = ~yc18,lng = ~xc18, radius = 2, stroke = FALSE,
                     fillColor = "#742E98", fillOpacity = 1)%>%
    addCircleMarkers(dc19, lat = ~yc19,lng = ~xc19, radius = 2, stroke = FALSE,
                     fillColor = "#C1A32D", fillOpacity = 1)%>%
    addCircleMarkers(dc20, lat = ~yc20,lng = ~xc20, radius = 2, stroke = FALSE,
                     fillColor = "#3D3D3D", fillOpacity = 1)
  m 
  