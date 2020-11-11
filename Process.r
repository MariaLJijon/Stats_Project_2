library("jsonlite")
library("chron")
library("dlm")
library("sp")
library("rgdal")
library("dplyr")

#2020-08-18-Tuesday_1
#Get time, longitude, latitude
data1 <-read_json('20200818114606.geojson')

data1 <- (data1[2])$features

df1 <- data.frame(cbind("time" = data1[[1]]$properties$time,
                        "longitude" = data1[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data1[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data1)){
  ab <- data.frame(cbind("time" = data1[[i]]$properties$time,
                         "longitude" = data1[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data1[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df1 <- rbind(df1, ab)
}

df1<-head(df1, 29)

#Since longitude has to be a number we need to convert it 
df1$longitude <- as.numeric(df1$longitude)

#Make sure the points are in chronological order 
df1 <-df1[order(df1$time),] 
rownames(df1) <- 1:nrow(df1)

#Now transform time to chron
for(i in 1:nrow(df1)){
  a <-substr(df1$time[i], start=1, stop=10)
  b <-substr(df1$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df1$time[i] = c
}

#Need to get utm-coordinates
spat_df1 <- SpatialPointsDataFrame(coords=df1[, c("longitude", "latitude")],
                                  data=df1['time'],   # This needs to be a data frame
                                  proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df1 <- spTransform(spat_df1, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords1 <- coordinates(utm_df1)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d1<-as.data.frame(utm_coords1)
d1<-cbind(df1, d1)
names(d1)[4] <- "utm_longitude"
names(d1)[5] <- "utm_latitude"

#time differences between each point
dt1 <- c(as.numeric(difftime(d1$time[2], d1$time[1], units = "secs")))
for(i in 3:nrow(d1)){
  dt1 <- c(dt1, as.numeric(difftime(d1$time[i], d1$time[i-1], units = "secs")))
}

dt1<-c(dt1, NA)
d1<-cbind(d1, dt1)
names(d1)[6] <- "dt"

#time since start of journey
pth_time1 <-c(as.numeric(difftime(d1$time[1], d1$time[1], units = "secs")))

for(i in 2:nrow(d1)){
  pth_time1 <- c(pth_time1, as.numeric(difftime(d1$time[i], d1$time[1], units = "secs")))
}

d1<-cbind(d1, pth_time1)
names(d1)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d1$utm_longitude[2]-d1$utm_longitude[1])^2
b<-(d1$utm_latitude[2]-d1$utm_latitude[1])^2
dist1 <-c(sqrt(a+b))
for(i in 3:nrow(d1)){
    a<-(d1$utm_longitude[i]-d1$utm_longitude[i-1])^2
    b<-(d1$utm_latitude[i]-d1$utm_latitude[i-1])^2
    dist1<-c(dist1, sqrt(a+b))
}
dist1<-c(dist1, NA)

d1<-cbind(d1, dist1)
names(d1)[8] <- "dist"

#Speed between each point
vel_1<-c(d1$dist[1]/d1$dt[1])
for(i in 2:nrow(d1)){
    vel_1<-c(vel_1, d1$dist[i]/d1$dt[i])
}

d1<-cbind(d1, vel_1)
names(d1)[9] <- "speed"

for(i in 1:nrow(d1)){
  if(is.infinite(d1$speed[i])){
    d1$speed[i]<-NA 
  }
}

#Specify the Day (Tuesday)
day <- rep(c("Tuesday"),each=nrow(d1))
d1<- cbind(day, d1)
#And specify which one (First Tuesday)
num <- rep(c("Tuesday_1"),each=nrow(d1))
d1<- cbind(num, d1)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt1 <- median(d1$dt, na.rm = TRUE)
g_mat1 <- matrix(c(1, 0, dt1, 0,
                   0, 1, 0, dt1,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d1$speed, na.rm = TRUE)

dlm_spec1 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat1,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords1[1, ], rep(median_speed / dt1, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_1 <- dlmFilter(utm_coords1, dlm_spec1)
dlm_smooth_mod_1 <- dlmSmooth(dlm_filter_mod_1)

#Kalman filter
df_dlm_filt_1 <- data.frame(dlm_filter_mod_1$m)
df_dlm_filt_1 <- subset(df_dlm_filt_1, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_1 <- data.frame(dlm_smooth_mod_1$s)
df_dlm_smooth_1 <- subset(df_dlm_smooth_1, select = -c(X3, X4))

#2020-08-19-Wednesday_1
data3 <-read_json('20200819132607.geojson')

data3 <- (data3[2])$features

df3 <- data.frame(cbind("time" = data3[[1]]$properties$time,
                        "longitude" = data3[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data3[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data3)){
  ab <- data.frame(cbind("time" = data3[[i]]$properties$time,
                         "longitude" = data3[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data3[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df3 <- rbind(df3, ab)
}

df3<-head(df3, 43)

#Since longitude has to be a number we need to convert it 
df3$longitude <- as.numeric(df3$longitude)

#Make sure the points are in chronological order 
df3 <-df3[order(df3$time),] 
rownames(df3) <- 1:nrow(df3)

#Now transform time to chron
for(i in 1:nrow(df3)){
  a <-substr(df3$time[i], start=1, stop=10)
  b <-substr(df3$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df3$time[i] = c
}

#Need to get utm-coordinates
spat_df3 <- SpatialPointsDataFrame(coords=df3[, c("longitude", "latitude")],
                                   data=df3['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df3 <- spTransform(spat_df3, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords3 <- coordinates(utm_df3)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d3<-as.data.frame(utm_coords3)
d3<-cbind(df3, d3)
names(d3)[4] <- "utm_longitude"
names(d3)[5] <- "utm_latitude"

#time differences between each point
dt3 <- c(as.numeric(difftime(d3$time[2], d3$time[1], units = "secs")))
for(i in 3:nrow(d3)){
  dt3 <- c(dt3, as.numeric(difftime(d3$time[i], d3$time[i-1], units = "secs")))
}
dt3<-c(dt3, NA)
d3<-cbind(d3, dt3)
names(d3)[6] <- "dt"

#time since start of journey
pth_time3 <-c(as.numeric(difftime(d3$time[1], d3$time[1], units = "secs")))

for(i in 2:nrow(d3)){
  pth_time3 <- c(pth_time3, as.numeric(difftime(d3$time[i], d3$time[1], units = "secs")))
}

d3<-cbind(d3, pth_time3)
names(d3)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d3$utm_longitude[2]-d3$utm_longitude[1])^2
b<-(d3$utm_latitude[2]-d3$utm_latitude[1])^2
dist3 <-c(sqrt(a+b))
for(i in 3:nrow(d3)){
  a<-(d3$utm_longitude[i]-d3$utm_longitude[i-1])^2
  b<-(d3$utm_latitude[i]-d3$utm_latitude[i-1])^2
  dist3<-c(dist3, sqrt(a+b))
}
dist3<-c(dist3, NA)

d3<-cbind(d3, dist3)
names(d3)[8] <- "dist"

#Speed between each point
vel_3<-c(d3$dist[1]/d3$dt[1])
for(i in 2:nrow(d3)){
  vel_3<-c(vel_3, d3$dist[i]/d3$dt[i])
}

d3<-cbind(d3, vel_3)
names(d3)[9] <- "speed"

for(i in 1:nrow(d3)){
  if(is.infinite(d3$speed[i])){
    d3$speed[i]<-NA 
  }
}

#Specify the Day (Wednesday)
day <- rep(c("Wednesday"),each=nrow(d3))
d3<- cbind(day, d3)
#And specify which one (First Wednesday)
num <- rep(c("Wednesday_1"),each=nrow(d3))
d3<- cbind(num, d3)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt3 <- median(d3$dt, na.rm = TRUE)
g_mat3 <- matrix(c(1, 0, dt3, 0,
                   0, 1, 0, dt3,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d3$speed, na.rm = TRUE)

dlm_spec3 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat3,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords3[1, ], rep(median_speed / dt3, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_3 <- dlmFilter(utm_coords3, dlm_spec3)
dlm_smooth_mod_3 <- dlmSmooth(dlm_filter_mod_3)

#Kalman filter
df_dlm_filt_3 <- data.frame(dlm_filter_mod_3$m)
df_dlm_filt_3 <- subset(df_dlm_filt_3, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_3 <- data.frame(dlm_smooth_mod_3$s)
df_dlm_smooth_3 <- subset(df_dlm_smooth_3, select = -c(X3, X4))

#2020-08-20-Thursday_1
data5 <-read_json('20200820151044.geojson')

data5 <- (data5[2])$features

df5 <- data.frame(cbind("time" = data5[[1]]$properties$time,
                        "longitude" = data5[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data5[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data5)){
  ab <- data.frame(cbind("time" = data5[[i]]$properties$time,
                         "longitude" = data5[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data5[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df5 <- rbind(df5, ab)
}

df5<-head(df5, 85)

#Since longitude has to be a number we need to convert it 
df5$longitude <- as.numeric(df5$longitude)

#Make sure the points are in chronological order 
df5 <-df5[order(df5$time),] 
rownames(df5) <- 1:nrow(df5)

#Now transform time to chron
for(i in 1:nrow(df5)){
  a <-substr(df5$time[i], start=1, stop=10)
  b <-substr(df5$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df5$time[i] = c
}

#Need to get utm-coordinates
spat_df5 <- SpatialPointsDataFrame(coords=df5[, c("longitude", "latitude")],
                                   data=df5['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df5 <- spTransform(spat_df5, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords5 <- coordinates(utm_df5)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d5<-as.data.frame(utm_coords5)
d5<-cbind(df5, d5)
names(d5)[4] <- "utm_longitude"
names(d5)[5] <- "utm_latitude"

#time differences between each point
dt5 <- c(as.numeric(difftime(d5$time[2], d5$time[1], units = "secs")))
for(i in 3:nrow(d5)){
  dt5 <- c(dt5, as.numeric(difftime(d5$time[i], d5$time[i-1], units = "secs")))
}

dt5<-c(dt5, NA)
d5<-cbind(d5, dt5)
names(d5)[6] <- "dt"

#time since start of journey
pth_time5 <-c(as.numeric(difftime(d5$time[1], d5$time[1], units = "secs")))

for(i in 2:nrow(d5)){
  pth_time5 <- c(pth_time5, as.numeric(difftime(d5$time[i], d5$time[1], units = "secs")))
}

d5<-cbind(d5, pth_time5)
names(d5)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d5$utm_longitude[2]-d5$utm_longitude[1])^2
b<-(d5$utm_latitude[2]-d5$utm_latitude[1])^2
dist5 <-c(sqrt(a+b))
for(i in 3:nrow(d5)){
  a<-(d5$utm_longitude[i]-d5$utm_longitude[i-1])^2
  b<-(d5$utm_latitude[i]-d5$utm_latitude[i-1])^2
  dist5<-c(dist5, sqrt(a+b))
}
dist5<-c(dist5, NA)

d5<-cbind(d5, dist5)
names(d5)[8] <- "dist"

#Speed between each point
vel_5<-c(d5$dist[1]/d5$dt[1])
for(i in 2:nrow(d5)){
  vel_5<-c(vel_5, d5$dist[i]/d5$dt[i])
}

d5<-cbind(d5, vel_5)
names(d5)[9] <- "speed"

for(i in 1:nrow(d5)){
    if(is.infinite(d5$speed[i])){
       d5$speed[i]<-NA 
    }
}

#Specify the Day (Thursday)
day <- rep(c("Thursday"),each=nrow(d5))
d5<- cbind(day, d5)
#And specify which one (First Thursday)
num <- rep(c("Thursday_1"),each=nrow(d5))
d5<- cbind(num, d5)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt5 <- median(d5$dt, na.rm = TRUE)
g_mat5 <- matrix(c(1, 0, dt5, 0,
                   0, 1, 0, dt5,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d5$speed, na.rm = TRUE)

dlm_spec5 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat5,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords5[1, ], rep(median_speed / dt5, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_5 <- dlmFilter(utm_coords5, dlm_spec5)
dlm_smooth_mod_5 <- dlmSmooth(dlm_filter_mod_5)

#Kalman filter
df_dlm_filt_5 <- data.frame(dlm_filter_mod_5$m)
df_dlm_filt_5 <- subset(df_dlm_filt_5, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_5 <- data.frame(dlm_smooth_mod_5$s)
df_dlm_smooth_5 <- subset(df_dlm_smooth_5, select = -c(X3, X4))

#2020-08-24-Monday _1
data9 <-read_json('20200824130857.geojson')

data9 <- (data9[2])$features

df9 <- data.frame(cbind("time" = data9[[1]]$properties$time,
                        "longitude" = data9[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data9[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data9)){
  ab <- data.frame(cbind("time" = data9[[i]]$properties$time,
                         "longitude" = data9[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data9[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df9 <- rbind(df9, ab)
}

df9<-head(df9,144)

#Since longitude has to be a number we need to convert it 
df9$longitude <- as.numeric(df9$longitude)

#Make sure the points are in chronological order 
df9 <-df9[order(df9$time),] 
rownames(df9) <- 1:nrow(df9)

#Now transform time to chron
for(i in 1:nrow(df9)){
  a <-substr(df9$time[i], start=1, stop=10)
  b <-substr(df9$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df9$time[i] = c
}

#Need to get utm-coordinates
spat_df9 <- SpatialPointsDataFrame(coords=df9[, c("longitude", "latitude")],
                                   data=df9['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df9 <- spTransform(spat_df9, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords9 <- coordinates(utm_df9)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d9<-as.data.frame(utm_coords9)
d9<-cbind(df9, d9)
names(d9)[4] <- "utm_longitude"
names(d9)[5] <- "utm_latitude"

#time differences between each point
dt9 <- c(as.numeric(difftime(d9$time[2], d9$time[1], units = "secs")))
for(i in 3:nrow(d9)){
  dt9 <- c(dt9, as.numeric(difftime(d9$time[i], d9$time[i-1], units = "secs")))
}

dt9<-c(dt9, NA)
d9<-cbind(d9, dt9)
names(d9)[6] <- "dt"

#time since start of journey
pth_time9 <-c(as.numeric(difftime(d9$time[1], d9$time[1], units = "secs")))

for(i in 2:nrow(d9)){
  pth_time9 <- c(pth_time9, as.numeric(difftime(d9$time[i], d9$time[1], units = "secs")))
}

d9<-cbind(d9, pth_time9)
names(d9)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d9$utm_longitude[2]-d9$utm_longitude[1])^2
b<-(d9$utm_latitude[2]-d9$utm_latitude[1])^2
dist9 <-c(sqrt(a+b))
for(i in 3:nrow(d9)){
  a<-(d9$utm_longitude[i]-d9$utm_longitude[i-1])^2
  b<-(d9$utm_latitude[i]-d9$utm_latitude[i-1])^2
  dist9<-c(dist9, sqrt(a+b))
}
dist9<-c(dist9, NA)

d9<-cbind(d9, dist9)
names(d9)[8] <- "dist"

#Speed between each point
vel_9<-c(d9$dist[1]/d9$dt[1])
for(i in 2:nrow(d9)){
  vel_9<-c(vel_9, d9$dist[i]/d9$dt[i])
}

d9<-cbind(d9, vel_9)
names(d9)[9] <- "speed"

for(i in 1:nrow(d9)){
  if(is.infinite(d9$speed[i])){
    d9$speed[i]<-NA 
  }
}

#Specify the Day (Monday)
day <- rep(c("Monday"),each=nrow(d9))
d9<- cbind(day, d9)
#And specify which one (First Monday)
num <- rep(c("Monday_1"),each=nrow(d9))
d9<- cbind(num, d9)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt9 <- median(d9$dt, na.rm = TRUE)
g_mat9 <- matrix(c(1, 0, dt9, 0,
                   0, 1, 0, dt9,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d9$speed, na.rm = TRUE)

dlm_spec9 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat9,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords9[1, ], rep(median_speed / dt9, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_9 <- dlmFilter(utm_coords9, dlm_spec9)
dlm_smooth_mod_9 <- dlmSmooth(dlm_filter_mod_9)

#Kalman filter
df_dlm_filt_9 <- data.frame(dlm_filter_mod_9$m)
df_dlm_filt_9 <- subset(df_dlm_filt_9, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_9 <- data.frame(dlm_smooth_mod_9$s)
df_dlm_smooth_9 <- subset(df_dlm_smooth_9, select = -c(X3, X4))

#2020-08-25-Tuesday_2
data2 <-read_json('20200825121346.geojson')

data2 <- (data2[2])$features

df2 <- data.frame(cbind("time" = data2[[1]]$properties$time,
                        "longitude" = data2[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data2[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data2)){
  ab <- data.frame(cbind("time" = data2[[i]]$properties$time,
                         "longitude" = data2[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data2[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df2 <- rbind(df2, ab)
}

df2<-head(df2, 127)

#Since longitude has to be a number we need to convert it 
df2$longitude <- as.numeric(df2$longitude)

#Make sure the points are in chronological order 
df2 <-df2[order(df2$time),] 
rownames(df2) <- 1:nrow(df2)

#Now transform time to chron
for(i in 1:nrow(df2)){
  a <-substr(df2$time[i], start=1, stop=10)
  b <-substr(df2$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df2$time[i] = c
}

#Need to get utm-coordinates
spat_df2 <- SpatialPointsDataFrame(coords=df2[, c("longitude", "latitude")],
                                  data=df2['time'],   # This needs to be a data frame
                                  proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df2 <- spTransform(spat_df2, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords2 <- coordinates(utm_df2)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d2<-as.data.frame(utm_coords2)
d2<-cbind(df2, d2)
names(d2)[4] <- "utm_longitude"
names(d2)[5] <- "utm_latitude"

#time differences between each point
dt2 <- c(as.numeric(difftime(d2$time[2], d2$time[1], units = "secs")))
for(i in 3:nrow(d2)){
  dt2 <- c(dt2, as.numeric(difftime(d2$time[i], d2$time[i-1], units = "secs")))
}

dt2<-c(dt2, NA)
d2<-cbind(d2, dt2)
names(d2)[6] <- "dt"

#time since start of journey
pth_time2 <-c(as.numeric(difftime(d2$time[1], d2$time[1], units = "secs")))

for(i in 2:nrow(d2)){
  pth_time2 <- c(pth_time2, as.numeric(difftime(d2$time[i], d2$time[1], units = "secs")))
}

d2<-cbind(d2, pth_time2)
names(d2)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d2$utm_longitude[2]-d2$utm_longitude[1])^2
b<-(d2$utm_latitude[2]-d2$utm_latitude[1])^2
dist2 <-c(sqrt(a+b))
for(i in 3:nrow(d2)){
    a<-(d2$utm_longitude[i]-d2$utm_longitude[i-1])^2
    b<-(d2$utm_latitude[i]-d2$utm_latitude[i-1])^2
    dist2<-c(dist2, sqrt(a+b))
}
dist2<-c(dist2, NA)

d2<-cbind(d2, dist2)
names(d2)[8] <- "dist"

#Speed between each point
vel_2<-c(d2$dist[1]/d2$dt[1])
for(i in 2:nrow(d2)){
    vel_2<-c(vel_2, d2$dist[i]/d2$dt[i])
}

d2<-cbind(d2, vel_2)
names(d2)[9] <- "speed"

for(i in 1:nrow(d2)){
  if(is.infinite(d2$speed[i])){
    d2$speed[i]<-NA 
  }
}

#Specify the Day (Tuesday)
day <- rep(c("Tuesday"),each=nrow(d2))
d2<- cbind(day, d2)
#And specify which one (Second Tuesday)
num <- rep(c("Tuesday_2"),each=nrow(d2))
d2<- cbind(num, d2)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt2 <- median(d2$dt, na.rm = TRUE)
g_mat2 <- matrix(c(1, 0, dt2, 0,
                   0, 1, 0, dt2,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d2$speed, na.rm = TRUE)


dlm_spec2 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat2,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords2[1, ], rep(median_speed / dt2, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_2 <- dlmFilter(utm_coords2, dlm_spec2)
dlm_smooth_mod_2 <- dlmSmooth(dlm_filter_mod_2)

#Kalman filter
df_dlm_filt_2 <- data.frame(dlm_filter_mod_2$m)
df_dlm_filt_2 <- subset(df_dlm_filt_2, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_2 <- data.frame(dlm_smooth_mod_2$s)
df_dlm_smooth_2 <- subset(df_dlm_smooth_2, select = -c(X3, X4))

#2020-08-26-Wednesday_2
data4 <-read_json('20200826131614.geojson')

data4 <- (data4[2])$features

df4 <- data.frame(cbind("time" = data4[[1]]$properties$time,
                        "longitude" = data4[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data4[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data4)){
  ab <- data.frame(cbind("time" = data4[[i]]$properties$time,
                         "longitude" = data4[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data4[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df4 <- rbind(df4, ab)
}

df4<-head(df4, 191)

#Since longitude has to be a number we need to convert it 
df4$longitude <- as.numeric(df4$longitude)

#Make sure the points are in chronological order 
df4 <-df4[order(df4$time),] 
rownames(df4) <- 1:nrow(df4)

#Now transform time to chron
for(i in 1:nrow(df4)){
  a <-substr(df4$time[i], start=1, stop=10)
  b <-substr(df4$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df4$time[i] = c
}

#Need to get utm-coordinates
spat_df4 <- SpatialPointsDataFrame(coords=df4[, c("longitude", "latitude")],
                                   data=df4['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df4 <- spTransform(spat_df4, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords4 <- coordinates(utm_df4)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d4<-as.data.frame(utm_coords4)
d4<-cbind(df4, d4)
names(d4)[4] <- "utm_longitude"
names(d4)[5] <- "utm_latitude"

#time differences between each point
dt4 <- c(as.numeric(difftime(d4$time[2], d4$time[1], units = "secs")))
for(i in 3:nrow(d4)){
  dt4 <- c(dt4, as.numeric(difftime(d4$time[i], d4$time[i-1], units = "secs")))
}

dt4<-c(dt4, NA)
d4<-cbind(d4, dt4)
names(d4)[6] <- "dt"

#time since start of journey
pth_time4 <-c(as.numeric(difftime(d4$time[1], d4$time[1], units = "secs")))

for(i in 2:nrow(d4)){
  pth_time4 <- c(pth_time4, as.numeric(difftime(d4$time[i], d4$time[1], units = "secs")))
}

d4<-cbind(d4, pth_time4)
names(d4)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d4$utm_longitude[2]-d4$utm_longitude[1])^2
b<-(d4$utm_latitude[2]-d4$utm_latitude[1])^2
dist4 <-c(sqrt(a+b))
for(i in 3:nrow(d4)){
  a<-(d4$utm_longitude[i]-d4$utm_longitude[i-1])^2
  b<-(d4$utm_latitude[i]-d4$utm_latitude[i-1])^2
  dist4<-c(dist4, sqrt(a+b))
}
dist4<-c(dist4, NA)

d4<-cbind(d4, dist4)
names(d4)[8] <- "dist"

#Speed between each point
vel_4<-c(d4$dist[1]/d4$dt[1])
for(i in 2:nrow(d4)){
  vel_4<-c(vel_4, d4$dist[i]/d4$dt[i])
}

d4<-cbind(d4, vel_4)
names(d4)[9] <- "speed"

for(i in 1:nrow(d4)){
  if(is.infinite(d4$speed[i])){
    d4$speed[i]<-NA 
  }
}

#Specify the Day (Wednesday)
day <- rep(c("Wednesday"),each=nrow(d4))
d4<- cbind(day, d4)
#And specify which one (Second Wednesday)
num <- rep(c("Wednesday_2"),each=nrow(d4))
d4<- cbind(num, d4)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt4 <- median(d4$dt, na.rm = TRUE)
g_mat4 <- matrix(c(1, 0, dt4, 0,
                   0, 1, 0, dt4,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d4$speed, na.rm = TRUE)


dlm_spec4 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat4,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords4[1, ], rep(median_speed / dt4, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_4 <- dlmFilter(utm_coords4, dlm_spec4)
dlm_smooth_mod_4 <- dlmSmooth(dlm_filter_mod_4)

#Kalman filter
df_dlm_filt_4 <- data.frame(dlm_filter_mod_4$m)
df_dlm_filt_4 <- subset(df_dlm_filt_4, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_4 <- data.frame(dlm_smooth_mod_4$s)
df_dlm_smooth_4 <- subset(df_dlm_smooth_4, select = -c(X3, X4))

#2020-08-27-Thursday_2
data6 <-read_json('20200827113234.geojson')

data6 <- (data6[2])$features

df6 <- data.frame(cbind("time" = data6[[1]]$properties$time,
                        "longitude" = data6[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data6[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data6)){
  ab <- data.frame(cbind("time" = data6[[i]]$properties$time,
                         "longitude" = data6[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data6[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df6 <- rbind(df6, ab)
}

df6<-head(df6, 116)

#Since longitude has to be a number we need to convert it 
df6$longitude <- as.numeric(df6$longitude)

#Make sure the points are in chronological order 
df6 <-df6[order(df6$time),] 
rownames(df6) <- 1:nrow(df6)

#Now transform time to chron
for(i in 1:nrow(df6)){
  a <-substr(df6$time[i], start=1, stop=10)
  b <-substr(df6$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df6$time[i] = c
}

#Need to get utm-coordinates
spat_df6 <- SpatialPointsDataFrame(coords=df6[, c("longitude", "latitude")],
                                   data=df6['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df6 <- spTransform(spat_df6, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords6 <- coordinates(utm_df6)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d6<-as.data.frame(utm_coords6)
d6<-cbind(df6, d6)
names(d6)[4] <- "utm_longitude"
names(d6)[5] <- "utm_latitude"

#time differences between each point
dt6 <- c(as.numeric(difftime(d6$time[2], d6$time[1], units = "secs")))
for(i in 3:nrow(d6)){
  dt6 <- c(dt6, as.numeric(difftime(d6$time[i], d6$time[i-1], units = "secs")))
}

dt6<-c(dt6, NA)
d6<-cbind(d6, dt6)
names(d6)[6] <- "dt"

#time since start of journey
pth_time6 <-c(as.numeric(difftime(d6$time[1], d6$time[1], units = "secs")))

for(i in 2:nrow(d6)){
  pth_time6 <- c(pth_time6, as.numeric(difftime(d6$time[i], d6$time[1], units = "secs")))
}

d6<-cbind(d6, pth_time6)
names(d6)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d6$utm_longitude[2]-d6$utm_longitude[1])^2
b<-(d6$utm_latitude[2]-d6$utm_latitude[1])^2
dist6 <-c(sqrt(a+b))
for(i in 3:nrow(d6)){
  a<-(d6$utm_longitude[i]-d6$utm_longitude[i-1])^2
  b<-(d6$utm_latitude[i]-d6$utm_latitude[i-1])^2
  dist6<-c(dist6, sqrt(a+b))
}
dist6<-c(dist6, NA)

d6<-cbind(d6, dist6)
names(d6)[8] <- "dist"

#Speed between each point
vel_6<-c(d6$dist[1]/d6$dt[1])
for(i in 2:nrow(d6)){
  vel_6<-c(vel_6, d6$dist[i]/d6$dt[i])
}

d6<-cbind(d6, vel_6)
names(d6)[9] <- "speed"

for(i in 1:nrow(d6)){
  if(is.infinite(d6$speed[i])){
    d6$speed[i]<-NA 
  }
}

#Specify the Day (Thursday)
day <- rep(c("Thursday"),each=nrow(d6))
d6<- cbind(day, d6)
#And specify which one (Second Thursday)
num <- rep(c("Thursday_2"),each=nrow(d6))
d6<- cbind(num, d6)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt6 <- median(d6$dt, na.rm = TRUE)
g_mat6 <- matrix(c(1, 0, dt6, 0,
                   0, 1, 0, dt6,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d6$speed, na.rm = TRUE)


dlm_spec6 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat6,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords6[1, ], rep(median_speed / dt6, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_6 <- dlmFilter(utm_coords6, dlm_spec6)
dlm_smooth_mod_6 <- dlmSmooth(dlm_filter_mod_6)

#Kalman filter
df_dlm_filt_6 <- data.frame(dlm_filter_mod_6$m)
df_dlm_filt_6 <- subset(df_dlm_filt_6, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_6 <- data.frame(dlm_smooth_mod_6$s)
df_dlm_smooth_6 <- subset(df_dlm_smooth_6, select = -c(X3, X4))

#2020-08-28-Friday_2
#There are two databases for this date
#So merge the two into df8

#Read first database for Friday_2
data8_1 <-read_json('20200828122627.geojson')

data8_1 <- (data8_1[2])$features

df8_1 <- data.frame(cbind("time" = data8_1[[1]]$properties$time,
                        "longitude" = data8_1[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data8_1[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data8_1)){
  ab <- data.frame(cbind("time" = data8_1[[i]]$properties$time,
                         "longitude" = data8_1[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data8_1[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df8_1 <- rbind(df8_1, ab)
}

#Read second database for Friday_2
data8_2 <-read_json('20200828130816.geojson')

data8_2 <- (data8_2[2])$features

df8_2 <- data.frame(cbind("time" = data8_2[[1]]$properties$time,
                         "longitude" = data8_2[[1]]$geometry$coordinates[[1]]),
                   "latitude" = data8_2[[1]]$geometry$coordinates[[2]], 
                   stringsAsFactors=FALSE)

for(i in 2:length(data8_2)){
  ab <- data.frame(cbind("time" = data8_2[[i]]$properties$time,
                         "longitude" = data8_2[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data8_2[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df8_2 <- rbind(df8_2, ab)
}

#Merge the two into a single database for Friday_2
df8 <- rbind(df8_1, df8_2)

df8<-head(df8, 139)

#Since longitude has to be a number we need to convert it 
df8$longitude <- as.numeric(df8$longitude)

#Make sure the points are in chronological order 
df8 <-df8[order(df8$time),] 
rownames(df8) <- 1:nrow(df8)

#Now transform time to chron
for(i in 1:nrow(df8)){
  a <-substr(df8$time[i], start=1, stop=10)
  b <-substr(df8$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df8$time[i] = c
}

#Need to get utm-coordinates
spat_df8 <- SpatialPointsDataFrame(coords=df8[, c("longitude", "latitude")],
                                   data=df8['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df8 <- spTransform(spat_df8, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords8 <- coordinates(utm_df8)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d8<-as.data.frame(utm_coords8)
d8<-cbind(df8, d8)
names(d8)[4] <- "utm_longitude"
names(d8)[5] <- "utm_latitude"

#time differences between each point
dt8 <- c(as.numeric(difftime(d8$time[2], d8$time[1], units = "secs")))
for(i in 3:nrow(d8)){
  dt8 <- c(dt8, as.numeric(difftime(d8$time[i], d8$time[i-1], units = "secs")))
}

dt8<-c(dt8, NA)
d8<-cbind(d8, dt8)
names(d8)[6] <- "dt"

#time since start of journey
pth_time8 <-c(as.numeric(difftime(d8$time[1], d8$time[1], units = "secs")))

for(i in 2:nrow(d8)){
  pth_time8 <- c(pth_time8, as.numeric(difftime(d8$time[i], d8$time[1], units = "secs")))
}

d8<-cbind(d8, pth_time8)
names(d8)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d8$utm_longitude[2]-d8$utm_longitude[1])^2
b<-(d8$utm_latitude[2]-d8$utm_latitude[1])^2
dist8 <-c(sqrt(a+b))
for(i in 3:nrow(d8)){
  a<-(d8$utm_longitude[i]-d8$utm_longitude[i-1])^2
  b<-(d8$utm_latitude[i]-d8$utm_latitude[i-1])^2
  dist8<-c(dist8, sqrt(a+b))
}
dist8<-c(dist8, NA)

d8<-cbind(d8, dist8)
names(d8)[8] <- "dist"

#Speed between each point
vel_8<-c(d8$dist[1]/d8$dt[1])
for(i in 2:nrow(d8)){
  vel_8<-c(vel_8, d8$dist[i]/d8$dt[i])
}

d8<-cbind(d8, vel_8)
names(d8)[9] <- "speed"

for(i in 1:nrow(d8)){
  if(is.infinite(d8$speed[i])){
    d8$speed[i]<-NA 
  }
}

#Specify the Day (Friday)
day <- rep(c("Friday"),each=nrow(d8))
d8<- cbind(day, d8)
#And specify which one (Second Friday)
num <- rep(c("Friday_2"),each=nrow(d8))
d8<- cbind(num, d8)

#For Kalman Filter & Smoother
gps_variance <- 20^2
v_mat <- diag(c(gps_variance, gps_variance))
# I will use the median time = 8 as dt
dt8 <- median(d8$dt, na.rm = TRUE)
g_mat8 <- matrix(c(1, 0, dt8, 0,
                   0, 1, 0, dt8,
                   0, 0, 1, 0,
                   0, 0, 0, 1), byrow=TRUE, ncol=4)
median_speed<-median(d8$speed, na.rm = TRUE)


dlm_spec8 <- dlm(
  FF= matrix(c(1,0,0,0,0,1,0,0), byrow=TRUE, ncol=4),
  GG= g_mat8,
  V = v_mat,
  W = diag(c(5, 5, 1, 1)^2),
  m0 = matrix(c(utm_coords8[1, ], rep(median_speed / dt8, 2)),
              ncol=1), 
  C0 = diag(rep(10^2, 4)))

dlm_filter_mod_8 <- dlmFilter(utm_coords8, dlm_spec8)
dlm_smooth_mod_8 <- dlmSmooth(dlm_filter_mod_8)

#Kalman filter
df_dlm_filt_8 <- data.frame(dlm_filter_mod_8$m)
df_dlm_filt_8 <- subset(df_dlm_filt_8, select = -c(X3, X4))
#Kalman Smoother
df_dlm_smooth_8 <- data.frame(dlm_smooth_mod_8$s)
df_dlm_smooth_8 <- subset(df_dlm_smooth_8, select = -c(X3, X4))

#For point after bridge
utm_longitude<-c(271689)
utm_latitude<-c(5195836)
point<-as.data.frame(cbind(utm_longitude, utm_latitude))

utms <- SpatialPoints(point[, c("utm_longitude", "utm_latitude")], proj4string=CRS("+proj=utm +zone=12")) #create UTM matrix
longlats <- spTransform(utms, CRS("+proj=longlat")) #transform
longlats<-coordinates(longlats)

longlats<-as.data.frame(longlats)
names(longlats)[1]<-"longitude"
names(longlats)[2]<-"latitude"
longlats<-cbind(longlats, point)

longlats

#The Northside Pedestrian Bridge
longitude<- c(-113.996315)
latitude<- c(46.878589)
time<-c(2)
#Transfrom to utm_coordinates
bridge<-cbind(longitude, latitude)
bridge<-cbind(bridge, time)
bridge<-as.data.frame(bridge)

#Need to get utm-coordinates
spat_df <- SpatialPointsDataFrame(coords=bridge[, c("longitude", "latitude")],
                                  data=bridge['time'],   # This needs to be a data frame
                                  proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df <- spTransform(spat_df, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords <- coordinates(utm_df)


bridge<-subset(bridge, select = -c(time))
bridge<-cbind(bridge, utm_coords)
names(bridge)[3]<-"utm_longitude"
names(bridge)[4]<-"utm_latitude"
bridge

longlats<-rbind(bridge, longlats)

#SkyRoom
#744 E Broadway St, Missoula, MT 59802, United States
#46.878589, -113.996315

#Plot all smoothers together

#Tuesday_1 is red
plot(df_dlm_smooth_1, col="red")
par(new=TRUE)

#Tuesday_2 is pink
plot(df_dlm_smooth_2, col="pink", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Wednesday_1 is blue 
plot(df_dlm_smooth_3, col="blue", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Wednesday_2 is lightblue 
plot(df_dlm_smooth_4, col="lightblue", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Thursday_1 is dark green
plot(df_dlm_smooth_5, col="darkgreen", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Thursday_2 is  green
plot(df_dlm_smooth_6, col="green", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Friday_2 is orange
plot(df_dlm_smooth_8, col="orange", axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Monday_1 is purple
plot(df_dlm_smooth_9, col="purple", axes = FALSE, xlab="", ylab="")


#Plot all smoothers with points 

#Tuesdays 
plot(df_dlm_smooth_1)
par(new=TRUE)
plot(df_dlm_smooth_2, axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Wednesdays 
plot(df_dlm_smooth_3, axes = FALSE, xlab="", ylab="")
par(new=TRUE)
plot(df_dlm_smooth_4, axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Thursdays 
plot(df_dlm_smooth_5, axes = FALSE, xlab="", ylab="")
par(new=TRUE)
plot(df_dlm_smooth_6, axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Friday_2 
plot(df_dlm_smooth_8, axes = FALSE, xlab="", ylab="")
par(new=TRUE)

#Monday_1 
plot(df_dlm_smooth_9, axes = FALSE, xlab="", ylab="")

#First point (5.5 mins after leaving home)
#----Will be found later on

#Second point (location we know he visits)
abline(h=longlats$utm_latitude[2], col="red")
abline(v=longlats$utm_longitude[2], col="red")

#Third location(practical, Northside Pedestrian bridge )
abline(h=longlats$utm_latitude[1], col="blue")
abline(v=longlats$utm_longitude[1], col="blue")

longlats

#Join all data from the two weeks 
#except for the first Friday and second Monday

df<-rbind(d1, d3, d5, d9, d2, d4, d6, d8)
dim(df)

#Path time vs Longitude
plot(df$utm_longitude, df$pth_time, xlab="Path_Time", ylab="Longitude")

#Path time vs Latitude
plot(df$pth_time, df$utm_latitude, xlab="Path_Time", ylab="Latitude")

#The relationship between Path Time and Latitude appears to be linear. 
#But it diverges as path_time passes
#The relationship between Path Time and Longitude is not linear, it almost grows expontentially

#So, what I want the time model to be
#predicted time path --> t
#lon-->longitude - mean longitude at origin
#lon2--> lon^2
#lat-->latitude - mean latitude at origin

#t= a0 + a1*lon  + a2*lon2 + a3*lat

#Mean longitute and latitude at origin from df
long<-c(df$longitude[1])
lat<-c(df$latitude[1])

for(i in 2:nrow(df)){
    if(df$pth_time[i]==0){
        long<-c(long, df$longitude[i])
        lat<-c(lat, df$latitude[i])
    }
}
mn_long<-mean(long)
mn_lat<-mean(lat)

#Histogram, path time
hist(df$pth_time)

# OLS model
y<-df$pth_time

lon<-df$longitude-mn_long
lat<-df$latitude-mn_lat

ols <- lm(y ~ lon + lat)
ols

#I tried to use a log transformation of pth_time (before)
#However, this did not improve the movel
#Same thing with square values of longitude and latitude

summary(ols)

longlats

plot(ols)

# Save the predicted values and residuals
df$predicted <- predict(ols)   
df$residuals <- residuals(ols)

plot(df$longitude, df$residuals, xlab="Longitude", ylab="Residuals")

plot(df$latitude, df$residuals, xlab="Latitude", ylab="Residuals")

plot(df$pth_time, df$residuals, xlab="Path Time", ylab="Residuals")

#Need to pick a point closest to Wayne's departure both in time in space
#At the same time it has to be a point which he passes by
#This is why I narrowed the analysis to the path at the begining of the journey
#I have to combine two things
#First that there is a high probabiity that Wayne passes that point
#And second that it is as soon as possible after five mins from his departure
#there are many suitible candidates before the 5 mins but these wont work in the model
#after the 5 mins, Wayne varies his path
#So I have chosen the first two points that I'm confident that Wayne will pass by
#This does not mean that the GPS will measur them accurately
#That is a source of possible error

longlats
#12.35    -10936.33   -106568.46

lon<-longlats$longitude[1]-mn_long
#lon2<-(longlats$longitude[3]-mn_long)^2
lat<-longlats$latitude[1]-mn_lat

#For the third point
pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
pred_pth_time
pred_pth_time/60
888+120
888+140

#Validation
#First Friday
#2020-08-21-Friday 
data7 <-read_json('20200821111447.geojson')

data7 <- (data7[2])$features

df7 <- data.frame(cbind("time" = data7[[1]]$properties$time,
                        "longitude" = data7[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data7[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data7)){
  ab <- data.frame(cbind("time" = data7[[i]]$properties$time,
                         "longitude" = data7[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data7[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df7 <- rbind(df7, ab)
}

df7<-head(df7, 106)

#Since longitude has to be a number we need to convert it 
df7$longitude <- as.numeric(df7$longitude)

#Make sure the points are in chronological order 
df7 <-df7[order(df7$time),] 
rownames(df7) <- 1:nrow(df7)

#Now transform time to chron
for(i in 1:nrow(df7)){
  a <-substr(df7$time[i], start=1, stop=10)
  b <-substr(df7$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df7$time[i] = c
}

#Need to get utm-coordinates
spat_df7 <- SpatialPointsDataFrame(coords=df7[, c("longitude", "latitude")],
                                   data=df7['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df7 <- spTransform(spat_df7, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords7 <- coordinates(utm_df7)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d7<-as.data.frame(utm_coords7)
d7<-cbind(df7, d7)
names(d7)[4] <- "utm_longitude"
names(d7)[5] <- "utm_latitude"

#time differences between each point
dt7 <- c(as.numeric(difftime(d7$time[2], d7$time[1], units = "secs")))
for(i in 3:nrow(d7)){
  dt7 <- c(dt7, as.numeric(difftime(d7$time[i], d7$time[i-1], units = "secs")))
}

dt7<-c(dt7, NA)
d7<-cbind(d7, dt7)
names(d7)[6] <- "dt"

#time since start of journey
pth_time7 <-c(as.numeric(difftime(d7$time[1], d7$time[1], units = "secs")))

for(i in 2:nrow(d7)){
  pth_time7 <- c(pth_time7, as.numeric(difftime(d7$time[i], d7$time[1], units = "secs")))
}

d7<-cbind(d7, pth_time7)
names(d7)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d7$utm_longitude[2]-d7$utm_longitude[1])^2
b<-(d7$utm_latitude[2]-d7$utm_latitude[1])^2
dist7 <-c(sqrt(a+b))
for(i in 3:nrow(d7)){
  a<-(d7$utm_longitude[i]-d7$utm_longitude[i-1])^2
  b<-(d7$utm_latitude[i]-d7$utm_latitude[i-1])^2
  dist7<-c(dist7, sqrt(a+b))
}
dist7<-c(dist7, NA)

d7<-cbind(d7, dist7)
names(d7)[8] <- "dist"

#Speed between each point
vel_7<-c(d7$dist[1]/d7$dt[1])
for(i in 2:nrow(d7)){
  vel_7<-c(vel_7, d7$dist[i]/d7$dt[i])
}

d7<-cbind(d7, vel_7)
names(d7)[9] <- "speed"

for(i in 1:nrow(d7)){
  if(is.infinite(d7$speed[i])){
    d7$speed[i]<-NA 
  }
}

#Specify the Day (Friday)
day <- rep(c("Friday"),each=nrow(d7))
d7<- cbind(day, d7)
#And specify which one (First Friday)
num <- rep(c("Friday_1"),each=nrow(d7))
d7<- cbind(num, d7)

#Find least distance with the bridge point

a<-(d7$utm_longitude[1]-longlats$utm_long[1])^2
b<-(d7$utm_latitude[1]-longlats$utm_lat[1])^2
dist<-c(sqrt(a+b))

for(i in 2:nrow(d7)){
    a<-(d7$utm_longitude[i]-longlats$utm_long[1])^2
    b<-(d7$utm_latitude[i]-longlats$utm_lat[1])^2
    dist<-c(dist, sqrt(a+b))
}

min(dist, na.rm=TRUE)
for (i in 1:length(dist)){
    if(dist[i]==min(dist, na.rm=TRUE)){
        print(i)
    }
}

#Actual path time at that point
d7$pth_time[94]
d7$pth_time[94]/60

lon<-d7$longitude[94]-d7$longitude[1]
lat<-d7$latitude[94]-d7$latitude[1]

#For the third point
pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
pred_pth_time
pred_pth_time/60

860-852.764378366189

#The (first) point is 9.17 meters from Wayne's position --> BRIDGE
#the predicted path time at this point is 852.76 seconds (14.21 minutes)
#The real path time is 860 seconds (14.33 minutes)
#There is a 7.24 second difference 

#Test with second point (after bridge)

a<-(d7$utm_longitude[1]-longlats$utm_long[2])^2
b<-(d7$utm_latitude[1]-longlats$utm_lat[2])^2
dist<-c(sqrt(a+b))

for(i in 2:nrow(d7)){
    a<-(d7$utm_longitude[i]-longlats$utm_long[2])^2
    b<-(d7$utm_latitude[i]-longlats$utm_lat[2])^2
    dist<-c(dist, sqrt(a+b))
}

min(dist, na.rm=TRUE)
for (i in 1:length(dist)){
    if(dist[i]==min(dist, na.rm=TRUE)){
        print(i)
    }
}

#Actual path time at that point
d7$pth_time[106]
d7$pth_time[106]/60

lon<-d7$longitude[106]-d7$longitude[1]
lat<-d7$latitude[106]-d7$latitude[1]

#For the third point
pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
pred_pth_time
pred_pth_time/60

1038-1030.66491340146

#The (second) point is 12.14 meters from Wayne's position --> after crossing bridge
#the predicted path time at this point is 1030.66 seconds (17.18 minutes)
#The real path time is 1038 seconds (17.3 minutes)
#There is a 7.33 second difference 

#Validation
#Second Monday 
#2020-08-31-Monday_2
data10 <-read_json('20200831115147.geojson')

data10 <- (data10[2])$features

df10 <- data.frame(cbind("time" = data10[[1]]$properties$time,
                        "longitude" = data10[[1]]$geometry$coordinates[[1]]),
                  "latitude" = data10[[1]]$geometry$coordinates[[2]], 
                  stringsAsFactors=FALSE)

for(i in 2:length(data10)){
  ab <- data.frame(cbind("time" = data10[[i]]$properties$time,
                         "longitude" = data10[[i]]$geometry$coordinates[[1]]),
                   "latitude" = data10[[i]]$geometry$coordinates[[2]],
                   stringsAsFactors=FALSE)
  
  df10 <- rbind(df10, ab)
}

df10<-head(df10, 114)

#Since longitude has to be a number we need to convert it 
df10$longitude <- as.numeric(df10$longitude)

#Make sure the points are in chronological order 
df10 <-df10[order(df10$time),] 
rownames(df10) <- 1:nrow(df10)

#Now transform time to chron
for(i in 1:nrow(df10)){
  a <-substr(df10$time[i], start=1, stop=10)
  b <-substr(df10$time[i], start=12, stop=19)
  c <- paste(a, b, sep=" ")
  as.chron(c)
  df10$time[i] = c
}

#Need to get utm-coordinates
spat_df10 <- SpatialPointsDataFrame(coords=df10[, c("longitude", "latitude")],
                                   data=df10['time'],   # This needs to be a data frame
                                   proj4string=CRS("+proj=lonlat +datum=WGS84"))
# This step converts the longitude/latitude -> UTM
utm_df10 <- spTransform(spat_df10, CRSobj = "+proj=utm +zone=12 +datum=WGS84")
utm_coords10 <- coordinates(utm_df10)

#Create a data frame with the original coordinates and time
#And add the utm coordinates
d10<-as.data.frame(utm_coords10)
d10<-cbind(df10, d10)
names(d10)[4] <- "utm_longitude"
names(d10)[5] <- "utm_latitude"

#time differences between each point
dt10 <- c(as.numeric(difftime(d10$time[2], d10$time[1], units = "secs")))
for(i in 3:nrow(d10)){
  dt10 <- c(dt10, as.numeric(difftime(d10$time[i], d10$time[i-1], units = "secs")))
}

dt10<-c(dt10, NA)
d10<-cbind(d10, dt10)
names(d10)[6] <- "dt"

#time since start of journey
pth_time10 <-c(as.numeric(difftime(d10$time[1], d10$time[1], units = "secs")))

for(i in 2:nrow(d10)){
  pth_time10 <- c(pth_time10, as.numeric(difftime(d10$time[i], d10$time[1], units = "secs")))
}

d10<-cbind(d10, pth_time10)
names(d10)[7] <- "pth_time"

#Distance between coordinates (using utm coordinates)
a<-(d10$utm_longitude[2]-d10$utm_longitude[1])^2
b<-(d10$utm_latitude[2]-d10$utm_latitude[1])^2
dist10 <-c(sqrt(a+b))
for(i in 3:nrow(d10)){
  a<-(d10$utm_longitude[i]-d10$utm_longitude[i-1])^2
  b<-(d10$utm_latitude[i]-d10$utm_latitude[i-1])^2
  dist10<-c(dist10, sqrt(a+b))
}
dist10<-c(dist10, NA)

d10<-cbind(d10, dist10)
names(d10)[8] <- "dist"

#Speed between each point
vel_10<-c(d10$dist[1]/d10$dt[1])
for(i in 2:nrow(d10)){
  vel_10<-c(vel_10, d10$dist[i]/d10$dt[i])
}

d10<-cbind(d10, vel_10)
names(d10)[9] <- "speed"

for(i in 1:nrow(d10)){
  if(is.infinite(d10$speed[i])){
    d10$speed[i]<-NA 
  }
}

#Specify the Day (Monday)
day <- rep(c("Monday"),each=nrow(d10))
d10<- cbind(day, d10)
#And specify which one (Second Monday)
num <- rep(c("Monday_2"),each=nrow(d10))
d10<- cbind(num, d10)

#Find least distance with the bridge point

a<-(d10$utm_longitude[1]-longlats$utm_long[1])^2
b<-(d10$utm_latitude[1]-longlats$utm_lat[1])^2
dist<-c(sqrt(a+b))

for(i in 2:nrow(d10)){
    a<-(d10$utm_longitude[i]-longlats$utm_long[1])^2
    b<-(d10$utm_latitude[i]-longlats$utm_lat[1])^2
    dist<-c(dist, sqrt(a+b))
}

min(dist, na.rm=TRUE)
for (i in 1:length(dist)){
    if(dist[i]==min(dist, na.rm=TRUE)){
        print(i)
    }
}

#Actual path time at that point
d10$pth_time[102]
d10$pth_time[102]/60

lon<-d10$longitude[102]-d10$longitude[1]
lat<-d10$latitude[102]-d10$latitude[1]

#For the third point
pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
pred_pth_time
pred_pth_time/60

914.208732762144-783

#The (first) point is 10.49 meters from Wayne's position --> BRIDGE
#the predicted path time at this point is 914.21 seconds (15.24 minutes)
#The real path time is 783 seconds (13.05 minutes)
#There is a 131.21 second difference 

#Test with second point (after bridge)

a<-(d10$utm_longitude[1]-longlats$utm_long[2])^2
b<-(d10$utm_latitude[1]-longlats$utm_lat[2])^2
dist<-c(sqrt(a+b))

for(i in 2:nrow(d10)){
    a<-(d10$utm_longitude[i]-longlats$utm_long[2])^2
    b<-(d10$utm_latitude[i]-longlats$utm_lat[2])^2
    dist<-c(dist, sqrt(a+b))
}

min(dist, na.rm=TRUE)
for (i in 1:length(dist)){
    if(dist[i]==min(dist, na.rm=TRUE)){
        print(i)
    }
}

#Actual path time at that point
d10$pth_time[114]
d10$pth_time[114]/60

lon<-d10$longitude[114]-d10$longitude[1]
lat<-d10$latitude[114]-d10$latitude[1]

#For the third point
pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
pred_pth_time
pred_pth_time/60

1102.25870697452-945

#The (second) point is 7.85 meters from Wayne's position --> after crossing bridge
#the predicted path time at this point is 1102.26 seconds (18.37 minutes)
#The real path time is 945 seconds (15.75 minutes)
#There is a 157.26 second difference 

#Level of confidence has two parts
#i) confidence of location
#ii) confidence of time
#After reviewing the Kalman filters and the Missoula Montana actual paths
#I am confident that the two places chosen will be crossed by Wayne on a given day
#However, since the GPS has variance of about 20 m, 
#You're gonna pass there but the GPS might make a mistake
#The gps might not show it since it has a varaiance of 
#the level of confidence in the time depends on the ols (standard error), f, and all that jazz
#however, a caviat, since there is heteroskedasticity this level of confidence might be missleading 

#confidence interval for predicted time
#predicted +- standard error*critical t value 

72.66*1.96
