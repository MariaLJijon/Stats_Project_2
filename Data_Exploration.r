library("jsonlite")
library("chron")
library("dlm")
library("sp")
library("rgdal")
library("dplyr")

#2020-08-18-Tuesday_1
#Read the data 
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

head(df1)

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

head(d1)

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

head(d1)

#Map Wayne's trajectory on Tuesday_1
plot(d1$longitude, d1$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Tuesday_1 path", col="blue")
#Green lines mark the start of his journey
abline(v=d1$longitude[1], col="green")
abline(h=d1$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d1$longitude[nrow(d1)], col="darkgreen")
abline(h=d1$latitude[nrow(d1)], col="darkgreen")

#When and where did Wayne start/end his journey on Tuesday, 2020-08-18
#How many data points do we have for this day?
print("start:")
d1$time[1]
d1$longitude[1]
d1$latitude[1]

print("end:")
d1$time[nrow(d1)]
d1$longitude[nrow(d1)]
d1$latitude[nrow(d1)]

print(" num data points:")
nrow(d1)
print("time seconds:")
sum(d1$dt, na.rm = TRUE)
print("time hours:")
sum(d1$dt, na.rm = TRUE)/3600

#On Tuesday (2020-08-18), 
#Wayne started his journey at 17:50:40
#He started at longitude = -114.000521, latitude = 46.8863239
#In Google Maps this is 700 Turner St, Missoula, MT 59802, USA

#He ended his journey at 22:07:14
#The end of his journey was at longitude = -113.9926389, latitude = 46.8737222
#In Google Maps this is 432 N Higgins Ave, Missoula, MT 59802, USA

#There are 90 recorded time records during his journey of 15394 seconds (4.28 hours)

#Understand the changes in time 
#Find the activity sessions within his journey

mean(d1$dt, na.rm = TRUE)
median(d1$dt, na.rm = TRUE)
max(d1$dt, na.rm = TRUE)
min(d1$dt, na.rm = TRUE)

#The mean is 172.9; the median is 32. The minimum is 1 and the maximum is 12295. 
#This means there were moments when Wayne stopped in his journey
#We must find these outliers to determine the sessions of movement

#Time difference  distribution
plot(d1$dt, ylab = "Time difference", na.rm = TRUE, main="Tuesday_1 time difference distribution")

#Overall the time difference seems constant, or seems to follow a trend. Except for one point. 
#This must be a large period of non-activity

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d1)){
    if(d1$dt[i]>119){
        print(d1$dt[i])
        print(i)
        print("---")
    }
}

#There is one period of non-activity on Tuesday(2020-08-18) d1$dt[80]

print("time difference:")
d1$dt[80]
d1$dt[80]/3600
print("start break:")
d1$time[80]
print("end break:")
d1$time[81]
print("start longitude/latitude:")
d1$longitude[80]
d1$latitude[80]
print("end longitude/latitude:")
d1$longitude[81]
d1$latitude[81]
print("distance between points:")
d1$dist[80]
print("speed at break:")
d1$speed[80]

#The large break at d1$dt[80] was 12295 seconds long (3.42 hours)
#It started at 18:37:36 and ended at 22:02:31
#The start point of the break was longitude: -113.98418167, latitude: 46.85985495
#Google maps shows this is University District, Missoula, MT 59812, USA
#The end point of the break was longitude: -113.9910075, latitude: 46.87116028
#Google maps shows this is 398-300 E Broadway St, Missoula, MT 59802, USA
#The distance between these points (using utm-coordinates) is 1360.61 meters
#The speed between these points (using utm-coordinates) is 0.11 m/s 
#This means that Wayne was still for a period of time (unknown) 
#but he moved to a different location without his his phone tracking him
#This is why the distance is big, the speed is slow, and the time difference is very large
#An important fact to note is that he stopped at the University District during this break

#Plot Wayne's trajectory on Tuesday_1 with this break
plot(d1$longitude, d1$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Tuesday_1 path with breaks", col="blue")
#Red lines mark the starting point of the break
abline(v=d1$longitude[80], col="red")
abline(h=d1$latitude[80], col="red")
#Pink lines mark the end point of the break
abline(v=d1$longitude[81], col="pink")
abline(h=d1$latitude[81], col="pink")
#Green lines mark the start of his journey
abline(v=d1$longitude[1], col="green")
abline(h=d1$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d1$longitude[nrow(d1)], col="darkgreen")
abline(h=d1$latitude[nrow(d1)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break
#i.e. the University District of Montana
#He got there at 18:37:36

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d1$speed, na.rm = TRUE)
median(d1$speed, na.rm = TRUE)
max(d1$speed, na.rm = TRUE)
min(d1$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 6.7 m/s (clearly there are outliers)
#The median is 2.03 m/s (maybe Wayne jogged?)
#The maximum speed is 101.64 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d1$speed, ylab = "Speed", na.rm = TRUE, main="Tuesday_1 speed distribution")

#The speed distribution seems to show a fairly constant speed with several outliers
#I will check if this holds for other days
#If such is the case, outliers are measuring errors and not jogging/vehicles

#2020-08-25-Tuesday_2
#Read the data 
#Get time, longitude, latitude
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

head(d2)

#Map Wayne's trajectory on Tuesday_2
plot(d2$longitude, d2$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Tuesday_2 path",col="blue")
#Green lines mark the start of his journey
abline(v=d2$longitude[1], col="green")
abline(h=d2$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d2$longitude[nrow(d2)], col="darkgreen")
abline(h=d2$latitude[nrow(d2)], col="darkgreen")

#When and where did Wayne start/end his journey on Tuesday, 2020-08-18
#How many data points do we have for this day?
print("start:")
d2$time[1]
d2$longitude[1]
d2$latitude[1]

print("end:")
d2$time[nrow(d2)]
d2$longitude[nrow(d2)]
d2$latitude[nrow(d2)]

print(" num data points:")
nrow(d2)
print("time seconds:")
sum(d2$dt, na.rm = TRUE)
print("time hours:")
sum(d2$dt, na.rm = TRUE)/3600

#On Tuesday (2020-08-25), 
#Wayne started his journey at 18:15:31 (later than the previous Tuesday-->17:50:40)
#He started at longitude = -114.0001423, latitude = 46.88749925
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA
#This is close to St. Mary's Cemetery 
#And very close to last Tuesday's starting point (700 Turner St, Missoula, MT 59802, USA)


#He ended his journey at 22:16:57 (about the same time as the previous Tuesday-->22:07:14)
#The end of his journey was at longitude = -114.00014976, latitude = 46.88745131
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA (Practically the same place where he started)
#Does this mean this is where Wayne lives? and he left and returned home?

#There are 711 recorded time records during his journey of 14486 seconds (4.02 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Tuesday as last one?

mean(d2$dt, na.rm = TRUE)
median(d2$dt, na.rm = TRUE)
max(d2$dt, na.rm = TRUE)
min(d2$dt, na.rm = TRUE)

#The mean is 20.4; the median is 7. The minimum is 1 and the maximum is 9171 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d2$dt, ylab = "Time difference", na.rm = TRUE, main="Tuesday_2 time difference distribution")

#The time difference seems to be overall constant
#Except for one point

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d2)){
    if(d2$dt[i]>119){
        print(d2$dt[i])
        print(i)
        print("---")
    }
}

#There is one period of non-activity on Tuesday(2020-08-25)
#d2$dt[359] where there is a break of 9171 seconds

print("time difference:")
d2$dt[359]
d2$dt[359]/3600
print("start break:")
d2$time[359]
print("end break:")
d2$time[360]
print("start longitude/latitude:")
d2$longitude[359]
d2$latitude[359]
print("end longitude/latitude:")
d2$longitude[360]
d2$latitude[360]
print("distance between points:")
d2$dist[359]
print("speed at break:")
d2$speed[359]

#The large break at d2$dt[359] was 9171 seconds long (2.55 hours)
#It started at 18:58:26 and ended at 21:31:17
#The start point of the break was longitude: -113.98519881, latitude: 46.85989284
#Google maps shows this as University District, Missoula, MT 59812, USA
#The end point of the break was longitude: -113.9853323, latitude: 46.8606535
#Google maps shows this as University District, Missoula, MT 59812, USA
#The distance between these points (using utm-coordinates) is 85.19 meters
#The speed between these points (using utm-coordinates) is 0.009 m/s 
#By looking at Google Maps I can tell that Wayne stayed in the park in the vecinity of Grizzly Statue during that time
#He seems to do that on Tuesdays (in the breaks)

#Plot Wayne's trajectory on Tuesday_2 with this break
plot(d2$longitude, d2$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Tuesday_2 path with breaks",col="blue")

#Red lines mark the starting point of the break
abline(v=d2$longitude[359], col="red")
abline(h=d2$latitude[359], col="red")
#Pink lines mark the end point of the break
abline(v=d2$longitude[360], col="pink")
abline(h=d2$latitude[360], col="pink")
#Green lines mark the start of his journey
abline(v=d2$longitude[1], col="green")
abline(h=d2$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d2$longitude[nrow(d2)], col="darkgreen")
abline(h=d2$latitude[nrow(d2)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break 
#i.e. the University District of Montana, park near Grizzly Statue
#He got there at 18:58:26, about 20 minutes later than the previous Tuesday (18:37:36)
#Unlike last Tuesday, this path (with the break) seems more consistent
#He returned to his starting point through a different path 

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d2$speed, na.rm = TRUE)
median(d2$speed, na.rm = TRUE)
max(d2$speed, na.rm = TRUE)
min(d2$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 6.47 m/s (clearly there are outliers)
#The median is 2.79 m/s (maybe Wayne jogged?)
#The maximum speed is 77.87 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d2$speed, ylab = "Speed", na.rm = TRUE, main="Tuesday_2 speed distribution",)

#The speed distribution plot shows that there does seem to be a trend in speed
#But there are MANY outliers

#Tuesday_1 vs Tuesday_2
#Plot both Tuesdays in same graph
#Mark their start/end points

#Map Wayne's trajectory on Tuesday_1
plot(d1$longitude, d1$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Tuesday_1 vs Tuesday_2", col="blue")
#Green lines mark the start of his journey
abline(v=d1$longitude[1], col="green")
abline(h=d1$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d1$longitude[nrow(d1)], col="darkgreen")
abline(h=d1$latitude[nrow(d1)], col="darkgreen")

par(new=TRUE)

#Map Wayne's trajectory on Tuesday_2
plot(d2$longitude, d2$latitude, axes = FALSE, xlab="", ylab="", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     col="lightblue")
#Green lines mark the start of his journey
abline(v=d2$longitude[1], col="magenta")
abline(h=d2$latitude[1], col="magenta")
#Dark green lines amrk the end of his journey
abline(v=d2$longitude[nrow(d2)], col="purple")
abline(h=d2$latitude[nrow(d2)], col="purple")

#The start/end point of the second Tuesday (as said before) are practicaly the same
#The start of the first Tuesday is very close to the start/end point of the second one
#The end point of the first Tuesday is far, maybe WAyne's phone ran out of battery before returning home
#There does seem to be a pattern in which path Wayne chooses to take from
#his starting point to the University Grizzly Sture part
#However, the first Tuesday does not have as many data points as the second one, 
#and there seems to be some time missing
#This day may not be very reliable
#In any case we need to smooth these paths using a Kalman Smoother

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

head(d3)

#Map Wayne's trajectory on Wednesday_1
plot(d3$longitude, d3$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Wednesday_1 path",col="blue")
#Green lines mark the start of his journey
abline(v=d3$longitude[1], col="green")
abline(h=d3$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d3$longitude[nrow(d3)], col="darkgreen")
abline(h=d3$latitude[nrow(d3)], col="darkgreen")

#When and where did Wayne start/end his journey on Wednesday, 2020-08-19
#How many data points do we have for this day?
print("start:")
d3$time[1]
d3$longitude[1]
d3$latitude[1]

print("end:")
d3$time[nrow(d3)]
d3$longitude[nrow(d3)]
d3$latitude[nrow(d3)]

print(" num data points:")
nrow(d3)
print("time seconds:")
sum(d3$dt, na.rm = TRUE)
print("time hours:")
sum(d3$dt, na.rm = TRUE)/3600

#On Wednesday (2020-08-19), 
#Wayne started his journey at 19:27:55 
#He started at longitude = -114.0005151, latitude = 46.8870326
#In Google Maps this is 700-898 Palmer St, Missoula, MT 59802, USA
#Very close to Tuesday_1's starting point(700 Turner St, Missoula, MT 59802, USA)

#He ended his journey at 00:07:20 
#The end of his journey was at longitude = -114.0000823, latitude = 46.8873958
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA (very close to starting point)
#Maybe Wayne lives in this area

#There are 249 recorded time records during his journey of 16765 seconds (4.66 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Wednesday as on Tuesday?

mean(d3$dt, na.rm = TRUE)
median(d3$dt, na.rm = TRUE)
max(d3$dt, na.rm = TRUE)
min(d3$dt, na.rm = TRUE)

#The mean is 67.6; the median is 20. The minimum is 1 and the maximum is 11360 
#This means there were moments when Wayne stopped in his journey, there are outliers

#Time difference  distribution
plot(d3$dt, ylab = "Time difference", na.rm = TRUE, main="Wednesday_1 time difference distribution")

#As seen in previous days the dt looks constant save for one point

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d3)){
  if(d3$dt[i]>119){
    print(d3$dt[i])
    print(i)
    print("---")
  }
}

#One period of non-activity on Wednesday(2020-08-19)
#d3$dt[123] where there is a break of 11360 seconds

print("time difference:")
d3$dt[123]
d3$dt[123]/3600
print("start break:")
d3$time[123]
print("end break:")
d3$time[124]
print("start longitude/latitude:")
d3$longitude[123]
d3$latitude[123]
print("end longitude/latitude:")
d3$longitude[124]
d3$latitude[124]
print("distance between points:")
d3$dist[123]
print("speed at break:")
d3$speed[123]

#The large break at d3$dt[123] was 11360 seconds long (3.16 hours)
#It started at 20:12:50 and ended at 23:22:10
#The start point of the break was longitude: -113.98503975, latitude: 46.85977479
#Google maps shows this as University District, Missoula, MT 59812, USA (very similar to Tuesday's break)
#The end point of the break was longitude: -113.98526996, latitude: 46.85994711
#Google maps shows this as 32 Campus Dr, Missoula, MT 59812, United States
#The distance between these points (using utm-coordinates) is 25.99 meters
#The speed between these points (using utm-coordinates) is 0.002 m/s 
#By looking at Google Maps I can tell that Wayne stayed in the park in the vecinity of Grizzly Statue during that time
#He seems to do that on Tuesdays as well (in the breaks)

#Plot Wayne's trajectory on Wednesday_1 with this break
plot(d3$longitude, d3$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Wednesday_1 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d3$longitude[123], col="red")
abline(h=d3$latitude[123], col="red")
#Pink lines mark the end point of the break
abline(v=d3$longitude[124], col="pink")
abline(h=d3$latitude[124], col="pink")
#Green lines mark the start of his journey
abline(v=d3$longitude[1], col="green")
abline(h=d3$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d3$longitude[nrow(d3)], col="darkgreen")
abline(h=d3$latitude[nrow(d3)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break 
#i.e. the University District of Montana, park near Grizzly Statue

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d3$speed, na.rm = TRUE)
median(d3$speed, na.rm = TRUE)
max(d3$speed, na.rm = TRUE)
min(d3$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 6.47 m/s (similar to Tuesday)
#The median is 2.08 m/s 
#The maximum speed is 128.87 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d3$speed, ylab = "Speed", na.rm = TRUE, main="Wednesday_1 speed distribution")

#The speed distribution plot shows that there does seem to be a trend in speed
#But there are outliers (measuring errors perhaps?)

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

head(d4)

#Map Wayne's trajectory on Wednesday_2
plot(d4$longitude, d4$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.07, -113.985), ylim=c(46.855, 46.92),
     main="Wednesday_2 path", col="blue")
#Green lines mark the start of his journey
abline(v=d4$longitude[1], col="green")
abline(h=d4$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d4$longitude[nrow(d4)], col="darkgreen")
abline(h=d4$latitude[nrow(d4)], col="darkgreen")

#When and where did Wayne start/end his journey on Wednesday, 2020-08-26
#How many data points do we have for this day?
print("start:")
d4$time[1]
d4$longitude[1]
d4$latitude[1]

print("end:")
d4$time[nrow(d4)]
d4$longitude[nrow(d4)]
d4$latitude[nrow(d4)]

print(" num data points:")
nrow(d4)
print("time seconds:")
sum(d4$dt, na.rm = TRUE)
print("time hours:")
sum(d4$dt, na.rm = TRUE)/3600

#On Wednesday (2020-08-26), 
#Wayne started his journey at 19:18:54, about nine minutes earlier than the previous Wednesday (19:27:55)
#He started at longitude=-114.00021406, latitude=46.88750954
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA
#This is very close to Wednesday_1's starting point(700-898 Palmer St, Missoula, MT 59802, USA)

#He ended his journey at 04:21:13 of Thursday 2020-08-27
#but since it is one path we will still count this as Wednesday_2
#The end of his journey was at longitude=-114.00077532, latitude=46.88808643
#Google Maps shows this as 725-709 Charlo St, Missoula, MT 59802, USA (close to his starting point)

#There are 865 recorded time records during his journey of 32539 seconds (9.04 hours), much longer than Wednesday_1

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Wednesday as last one?

mean(d4$dt, na.rm = TRUE)
median(d4$dt, na.rm = TRUE)
max(d4$dt, na.rm = TRUE)
min(d4$dt, na.rm = TRUE)

#The mean is 37.66; the median is 8. The minimum is 1 and the maximum is 13136 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d4$dt, ylab = "Time difference", na.rm = TRUE, main="Wednesday_2 time difference distribution")

#The dt distribution looks constatn save for two points

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d4)){
  if(d4$dt[i]>119){
    print(d4$dt[i])
    print(i)
    print("---")
  }
}

#There are 2 periods of non-activity on Wednesday(2020-08-26)
#The first one is at d4$dt[422] of 13136 seconds
#The second one is at d4$dt[779] of 12934 seconds

#Lets examine the first break (d4$dt[422])
print("time difference:")
d4$dt[422]
d4$dt[422]/3600
print("start break:")
d4$time[422]
print("end break:")
d4$time[423]
print("start longitude/latitude:")
d4$longitude[422]
d4$latitude[422]
print("end longitude/latitude:")
d4$longitude[423]
d4$latitude[423]
print("distance between points:")
d4$dist[422]
print("speed at break:")
d4$speed[422]

#The large break at d2$dt[422] was 13136 seconds long (3.65 hours)
#It started at 20:11:11 and ended at 23:50:07 (very similar to last Wednesday's break (start:20:12:50, end:23:22:10))
#The start point was longitude=-113.98605016, latitude=46.85962753
#Google maps shows this as University District, Missoula, MT 59812, USA (like last Wednesday and Tuesday)
#Looking at the map it is the same park by Grizzly Statue but a different part of the park
#The end point of the break is longitude:-113.98517711, latitude:46.8598412
#Google maps shows this as University District, Missoula, MT 59812, USA; the same park
#The distance between these points (usint utm-coordinates) is 70.7 meters
#The speed between these points (using utm-coordinates) is 0.005 m/s 
#By looking at Google Maps I can tell that Wayne stayed in the park in the vecinity of Grizzly Statue during that time
#He seems to do that on Wednesdays around this time, and a bit earlier on Tuesdays

#Lets examine the second break (d4$dt[779])
print("time difference:")
d4$dt[779]
d4$dt[779]/3600
print("start break:")
d4$time[779]
print("end break:")
d4$time[780]
print("start longitude/latitude:")
d4$longitude[779]
d4$latitude[779]
print("end longitude/latitude:")
d4$longitude[780]
d4$latitude[780]
print("distance between points:")
d4$dist[779]
print("speed at break:")
d4$speed[779]

#The large break at d2$dt[779] was 12934 seconds long (3.59 hours)
#It started at 00:35:44 and ended at 04:11:18
#Wednesday_2's path and duration is much longer than last Wednesday
#This second break shows that Wayne probably decided to have a night out
#The start point was longitude=-114.0001879, latitude=46.88738864
#Google maps shows this as 1600-1698 Holmes St, Missoula, MT 59802, USA (oddly close to the start of Wednesday_2)
#The end point of the break is longitude:-114.07046128, latitude:46.91914519
#Google maps shows this as 5399-5201 Expressway, Missoula, MT 59808, USA 
#The distance between these points (usint utm-coordinates) is 6415.1 meters (not very close)
#The speed between these points (using utm-coordinates) is 0.5 m/s 
#Maybe this day Wayne had to go pick up someone/something at the Expressway station very late at night
#During this time it is marked as a break because his phone was off 

#Plot Wayne's trajectory on Wednesday_2 with these breaks
plot(d4$longitude, d4$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.07, -113.985), ylim=c(46.855, 46.92),
     main="Wednesday_2 path with breaks",col="blue")

#Red lines mark the starting point of the first break
abline(v=d4$longitude[422], col="red")
abline(h=d4$latitude[422], col="red")
#Pink lines mark the end point of the first break
abline(v=d4$longitude[423], col="pink")
abline(h=d4$latitude[423], col="pink")
#Orange lines mark the starting point of the second break
abline(v=d4$longitude[779], col="orange")
abline(h=d4$latitude[779], col="orange")
#Brown lines mark the end point of the second break
abline(v=d4$longitude[780], col="brown")
abline(h=d4$latitude[780], col="brown")
#Green lines mark the start of his journey
abline(v=d4$longitude[1], col="green")
abline(h=d4$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d4$longitude[nrow(d4)], col="darkgreen")
abline(h=d4$latitude[nrow(d4)], col="darkgreen")

#The plot shows that as Wednesday_1 Wayne went from home 
#to the University District of Montana, park near Grizzly Statue
#He did this around the same time
#After this first break he returned home and later that night he went to the Expressway station
#his phone was probably off on the way there (so it is marked as a period of non-activity)
#But when returning home we can see the route he took
#The end point is very similar to the starting point

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d4$speed, na.rm = TRUE)
median(d4$speed, na.rm = TRUE)
max(d4$speed, na.rm = TRUE)
min(d4$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 6.83 m/s (clearly there are outliers)
#The median is 3.1 m/s 
#The maximum speed is 179.53 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d4$speed, ylab = "Speed", na.rm = TRUE, main="Wednesday_2 speed distribution",)

#The speed distribution plot shows that there appears to be a trend in speed
#There is a slight bump in this trend at the end
#But there are MANY outliers

#Wednesday_1 vs Wednesday_2
#Plot both Wednesdays in same graph
#Mark their start/end points

#Map Wayne's trajectory on Wednesday_1
plot(d3$longitude, d3$latitude, xlab="longitude", ylab="longitude",
     xlim=c(-114.07, -113.985), ylim=c(46.855, 46.92),
     main="Wednesday_1 vs Wednesday_2", col="blue")
#Green lines mark the start of his journey
abline(v=d3$longitude[1], col="green")
abline(h=d3$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d3$longitude[nrow(d3)], col="darkgreen")
abline(h=d3$latitude[nrow(d3)], col="darkgreen")

par(new=TRUE)

#Map Wayne's trajectory on Wednesday_2
plot(d4$longitude, d4$latitude, axes = FALSE, xlab="", ylab="", 
     xlim=c(-114.07, -113.985), ylim=c(46.855, 46.92),
     col="lightblue")
#Green lines mark the start of his journey
abline(v=d4$longitude[1], col="magenta")
abline(h=d4$latitude[1], col="magenta")
#Dark green lines amrk the end of his journey
abline(v=d4$longitude[nrow(d4)], col="purple")
abline(h=d4$latitude[nrow(d4)], col="purple")

#The start/end point of both Wednesdays appear to be practically the same
#On the second Wednesday Wayne took an unexpected journey to the Expressway Station
#Something he did not do the first Wednesday
#this can be considered an anomaly
#On both Wednesdays Wayne went to the University Grizzly Statue Park around the same time

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

head(d5)

#Map Wayne's trajectory on Thursday_1
plot(d5$longitude, d5$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Thursday_1 path", col="blue")
#Green lines mark the start of his journey
abline(v=d5$longitude[1], col="green")
abline(h=d5$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d5$longitude[nrow(d5)], col="darkgreen")
abline(h=d5$latitude[nrow(d5)], col="darkgreen")

#When and where did Wayne start/end his journey on Tuesday, 2020-08-18
#How many data points do we have for this day?
print("start:")
d5$time[1]
d5$longitude[1]
d5$latitude[1]

print("end:")
d5$time[nrow(d5)]
d5$longitude[nrow(d5)]
d5$latitude[nrow(d5)]

print(" num data points:")
nrow(d5)
print("time seconds:")
sum(d5$dt, na.rm = TRUE)
print("time hours:")
sum(d5$dt, na.rm = TRUE)/3600

#On Thursday (2020-08-20), 
#Wayne started his journey at 21:13:09 (later than Tuesdays and Wednesdays)
#He started at longitude= -114.00013904, latitude= 46.88713864
#Google Maps shos this is 1600-1698 Holmes St, Missoula, MT 59802, USA (similar to previous starting points)

#He ended his journey at 00:51:41 (still count it as Thursday, not Friday)
#The end of his journey was at longitude = -113.9970166, latitude = 46.8756735
#Google Maps shows this as 549-501 Old U.S. 93, Missoula, MT 59802, USA

#The start point is very different from the end point, why?
#Phone out of battery? or other reason...

#There are 354 recorded time records during his journey of 13112 seconds (3.64 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Tuesday as last one?

mean(d5$dt, na.rm = TRUE)
median(d5$dt, na.rm = TRUE)
max(d5$dt, na.rm = TRUE)
min(d5$dt, na.rm = TRUE)

#The mean is 37.14; the median is 11. The minimum is 0 and the maximum is 8459 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d5$dt, ylab = "Time difference", na.rm = TRUE, main="Thursday_1 time difference distribution")

#As seen before the dt distribution looks constant except for one point

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d5)){
  if(d5$dt[i]>119){
    print(d5$dt[i])
    print(i)
    print("---")
  }
}

#There is one large period of non-activity on Thursday(2020-08-20)
#This is d5$dt[225] where there is a break of 8459 seconds

print("time difference:")
d5$dt[225]
d5$dt[225]/3600
print("start break:")
d5$time[225]
print("end break:")
d5$time[226]
print("start longitude/latitude:")
d5$longitude[225]
d5$latitude[225]
print("end longitude/latitude:")
d5$longitude[226]
d5$latitude[226]
print("distance between points:")
d5$dist[225]
print("speed at break:")
d5$speed[225]

#The break at d5$dt[225] was 8459 seconds long (2.35 hours)
#It started at 22:03:22 and ended at 00:24:21
#The start point of the break was longitude: -113.98590667, latitude: 46.85954398
#Google maps shows this as University District, Missoula, MT 59812, USA, the park by Grizzly Statue
#The end point of the break was longitude: -113.98525421, latitude: 46.85991266
#Google maps shows this as University District, Missoula, MT 59812, USA, in the same park
#The distance between these points (using utm-coordinates) is 64,48 meters
#The speed between these points (using utm-coordinates) is 0.007 m/s 
#By looking at Google Maps I can tell that Wayne stayed in the park in the vecinity of Grizzly Statue during that time

#Plot Wayne's trajectory on Thursday_1 with this break
plot(d5$longitude, d5$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Thursday_1 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d5$longitude[225], col="red")
abline(h=d5$latitude[225], col="red")
#Pink lines mark the end point of the break
abline(v=d5$longitude[226], col="pink")
abline(h=d5$latitude[226], col="pink")
#Green lines mark the start of his journey
abline(v=d5$longitude[1], col="green")
abline(h=d5$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d5$longitude[nrow(d5)], col="darkgreen")
abline(h=d5$latitude[nrow(d5)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was during the break
#He did move in this break but stayed in the vecinity of park near Grizzly Statue 
#His starting point was not the same as the end point

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d5$speed, na.rm = TRUE)
median(d5$speed, na.rm = TRUE)
max(d5$speed, na.rm = TRUE)
min(d5$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 4.06 m/s 
#The median is 1.99 m/s (maybe Wayne jogged?)
#The maximum speed is 51.1 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d5$speed, ylab = "Speed", na.rm = TRUE, main="Thursday_1 speed distribution",)

#The speed distribution plot shows that there does seem to be a a kind of trend in speed
#But there are MANY outliers

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

head(d6)

#Map Wayne's trajectory on Tuesday_2
plot(d6$longitude, d6$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Thursday_2 path", col="blue")
#Green lines mark the start of his journey
abline(v=d6$longitude[1], col="green")
abline(h=d6$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d6$longitude[nrow(d6)], col="darkgreen")
abline(h=d6$latitude[nrow(d6)], col="darkgreen")

#When and where did Wayne start/end his journey on Thursday, 2020-08-27
#How many data points do we have for this day?
print("start:")
d6$time[1]
d6$longitude[1]
d6$latitude[1]

print("end:")
d6$time[nrow(d6)]
d6$longitude[nrow(d6)]
d6$latitude[nrow(d6)]

print(" num data points:")
nrow(d6)
print("time seconds:")
sum(d6$dt, na.rm = TRUE)
print("time hours:")
sum(d6$dt, na.rm = TRUE)/3600

#On Thursday (2020-08-27), 
#Wayne started his journey at 17:34:38, much earlier than the previous Thursday (21:13:09)
#He started at longitude= -114.00010499, latitude= 46.88747926
#Google Maps shows this as 1600-1698 Holmes St, Missoula, MT 59802, USA (like last Thursday, and other starting points)

#Wayne ended his journey at 23:09:57, about 20 minutes before the previous Thursday (00:51:41)
#The end point was at longitude= -114.0001869, latitude= 46.88739359
#This is 1600-1698 Holmes St, Missoula, MT 59802, USA (practlically the same as the starting point)

#There are 712 recorded time records during his journey of 20119 seconds (5.59 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Thursday as last one?

mean(d6$dt, na.rm = TRUE)
median(d6$dt, na.rm = TRUE)
max(d6$dt, na.rm = TRUE)
min(d6$dt, na.rm = TRUE)

#The mean is 28.3; the median is 8. The minimum is 0 and the maximum is 14551 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d6$dt, ylab = "Time difference", na.rm = TRUE, main="Thursday_2 time difference distribution")

#The time difference distribution has a constant trend except for one point, this is a break

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d6)){
  if(d6$dt[i]>119){
    print(d6$dt[i])
    print(i)
    print("---")
  }
}

#One periods of non-activity on Thursday(2020-08-27)
#Except for d6$dt[363] where there is a break of 14551 seconds

print("time difference:")
d6$dt[363]
d6$dt[363]/3600
print("start break:")
d6$time[363]
print("end break:")
d6$time[364]
print("start longitude/latitude:")
d6$longitude[363]
d6$latitude[363]
print("end longitude/latitude:")
d6$longitude[364]
d6$latitude[364]
print("distance between points:")
d6$dist[363]
print("speed at break:")
d6$speed[363]

#The break at d5$dt[363] was 14551 seconds long (4.04 hours)
#It started at 18:21:27 and ended at 22:23:58 
#This is a very different time period from last Thursday (start: 22:03:22; end: 00:24:21)
#The start poit was at longitude= -113.98596252, latitude= 46.85959502
#Google maps shows this is University District, Missoula, MT 59812, USA; the park by Grizzly Statue (his usual spot)
#The end point of the break was longitude= -113.98516191, latitude= 46.85985789
#Google maps shows the same park at University District, Missoula, MT 59812, USA

#The distance between these points (using utm-coordinates) is 67.7 meters
#The speed between these points (using utm-coordinates) is 0.005 m/s 

#Plot Wayne's trajectory on Thursday_2 with this break
plot(d6$longitude, d6$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Thursday_2 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d6$longitude[363], col="red")
abline(h=d6$latitude[363], col="red")
#Pink lines mark the end point of the break
abline(v=d6$longitude[364], col="pink")
abline(h=d6$latitude[364], col="pink")
#Green lines mark the start of his journey
abline(v=d6$longitude[1], col="green")
abline(h=d6$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d6$longitude[nrow(d6)], col="darkgreen")
abline(h=d6$latitude[nrow(d6)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break 
#i.e. the University District of Montana, park near Grizzly Statue (his spot)
#His timing was very different from last Thursday
#He returned to his starting point through a different path 

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d6$speed, na.rm = TRUE)
median(d6$speed, na.rm = TRUE)
max(d6$speed, na.rm = TRUE)
min(d6$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 4.86 m/s 
#The median is 2.62 m/s 
#The maximum speed is 63.49 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d6$speed, ylab = "Speed",  main="Thursday_2 speed distribution", na.rm = TRUE)

#The speed distribution plot shows that there does seem to be somewhat a trend in speed
#But there are MANY outliers

#Thursday_1 vs Thursday_2
#Plot both Tuesdays in same graph
#Mark their start/end points

#Map Wayne's trajectory on Tuesday_1
plot(d5$longitude, d5$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
      main="Thursday_1 vs Thursday_2", col="blue")
#Green lines mark the start of his journey
abline(v=d5$longitude[1], col="green")
abline(h=d5$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d5$longitude[nrow(d5)], col="darkgreen")
abline(h=d5$latitude[nrow(d5)], col="darkgreen")

par(new=TRUE)

#Map Wayne's trajectory on Tuesday_2
plot(d6$longitude, d6$latitude, axes = FALSE, xlab="", ylab="",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     col="lightblue")
#Green lines mark the start of his journey
abline(v=d6$longitude[1], col="magenta")
abline(h=d6$latitude[1], col="magenta")
#Dark green lines amrk the end of his journey
abline(v=d6$longitude[nrow(d6)], col="purple")
abline(h=d6$latitude[nrow(d6)], col="purple")

#The start/end point of the second Thursday (as said before) are practicaly the same
#The start of the first Thursday is very close to the start/end point of the second one
#The end point of the first Thursday is far, maybe WAyne's phone ran out of battery before returning home
#The paths he chose in both days seem similar but not exact
#We would have to use a Kalman Smoother to check the consistency on Thursdays
#He does seem to go to his spot though (University of Montana Park near Grizzly Sture)
#There does seem to be a pattern in which path Wayne chooses to take from
#his starting point to the University Grizzly Sture part
#Because of such different time frames on Thursdays this day may not be very reliable

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

head(d7)

#Map Wayne's trajectory on Friday_1
plot(d7$longitude, d7$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Friday_1 path", col="blue")
#Green lines mark the start of his journey
abline(v=d7$longitude[1], col="green")
abline(h=d7$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d7$longitude[nrow(d7)], col="darkgreen")
abline(h=d7$latitude[nrow(d7)], col="darkgreen")

#When and where did Wayne start/end his journey on Friday, 2020-08-21
#How many data points do we have for this day?
print("start:")
d7$time[1]
d7$longitude[1]
d7$latitude[1]

print("end:")
d7$time[nrow(d7)]
d7$longitude[nrow(d7)]
d7$latitude[nrow(d7)]

print(" num data points:")
nrow(d7)
print("time seconds:")
sum(d7$dt, na.rm = TRUE)
print("time hours:")
sum(d7$dt, na.rm = TRUE)/3600

#On Friday (2020-08-21), 
#Wayne started his journey at 17:17:21 
#He started at longitude = -114.0007782, latitude = 46.8868786
#In Google Maps this is 711 Palmer St, Missoula, MT 59802, USA
#This is close to St. Mary's Cemetery and previous starting points

#He ended his journey at 00:24:53 
#The end of his journey was at longitude = -114.0000347, latitude = 46.88742157
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA (close to starting point)

#There are 667 recorded time records during his journey of 25652 seconds (7.13 hours)

#Understand the changes in time 
#Find the activity sessions within his journey

mean(d7$dt, na.rm = TRUE)
median(d7$dt, na.rm = TRUE)
max(d7$dt, na.rm = TRUE)
min(d7$dt, na.rm = TRUE)

#The mean is 38.52; the median is 8. The minimum is 0 and the maximum is 20257 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d7$dt, ylab = "Time difference", na.rm = TRUE, main="Friday_1 time difference distribution")

#As expected, the time difference distribution is mostly constant save for one data point

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d7)){
  if(d7$dt[i]>119){
    print(d7$dt[i])
    print(i)
    print("---")
  }
}

#There  is oneperiods of non-activity on Friday(2020-08-21)
#Except for d7$dt[328] where there is a break of 20257 seconds

print("time difference:")
d7$dt[328]
d7$dt[328]/3600
print("start break:")
d7$time[328]
print("end break:")
d7$time[329]
print("start longitude/latitude:")
d7$longitude[328]
d7$latitude[328]
print("end longitude/latitude:")
d7$longitude[329]
d7$latitude[329]
print("distance between points:")
d7$dist[328]
print("speed at break:")
d7$speed[328]

#The break at d7$dt[328] was 20257 seconds long (5.63 hours)
#It started at 18:03:42 and ended at 23:41:19
#The start point of the break was longitude: -113.98524299, latitude: 46.8599788
#Google maps shows this is 32 Campus Dr, Missoula, MT 59812, United States
#i.e. this is the center of the park near Grizzly Statue (his spot)
#The end point of the break was longitude: -113.9853323, latitude: 46.85989745
#Google maps shows this as University District, Missoula, MT 59812, USA (he didn't move muc)
#The distance between these points (using utm-coordinates) is 9.19 meters
#The speed between these points (using utm-coordinates) is 0.0005 m/s 

#Plot Wayne's trajectory on Friday_1 with this break
plot(d7$longitude, d7$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Friday_1 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d7$longitude[359], col="red")
abline(h=d7$latitude[359], col="red")
#Pink lines mark the end point of the break
abline(v=d7$longitude[360], col="pink")
abline(h=d7$latitude[360], col="pink")
#Green lines mark the start of his journey
abline(v=d7$longitude[1], col="green")
abline(h=d7$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d7$longitude[nrow(d7)], col="darkgreen")
abline(h=d7$latitude[nrow(d7)], col="darkgreen")

#The plot shows that the starting point was very close to the ending point
#Wayne practically stayed in the same place (his spot) during his break 
#He took different paths on his way there than on this way back
#This day however, he moved further away from his spot and his starting point

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d7$speed, na.rm = TRUE)
median(d7$speed, na.rm = TRUE)
max(d7$speed, na.rm = TRUE)
min(d7$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 7.1 m/s (clearly there are outliers)
#The median is 2.97 m/s (maybe Wayne jogged?)
#The maximum speed is 78.47 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d7$speed, ylab = "Speed", main="Friday_1 speed distribution", na.rm = TRUE)

#The speed distribution plot shows that there does seem to be a trend in speed
#But there are MANY outliers

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

head(d8)

#Map Wayne's trajectory on Friday_2
plot(d8$longitude, d8$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Friday_2 path", col="blue")
#Green lines mark the start of his journey
abline(v=d8$longitude[1], col="green")
abline(h=d8$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d8$longitude[nrow(d8)], col="darkgreen")
abline(h=d8$latitude[nrow(d8)], col="darkgreen")

#When and where did Wayne start/end his journey on Tuesday, 2020-08-18
#How many data points do we have for this day?
print("start:")
d8$time[1]
d8$longitude[1]
d8$latitude[1]

print("end:")
d8$time[nrow(d8)]
d8$longitude[nrow(d8)]
d8$latitude[nrow(d8)]

print(" num data points:")
nrow(d8)
print("time seconds:")
sum(d8$dt, na.rm = TRUE)
print("time hours:")
sum(d8$dt, na.rm = TRUE)/3600

#On Friday (2020-08-28), 
#Wayne started his journey at 18:28:33, later than the previous Friday (17:17:21) 
#He started at longitude = -114.00011736, latitude = 46.88747376
#Google Maps shows this is 1600-1698 Holmes St, Missoula, MT 59802, USA
#This is close to St. Mary's Cemetery and previous starting points

#He ended his journey at 23:59:12, about half an hour earlier than Friday_1 (00:24:53) 
#The end of his journey was at longitude = -114.00003102, latitude = 46.88740515
#In Google Maps this is 1600-1698 Holmes St, Missoula, MT 59802, USA (practically the same as starting point)

#There are 658 recorded time records during his journey of 19839 seconds (5.51 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Friday as last one?

mean(d8$dt, na.rm = TRUE)
median(d8$dt, na.rm = TRUE)
max(d8$dt, na.rm = TRUE)
min(d8$dt, na.rm = TRUE)

#The mean is 30.2; the median is 8. The minimum is 0 and the maximum is 14411 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d8$dt, ylab = "Time difference", na.rm = TRUE, main="Friday_2 time difference distribution")

#The time distribution looks constant 
#Save for one point that shows a large period of non-activity
#And another point that is also higher thant he rest but not as much

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d8)){
  if(d8$dt[i]>119){
    print(d8$dt[i])
    print(i)
    print("---")
  }
}

#There are 2 periods of non-activity on Friday(2020-08-28)
#The longest break is d8$dt[328] where there is a break of 14411 seconds

print("time difference:")
d8$dt[328]
d8$dt[328]/3600
print("start break:")
d8$time[328]
print("end break:")
d8$time[329]
print("start longitude/latitude:")
d8$longitude[328]
d8$latitude[328]
print("end longitude/latitude:")
d8$longitude[329]
d8$latitude[329]
print("distance between points:")
d8$dist[328]
print("speed at break:")
d8$speed[328]

#The break at d8$dt[328] was 14411 seconds long (4 hours)
#It started at 19:15:40 and ended at 23:15:51
#It started about an hour later than the previous Friday (18:03:42)
#It ended with about 25 minutes before last Friday (23:41:19)
#The start point of the breakwas longitude= -113.98488461, latitude= 46.85983062
#Google Maps shows this is Missoula County, MT, USA; the park by Grizzly Statue (his spot)
#The end point of his break was longitude= -113.98520578, latitude= 46.85980517
#Google Maps shows this is University District, Missoula, MT 59812, USA (de didn't move much from his spot)
#The distance between these points (using utm-coordinates) is 24.66 meters
#The speed between these points (using utm-coordinates) is 0.002 m/s 

#Plot Wayne's trajectory on Friday_2 with this break
plot(d8$longitude, d8$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Friday_2 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d8$longitude[328], col="red")
abline(h=d8$latitude[328], col="red")
#Pink lines mark the end point of the break
abline(v=d8$longitude[329], col="pink")
abline(h=d8$latitude[329], col="pink")
#Green lines mark the start of his journey
abline(v=d8$longitude[1], col="green")
abline(h=d8$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d8$longitude[nrow(d8)], col="darkgreen")
abline(h=d8$latitude[nrow(d8)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break 
#i.e. the University District of Montana, park near Grizzly Statue; his spot
#He took a different path to return home than the one he came by

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d8$speed, na.rm = TRUE)
median(d8$speed, na.rm = TRUE)
max(d8$speed, na.rm = TRUE)
min(d8$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 5.55 m/s 
#The median is 2.94 m/s (maybe Wayne jogged?)
#The maximum speed is 46.35 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0.001 m/s 

#Speed distribution
plot(d8$speed, ylab = "Speed", main="Friday_2 speed distribution", na.rm = TRUE)

#The speed distribution plot shows that there does seem to be somewhat of a trend in speed
#This trend is not as defined
#But there are MANY outliers

#Friday_1 vs Friday_2
#Plot both Fridays in same graph
#Mark their start/end points

#Map Wayne's trajectory on Friday_1
plot(d7$longitude, d7$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Friday_1 vs Friday_2", col="blue")
#Green lines mark the start of his journey
abline(v=d7$longitude[1], col="green")
abline(h=d7$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d7$longitude[nrow(d7)], col="darkgreen")
abline(h=d7$latitude[nrow(d7)], col="darkgreen")

par(new=TRUE)

#Map Wayne's trajectory on Friday_2
plot(d8$longitude, d8$latitude, axes = FALSE, xlab="", ylab="",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     col="lightblue")
#Green lines mark the start of his journey
abline(v=d8$longitude[1], col="magenta")
abline(h=d8$latitude[1], col="magenta")
#Dark green lines amrk the end of his journey
abline(v=d8$longitude[nrow(d8)], col="purple")
abline(h=d8$latitude[nrow(d8)], col="purple")

#The start/end points of the second Friday are practically the same
#The starting/ending points of both Fridays are very close by
#The paths taken both days needs to be smoothed to find a better pattern
#Overall they seem like very similar days
#We would still have to compare with the times at each location

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

head(d9)

#Map Wayne's trajectory on Monday_1
plot(d9$longitude, d9$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Monday_1 path", col="blue")
#Green lines mark the start of his journey
abline(v=d9$longitude[1], col="green")
abline(h=d9$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d9$longitude[nrow(d9)], col="darkgreen")
abline(h=d9$latitude[nrow(d9)], col="darkgreen")

#When and where did Wayne start/end his journey on Monday, 2020-08-24
#How many data points do we have for this day?
print("start:")
d9$time[1]
d9$longitude[1]
d9$latitude[1]

print("end:")
d9$time[nrow(d9)]
d9$longitude[nrow(d9)]
d9$latitude[nrow(d9)]

print(" num data points:")
nrow(d9)
print("time seconds:")
sum(d9$dt, na.rm = TRUE)
print("time hours:")
sum(d9$dt, na.rm = TRUE)/3600

#On Monday (2020-08-24), 
#Wayne started his journey at 19:11:14 
#He started at longitude = -114.00052, latitude = 46.8872596
#Google Maps shows this is 706 Palmer St, Missoula, MT 59802, USA (close to his usual starting points)
#He ended his journey at 23:38:49
#The end of his journey was at longitude= -114.00004697, latitude= 46.88739311
#Google Maps shows this si 1600-1698 Holmes St, Missoula, MT 59802, USA
#This is very close to his starting poing (near St. Mary's cementary)

#There are 684 recorded time records during his journey of 16055 seconds (4.46 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Tuesday as last one?

mean(d9$dt, na.rm = TRUE)
median(d9$dt, na.rm = TRUE)
max(d9$dt, na.rm = TRUE)
min(d9$dt, na.rm = TRUE)

#The mean is 23.51; the median is 8. The minimum is 1 and the maximum is 10759 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d9$dt, ylab = "Time difference", na.rm = TRUE, main="Monday_1 time difference distribution")

#As seen in other days, the time difference distribution looks constant save for one data point

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d9)){
  if(d9$dt[i]>119){
    print(d9$dt[i])
    print(i)
    print("---")
  }
}

#One period of non-activity on Monday(2020-08-24)
#d9$dt[362] where there is a break of 10759 seconds

print("time difference:")
d9$dt[362]
d9$dt[362]/3600
print("start break:")
d9$time[362]
print("end break:")
d9$time[363]
print("start longitude/latitude:")
d9$longitude[362]
d9$latitude[362]
print("end longitude/latitude:")
d9$longitude[363]
d9$latitude[363]
print("distance between points:")
d9$dist[362]
print("speed at break:")
d9$speed[362]

#The large break at d9$dt[362] was 10759 seconds long (2.99 hours)
#It started at 19:57:30 and ended at 22:56:49
#The start point of the break was longitude: -113.98598544, latitude: 46.8596389
#Google maps show sthis is University District, Missoula, MT 59812, USA (his spot)
#The end point was longitude: -113.98519928, latitude: 46.85980264
#Google maps also shows this as University District, Missoula, MT 59812, USA (again his spot, he didn't move much)
#The distance between these points (using utm-coordinates) is 62.67 meters
#The speed between these points (using utm-coordinates) is 0.0006 m/s 
#By looking at Google Maps I can tell that Wayne stayed in the park in the vecinity of Grizzly Statue during that time

#Plot Wayne's trajectory on Monday_1 with this break
plot(d9$longitude, d9$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Monday_1 path with breaks", col="blue")

#Red lines mark the starting point of the break
abline(v=d9$longitude[362], col="red")
abline(h=d9$latitude[362], col="red")
#Pink lines mark the end point of the break
abline(v=d9$longitude[363], col="pink")
abline(h=d9$latitude[363], col="pink")
#Green lines mark the start of his journey
abline(v=d9$longitude[1], col="green")
abline(h=d9$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d9$longitude[nrow(d9)], col="darkgreen")
abline(h=d9$latitude[nrow(d9)], col="darkgreen")

#The plot shows that the farthest Wayne went from his starting point was the start of the break 
#i.e. the University District of Montana, park near Grizzly Statue (his spot)
#His starting point is close to the end point
#And the break point (start and end) are close by, he didn't move much
#WAyne took different paths on the way to his spot and on the way back home

#Check for outliers in speed 
#These may be errors
#Or Wayne took a vehicle 
mean(d9$speed, na.rm = TRUE)
median(d9$speed, na.rm = TRUE)
max(d9$speed, na.rm = TRUE)
min(d9$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 5.89 m/s (clearly there are outliers)
#The median is 2.67 m/s (maybe Wayne jogged?)
#The maximum speed is 65.81 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d9$speed, ylab = "Speed", main="Monday_1 speed distribution", na.rm = TRUE)

#The speed distribution plot shows that there does seem to be a trend in speed
#The trend is not as defined because there are outliers
# there are MANY outliers

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

head(d10)

#Map Wayne's trajectory on Monday_2
plot(d10$longitude, d10$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Monday_2 path", col="blue")
#Green lines mark the start of his journey
abline(v=d10$longitude[1], col="green")
abline(h=d10$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d10$longitude[nrow(d10)], col="darkgreen")
abline(h=d10$latitude[nrow(d10)], col="darkgreen")

#When and where did Wayne start/end his journey on Monday, 2020-08-31
#How many data points do we have for this day?
print("start:")
d10$time[1]
d10$longitude[1]
d10$latitude[1]

print("end:")
d10$time[nrow(d10)]
d10$longitude[nrow(d10)]
d10$latitude[nrow(d10)]

print(" num data points:")
nrow(d10)
print("time seconds:")
sum(d10$dt, na.rm = TRUE)
print("time hours:")
sum(d10$dt, na.rm = TRUE)/3600

#On Monday (2020-08-31), 
#Wayne started his journey at 17:53:35, a little over an hour before the previous Monday (19:11:14)
#He started at longitude= -114.0002755, latitude= 46.88748749
#Google Maps shows this as 1600-1698 Holmes St, Missoula, MT 59802, USA (close to his usual starting points)
#He ended his journey at 22:48:55, a little less than an hour before the previous Monday (23:38:49)
#The end of his journey was at longitude= -114.00011585, latitude= 46.88740023
#Google Maps shows this as 1600-1698 Holmes St, Missoula, MT 59802, USA (very very close to his starting point)

#There are 867 recorded time records during his journey of 17720 seconds (4.92 hours)

#Understand the changes in time 
#Find the activity sessions within his journey
#Did Wayne take a break this Monday as last one?

mean(d10$dt, na.rm = TRUE)
median(d10$dt, na.rm = TRUE)
max(d10$dt, na.rm = TRUE)
min(d10$dt, na.rm = TRUE)

#The mean is 20.46; the median is 9. The minimum is 0 and the maximum is 4206 
#This means there were moments when Wayne stopped in his journey

#Time difference  distribution
plot(d10$dt, ylab = "Time difference", na.rm = TRUE, main="Monday_2 time difference distribution")

#The time difference distribution looks mostly constant
#Unlike other days there are more outliers 
#This means Wayne took more breaks in his journey 

#Loop to find periods of non activity 
#i.e. when Wayne was not walking for more than a minute
for(i in 1:nrow(d10)){
  if(d10$dt[i]>119){
    print(d10$dt[i])
    print(i)
    print("---")
  }
}

#There are 3 periods of non-activity on Monday(2020-08-31)
#The first is at d10$dt[362]
#The second at d10$dt[528]
#The third at d10$dt[538]

#For the first is at d10$dt[362]
print("time difference:")
d10$dt[362]
d10$dt[362]/3600
print("start break:")
d10$time[362]
print("end break:")
d10$time[363]
print("start longitude/latitude:")
d10$longitude[362]
d10$latitude[362]
print("end longitude/latitude:")
d10$longitude[363]
d10$latitude[363]
print("distance between points:")
d10$dist[362]
print("speed at break:")
d10$speed[362]

#The first break at d10$dt[362] was 1545 seconds long (0.43 hours)
#It started at 18:39:39 and ended at 19:05:24
#The starting point of the break was longitude= -113.9843389, latitude= 46.85979654
#Google maps shows this as University District, Missoula, MT 59812, USA (his usual resting spot)
#The end point was longitude= -113.98510751, latitude= 46.86002048
#Google maps shows this as 32 Campus Dr, Missoula, MT 59812, United States
#That is more towards the center of the park near Grizzly Statue (still his spot)
#The distance between these points (using utm-coordinates) is 63.69 meters
#The speed between these points (using utm-coordinates) is 0.04 m/s 

#For the second break, it is at d10$dt[528]
print("time difference:")
d10$dt[528]
d10$dt[528]/3600
print("start break:")
d10$time[528]
print("end break:")
d10$time[529]
print("start longitude/latitude:")
d10$longitude[528]
d10$latitude[528]
print("end longitude/latitude:")
d10$longitude[529]
d10$latitude[529]
print("distance between points:")
d10$dist[528]
print("speed at break:")
d10$speed[528]

#The second break at d10$dt[528] was 3406 seconds long (0.95 hours)
#It started at 19:56:15 (about the same time as his break last Monday) and ended at 20:53:01
#The starting point of the break was longitude= -113.984562, latitude= 46.8612473
#Google maps shows this as Anderson Hall, 32 Campus Dr, Missoula, MT 59812, USA
#This is a building not very far from his usual spot
#The end point was longitude= -113.9845723, latitude= 46.861243
#Google maps shows this as still Anderson Hall, 32 Campus Dr, Missoula, MT 59812, USA
#The distance between these points (using utm-coordinates) is 0.92 meters (barely any distance)
#The speed between these points (using utm-coordinates) is 0.0003 m/s 

#For the third break, it is at d10$dt[538]
print("time difference:")
d10$dt[538]
d10$dt[538]/3600
print("start break:")
d10$time[538]
print("end break:")
d10$time[539]
print("start longitude/latitude:")
d10$longitude[538]
d10$latitude[538]
print("end longitude/latitude:")
d10$longitude[539]
d10$latitude[539]
print("distance between points:")
d10$dist[538]
print("speed at break:")
d10$speed[538]

#The third break at d10$dt[538] was 4206 seconds long (1.17 hours)
#It started at 20:56:01 and ended at 22:06:07
#The starting point of the break was longitude= -113.9845684, latitude= 46.8612421
#Google maps shows this as Anderson Hall, 32 Campus Dr, Missoula, MT 59812, USA (oddly the same as last break)
#It could be that Wayne simply moved rooms in the same building
#The end point was longitude= -113.98523711, latitued= 46.85975196
#Google maps shows this as University District, Missoula, MT 59812, USA 
#(back to his usual spot at the park by Grizzly statue)
#The distance between these points (using utm-coordinates) is 173.37 meters 
#The speed between these points (using utm-coordinates) is 0.04 m/s 

#Plot Wayne's trajectory on Monday_2 with the three breaks
plot(d10$longitude, d10$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Monday_2 path with breaks", col="blue")

#Red lines mark the starting point of the first break
abline(v=d10$longitude[362], col="red")
abline(h=d10$latitude[362], col="red")
#Pink lines mark the end point of the first break
abline(v=d10$longitude[363], col="pink")
abline(h=d10$latitude[363], col="pink")
#Orange lines mark the starting point of the second break
abline(v=d10$longitude[528], col="orange")
abline(h=d10$latitude[528], col="orange")
#Brown lines mark the end point of the second break
abline(v=d10$longitude[529], col="brown")
abline(h=d10$latitude[529], col="brown")
#Gold lines mark the starting point of the third break
abline(v=d10$longitude[538], col="gold")
abline(h=d10$latitude[538], col="gold")
#Dark Gold lines mark the end point of the third break
abline(v=d10$longitude[539], col="gold3")
abline(h=d10$latitude[539], col="gold3")
#Green lines mark the start of his journey
abline(v=d10$longitude[1], col="green")
abline(h=d10$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d10$longitude[nrow(d10)], col="darkgreen")
abline(h=d10$latitude[nrow(d10)], col="darkgreen")

#The plot shows that the starting and ending pointare in close proximity
#The path Wayne chose on his way back is different than the one on his way out
#This Monday, Wayne took several breaks
#They were all wround the same area in the University District of Minesota

#Check for outliers in speed 
#These may be errors
mean(d10$speed, na.rm = TRUE)
median(d10$speed, na.rm = TRUE)
max(d10$speed, na.rm = TRUE)
min(d10$speed, na.rm = TRUE)

#We know the average walking speed of an adult is 1.4 m/s
#The mean speed of this day was 6.8 m/s 
#The median is 1.86 m/s 
#The maximum speed is 128.01 m/s, so either he took a vehicle or it is an error
#The minimum speed is 0 m/s (when wayne was still)

#Speed distribution
plot(d10$speed, ylab = "Speed", main="Monday_2 speed distribution", na.rm = TRUE)

#This speed distribution plot is very interesting
#Overall there is trend with speed
#But a bit before 400 and before 600 in the data points the speed looks constant
#There are no outliers in this section

#Check index of datapoints with speed under 0.05
for(i in 380:580){
    print(d10$speed[i])
}

d10$speed[384]
d10$dt[384]
d10$dist[384]

#This shows that during that time there might have been several brak points as seen before
#But overall he was very very still (hardly moving)

#Modnay_1 vs Monday_2
#Plot both Mondays in same graph
#Mark their start/end points

#Map Wayne's trajectory on Monday_1
plot(d9$longitude, d9$latitude, xlab="longitude", ylab="longitude", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     main="Monday_1 vs Monday_2 ", col="blue")
#Green lines mark the start of his journey
abline(v=d9$longitude[1], col="green")
abline(h=d9$latitude[1], col="green")
#Dark green lines amrk the end of his journey
abline(v=d9$longitude[nrow(d9)], col="darkgreen")
abline(h=d9$latitude[nrow(d9)], col="darkgreen")

par(new=TRUE)

#Map Wayne's trajectory on Monday_2
plot(d10$longitude, d10$latitude, axes = FALSE, xlab="", ylab="",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),
     col="lightblue")
#Green lines mark the start of his journey
abline(v=d10$longitude[1], col="magenta")
abline(h=d10$latitude[1], col="magenta")
#Dark green lines amrk the end of his journey
abline(v=d10$longitude[nrow(d10)], col="purple")
abline(h=d10$latitude[nrow(d10)], col="purple")

#The start/end point of both Mondays are very close by
#And the paths chosen on those days seem to coincide
#We need to run a smoother to verify

#join data in dummy set
d <- rbind(d1, d3, d5, d7, d9, d2, d4, d6, d8, d10)

#chronological order 
d <-d[order(d$time),] 
rownames(d) <- 1:nrow(d)

#Plot all days (without wednesdays)
#Mondays will be red
plot(d$longitude[d$day=="Monday"], d$latitude[d$day=="Monday"],
     xlab="longitude", ylab="latitude", main="All paths (sans Wednesday)",
     col="red")
par(new=TRUE)
#Tuesdays will be blue
plot(d$longitude[d$day=="Tuesday"], d$latitude[d$day=="Tuesday"],
     axes = FALSE, xlab = "", ylab = "", 
     col="blue")
par(new=TRUE)
#Thursdays will be orange
plot(d$longitude[d$day=="Thursday"], d$latitude[d$day=="Thursday"],
     axes = FALSE, xlab = "", ylab = "", 
      col="orange")
par(new=TRUE)
#Fridays will be purle
plot(d$longitude[d$day=="Friday"], d$latitude[df$day=="Friday"],
     axes = FALSE, xlab = "", ylab = "", 
     col="purple")

#Plot all days from home to Wayne's spot by the University
#Mondays will be red
plot(head(d$longitude[d$day=="Monday_1"],362), head(d$latitude[d$day=="Monday_1"],362),
     xlab="longitude", ylab="latitude", main="On the way to the spot",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="red")
par(new=TRUE)
plot(head(d$longitude[d$num=="Monday_2"],362), head(d$latitude[d$num=="Monday_2"],362),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="red")
par(new=TRUE)

#Tuesdays will be blue
plot(head(d$longitude[d$num=="Tuesday_1"], 80), head(d$latitude[d$num=="Tuesday_1"], 80),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="blue")
par(new=TRUE)
plot(head(d$longitude[d$num=="Tuesday_2"], 359), head(d$latitude[d$num=="Tuesday_2"], 359),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="blue")
par(new=TRUE)

#Wednesdays will be green
plot(head(d$longitude[d$num=="Wednesday_1"], 123), head(d$latitude[df$num=="Wednesday_1"], 123),
     axes = FALSE, xlab = "", ylab = "",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="green")
par(new=TRUE)
plot(head(d$longitude[d$num=="Wednesday_2"], 422), head(d$latitude[df$num=="Wednesday_2"], 422),
     axes = FALSE, xlab = "", ylab = "",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="green")
par(new=TRUE)

#Thursdays will be orange
plot(head(d$longitude[d$num=="Thursday_1"], 225), head(d$latitude[d$num=="Thursday_1"], 225),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="orange")
par(new=TRUE)
plot(head(d$longitude[d$num=="Thursday_2"], 363), head(d$latitude[d$num=="Thursday_2"], 363),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="orange")
par(new=TRUE)

#Fridays will be purle
plot(head(d$longitude[d$num=="Friday_1"], 328), head(d$latitude[df$num=="Friday_1"], 328),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="purple")
par(new=TRUE)
plot(head(d$longitude[d$num=="Friday_2"], 328), head(d$latitude[df$num=="Friday_2"], 328),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),, col="purple")


#Plot all days on the way back home
#Mondays will be red
plot(tail(d$longitude[d$day=="Monday_1"],363), tail(d$latitude[d$day=="Monday_1"],363),
     xlab="longitude", ylab="latitude", main="On the way to the spot",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="red")
par(new=TRUE)
plot(tail(d$longitude[d$num=="Monday_2"],538), tail(d$latitude[d$num=="Monday_2"],538),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="red")
par(new=TRUE)

#Tuesdays will be blue
plot(tail(d$longitude[d$num=="Tuesday_1"], 81), tail(d$latitude[d$num=="Tuesday_1"], 81),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="blue")
par(new=TRUE)
plot(tail(d$longitude[d$num=="Tuesday_2"], 360), tail(d$latitude[d$num=="Tuesday_2"], 360),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="blue")
par(new=TRUE)

#Wednesdays will be green
plot(tail(d$longitude[d$num=="Wednesday_1"], 124), tail(d$latitude[df$num=="Wednesday_1"], 124),
     axes = FALSE, xlab = "", ylab = "",
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="green")
par(new=TRUE)

#Thursdays will be orange
plot(tail(d$longitude[d$num=="Thursday_1"], 226), tail(d$latitude[d$num=="Thursday_1"], 226),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="orange")
par(new=TRUE)
plot(tail(d$longitude[d$num=="Thursday_2"], 364), tail(d$latitude[d$num=="Thursday_2"], 364),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="orange")
par(new=TRUE)

#Fridays will be purle
plot(tail(d$longitude[d$num=="Friday_1"], 329), tail(d$latitude[df$num=="Friday_1"], 329),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887), col="purple")
par(new=TRUE)
plot(tail(d$longitude[d$num=="Friday_2"], 329), tail(d$latitude[df$num=="Friday_2"], 329),
     axes = FALSE, xlab = "", ylab = "", 
     xlim=c(-114.001, -113.984), ylim=c(46.857, 46.887),, col="purple")


library("lubridate")

#join data in dummy set (only path from home to Wayne's spot)
d <- rbind(head(d1, 80), head(d3, 123), head(d5, 225), head(d7, 328),
           head(d9, 362), head(d2, 359), head(d4, 422), head(d6, 363),
           head(d8, 328), head(d10, 362))

#chronological order 
d <-d[order(d$time),] 
rownames(d) <- 1:nrow(d)

head(d)

x_1 <- as.POSIXct(d$time[1])
x_2 <-floor_date(x_1, "hour")
x_3 <-strftime(x_2, "%Y-%m-%d %H:%M:%S", tz = "")
h_time<-c(x_3)

for(i in 2:nrow(d)){
    x_1 <- as.POSIXct(d$time[i])
    x_2 <-floor_date(x_1, "hour")
    x_3 <-strftime(x_2, "%Y-%m-%d %H:%M:%S", tz = "")
    h_time<-c(h_time, x_3)
}

d<-cbind(d, h_time)

head(d)

temps<-read.csv("temps.csv")
temps = subset(temps, select = -c(X,X.1,X.2) )

head(temps)

d <- merge(temps,d,by="h_time")

head(d)
