library("jsonlite")
library("chron")
library("dlm")
library("sp")
library("rgdal")
library("dplyr")

#Algorithm: 

bomb_Wayne <-function(dataframe_geojson){
        
    data <-read_json(dataframe_geojson)

    data <- (data[2])$features

    df <- data.frame(cbind("time" = data[[1]]$properties$time,
                            "longitude" = data[[1]]$geometry$coordinates[[1]]),
                      "latitude" = data[[1]]$geometry$coordinates[[2]], 
                      stringsAsFactors=FALSE)

    for(i in 2:length(data)){
      ab <- data.frame(cbind("time" = data[[i]]$properties$time,
                             "longitude" = data[[i]]$geometry$coordinates[[1]]),
                       "latitude" = data[[i]]$geometry$coordinates[[2]],
                       stringsAsFactors=FALSE)

      df <- rbind(df, ab)
    }

    #df<-head(df, x)

    #Since longitude has to be a number we need to convert it 
    df$longitude <- as.numeric(df$longitude)

    #Make sure the points are in chronological order 
    df <-df[order(df$time),] 
    rownames(df) <- 1:nrow(df)

    #Now transform time to chron
    for(i in 1:nrow(df)){
      a <-substr(df$time[i], start=1, stop=10)
      b <-substr(df$time[i], start=12, stop=19)
      c <- paste(a, b, sep=" ")
      as.chron(c)
      df$time[i] = c
    }

    #time since start of journey
    pth_time <-c(as.numeric(difftime(df$time[1], df$time[1], units = "secs")))

    for(i in 2:nrow(df)){
      pth_time <- c(pth_time, as.numeric(difftime(df$time[i], df$time[1], units = "secs")))
    }

    df<-cbind(df, pth_time)
    names(df)[4] <- "pth_time"
    
    print("The bombing site is at longitude = ")
    print(-113.9963)
    print("And latitude = ")
    print(46.87683)
   
    #Set origin longitude and latitude (starting point)
    origin_long<-df$longitude[1]
    origin_lat<-df$latitude[1]
   
    #difference of origin to point (of index x) closest to bombing site
    lon<-(-113.9963-origin_long)
    lat<-(46.87683-origin_lat)

    #Find predicted path time to that point given origin
    pred_pth_time<-12.35 -10936.33*lon -106568.46*lat
    
    print("The predicted path time to the bombing site is:")
    print(pred_pth_time)
    
    #Add predicted path time to starting time
    
    a <-substr(df$time[1], start=1, stop=10)
    b <-substr(df$time[1], start=12, stop=19)
    
    c<-as.POSIXct(paste(a, b), format = "%Y-%m-%d %H:%M:%S")
    d<-c + pred_pth_time
    
    print("Wayne will arrive to the bombing location at:")
    print(d)
}

#Test with test data from the third week: 

Tues_2020_09_01 <- '20200901112100.geojson'
Wed_2020_09_02<- '20200902125611.geojson'
Thur_2020_09_03<- '20200903110618.geojson'
Tues_2020_09_08<- '20200908081420.geojson'
Thur_2020_09_10<- '20200910070926.geojson'
Mon_2020_09_14<- '20200914101156.geojson'

bomb_Wayne(Tues_2020_09_01)

bomb_Wayne(Wed_2020_09_02)

bomb_Wayne(Thur_2020_09_03)

bomb_Wayne(Tues_2020_09_08)

bomb_Wayne(Thur_2020_09_10)

bomb_Wayne(Mon_2020_09_14)
