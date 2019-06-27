#CLEAR R ENVIOURNMENT
rm(list=ls())

#LOAD REQUIRED PACKAGES
library(lubridate)
library(Hmisc)
library(dplyr)
library(DMwR)
library(png)
library(grid)
library(ggplot2)
library(geosphere)
library(magrittr)
library(tidyverse)
library(Hmisc)
library(exploratory)

#SET WORKING DIRECTORY
setwd("D:/DATA SCIENCE/COURSE/Projects/Project_Cab_Fare_Prediction")

#LOAD TRAIN AND TEST DATA
train=read.csv("train_cab.csv",header=T)
test=read.csv("test.csv",header=T)

#CHECK DIMENSIONS OF TRAIN DATA
dim(train)

#CHECK DATA TYPES OF VARIABLES IN TRAIN DATA
str(train)
#CHECK MISSING VALUES BEFORE TYPE CONVERSION 
sum(is.na(train))
#CHANGE REQUIRED TYPES
train$fare_amount=as.numeric(as.character(train$fare_amount))#CHANGE fare_amount TO NUMERIC
train$pickup_datetime=lubridate::ymd_hms(train$pickup_datetime, tz="UTC")#CHANGE pickup_datetime TO POXIct WITH TIME ZONE UTC

###NOTE###
#AFTER CHANGING VARIABLE DATA TYPES SOME MISFORMATTED VALUES WILL BE INTRODUCED AS MISIING VALUES.
#TREAT THEM AS MISSING VALUES BECAUSE THEY ARE MISFORMATTED.

#MISSING VALUE ANALYSIS
#CREATING SEPERATE DATA FRAME FOR MISSING VALUE % FOR EACH VARIABLE
missing_value=data.frame(apply(train,2,function(x){sum(is.na(x))}))
names(missing_value)[1]="Missing_percentage"
missing_value$Missing_percentage=(missing_value$Missing_percentage/nrow(train))*100
View(missing_value)
#TREATING MISSING VALUES ACCORDING TO THEIR % AND IMPORTANCE IN ANALYSIS
#DELETE MISSING VALUES FOR pickup_datetime. BECAUSE IT IS INTRODUCED MY TYPE CONVERSION OF MISFORMATTED VALUE
completeVec=complete.cases(train[, "pickup_datetime"])
train=train[completeVec,]
#IMPUTE MISSING VALUES FOR passenger_count WITH MEDIAN.
#HOWEVER WE WILL SEE IN FURTHER ANALYSIS THAT passenger_count IS NOT IMPORTANT VARIABLE
train$passenger_count[is.na(train$passenger_count)] = median(train$passenger_count,na.rm = T)
#IMPUTING MISSING VALUES OF fare_amount BECAUSE IT IS OUR DEPENDENT VARIABLE AND REMOVING ITS VALUES MAY BE LOSSING OF IMPORTANT INSTANCES IN DATASET
#IMPUTE WITH MEAN, MEDIAN AND KNN
actual=train$fare_amount[450]
actual#56.8
train$fare_amount[450]=NA
mean=mean(train$fare_amount,na.rm = T)
mean#15.012
median=median(train$fare_amount,na.rm = T)
median#8.5
knn=knnImputation(train[,-2],k=3)
knn$fare_amount[450]#47.3329
#AS WE CAN SEE MISSING VALUE IMPUTED BY KNN IS NEAREST TO ACTUAL VALUE HENCE WE WILL REPLACE MISSING VALUES WITH KNN.
train$fare_amount[450]=actual#REPLACE WITH ACTUAL VALUE
for (i in 1:16067){
 train$fare_amount[is.na(train$fare_amount)] = knn$fare_amount[i]
}
sum(is.na(train))#NO MISSING VALUE 

#OUTLIER ANALYSIS AND FEATURE EXTRACTION
#OUTLIER ANALYSIS IS NOT DONE WITH BOXPLOT METHOD.
#BECAUSE THIS WILL REMOVE IMPORTANT INSTANCES IN DATA.
#CONFINE THE LOCATION DATA OF train ACCORDING TO EXTREME LOCATIONS OF TEST DATA
minmax_longitudes=c(min(min(test[,2]),min(test[,4])),max(max(test[,2]),max(test[,4])))
minmax_lattitudes=c(min(min(test[,3]),min(test[,5])),max(max(test[,3]),max(test[,5])))
minmax_longitudes
minmax_lattitudes

#DEFINE BOUNDING BOX FOR train 
BOX=c(minmax_longitudes[1],minmax_longitudes[2],minmax_lattitudes[1],minmax_lattitudes[2])
#SELECT train DATA WITHIN BOUNDING BOX
train=train[train$pickup_longitude>minmax_longitudes[1]&train$pickup_longitude<minmax_longitudes[2]
            &train$dropoff_longitude>minmax_longitudes[1]&train$dropoff_longitude<minmax_longitudes[2]
            &train$dropoff_latitude>minmax_lattitudes[1]&train$dropoff_latitude<minmax_lattitudes[2]
            &train$pickup_latitude>minmax_lattitudes[1]&train$pickup_latitude<minmax_lattitudes[2],]

#SELECT NON NEGATIVE fare_amount
train=train[train$fare_amount>=0,]

#IDENTIFY CITY. 
#CALCULATE MEAN LOCATIONS FOR test DATA AND LOCATE CITY ON THESE MEAN COORDINATES.
mean_test_1=c(mean(test$pickup_longitude),mean(test$pickup_latitude))
mean_test_1
#CITY IS NEW YORK. WE ARE GOING TO PREDICT TRIP AMOUNT FOR NEW YORK.

#DOWNLOADING CITY MAP OF NEW YORK AS PNG FILE
myurl <- "https://aiblog.nl/download/nyc_-74.3_-73.7_40.5_40.9.png"
z <- tempfile()
download.file(myurl,z,mode="wb")
pic <- readPNG(z)
rg <- grid::rasterGrob(pic, width=unit(1,"npc"), height=unit(1,"npc"))
data=dplyr::select(train,c(pickup_longitude))
data=cbind(data,train$pickup_latitude)
colnames(data) <- c('Longitude', 'Latitude')
BB = c(-74.3, -73.7, 40.5, 40.9)#BOUNDING BOX FOR MAP
#PLOTTING PICKUP LOCATIONS ON CITY MAP
ggplot(data, aes(Longitude,Latitude)) + 
  annotation_custom(rg) +
  geom_point(colour="blue") +
  scale_x_continuous(expand=c(0,0), lim=c(BB[1],BB[2])) +
  scale_y_continuous(expand=c(0,0), lim=c(BB[3],BB[4])) +
  theme_void() +
  theme(aspect.ratio = nrow(pic)/ncol(pic))
#ZOOMIN PLOT 

#PLOTTING DROPOFF LOCATIONS ON CITY MAP
data_1=dplyr::select(train,c(dropoff_longitude))
data_1=cbind(data,train$dropoff_latitude)
colnames(data_1) <- c('Longitude', 'Latitude')
ggplot(data_1, aes(Longitude,Latitude)) + 
  annotation_custom(rg) +
  geom_point(colour="green") +
  scale_x_continuous(expand=c(0,0), lim=c(BB[1],BB[2])) +
  scale_y_continuous(expand=c(0,0), lim=c(BB[3],BB[4])) +
  theme_void() +
  theme(aspect.ratio = nrow(pic)/ncol(pic))

#EXTRACTING HOUR, DATE, MONTH, YEAR, DAYS OF WEEK, DAY
pickup_hour=format(train$pickup_datetime,"%H")
pickup_day=format(train$pickup_datetime,"%d")
pickup_month=format(train$pickup_datetime,"%m")
pickup_year=format(train$pickup_datetime,"%Y")
pickup_day_of_week=weekdays(as.Date(train$pickup_datetime))
pickup_date=format(as.Date(train$pickup_datetime))

#COMBINE WITH trian DATA
train=cbind(train,pickup_date)
train=cbind(train,pickup_hour)
train=cbind(train,pickup_day)
train=cbind(train,pickup_month)
train=cbind(train,pickup_year)
train=cbind(train,pickup_day_of_week)

#CHECK FOR VARIABLE DATA TYPES
str(train)

#CHANGE TYPES FOR REQUIRED VARIABLES 
train$pickup_hour=as.numeric(as.character(train$pickup_hour))
train$pickup_day=as.numeric(as.character(train$pickup_day))
train$pickup_month=as.numeric(as.character(train$pickup_month))
train$pickup_year=as.numeric(as.character(train$pickup_year))
train$pickup_day_of_week=as.character(train$pickup_day_of_week)
train$pickup_date=as.Date(train$pickup_date)

#CALCULATING TRIP DISTANCE IN km.
train = train %>% 
           mutate(distance_km = by(train, 1:nrow(train), function(row) { 
             distHaversine(c(row$pickup_longitude, row$pickup_latitude),c(row$dropoff_longitude, row$dropoff_latitude) ,r=6371.137)  
             }))

#density plot for pickup locations        
ggplot(data, aes(Longitude,Latitude) ) +
  annotation_custom(rg)+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0),lim=c(BB[1],BB[2])) +
  scale_y_continuous(expand = c(0, 0),lim=c(BB[3],BB[4])) +
  theme(
    aspect.ratio = nrow(pic)/ncol(pic)
  )
#density plot for dropoff locations        
ggplot(data_1, aes(Longitude,Latitude) ) +
  annotation_custom(rg)+
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0),lim=c(BB[1],BB[2])) +
  scale_y_continuous(expand = c(0, 0),lim=c(BB[3],BB[4])) +
  theme(
    legend.position='none'
  )

#CHECKING FOR AIRPORT RIDES
#3 MAIN AIRPORTS IN/NEAR NEW YORK CITY. JFK, LA GAURDIA, NEWARK.
#LOCATIONS FROM GOOGLE MAP.
jfk = c(-73.7822222222, 40.6441666667)#JFK
nyc = c(-74.0063889, 40.7141667)#NEW YORK CITY CENTER 
lgr = c(-73.8719444444,40.7747222222)#LA GAURDIA 
ewr = c(-74.175, 40.69)#NEWARK 

#FUNCTION FOR GEOGRAPHICAL DISTANCE
get_geo_distance = function(long1, lat1, long2, lat2, units = "km") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list_extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  distance
}

#PICKUP DISTANCES FOR AIRPORTS
pickup_distance_jfk= get_geo_distance(train$pickup_longitude,train$pickup_latitude,jfk[1],jfk[2],"km") 
pickup_distance_lgr= get_geo_distance(train$pickup_longitude,train$pickup_latitude,lgr[1],lgr[2],"km") 
pickup_distance_ewr= get_geo_distance(train$pickup_longitude,train$pickup_latitude,ewr[1],ewr[2],"km") 

#DROPOFF DISTANCES FOR AIRPORTS
dropoff_distance_jfk= get_geo_distance(train$dropoff_longitude,train$dropoff_latitude,jfk[1],jfk[2],"km") 
dropoff_distance_lgr= get_geo_distance(train$dropoff_longitude,train$dropoff_latitude,lgr[1],lgr[2],"km") 
dropoff_distance_ewr= get_geo_distance(train$dropoff_longitude,train$dropoff_latitude,ewr[1],ewr[2],"km") 

#CREATING DATA FRAME FOR DISTANCES FROM ALL AIRPORTS WITH THEIR TRIP AMOUNT 
all_rides=data.frame(matrix(ncol=7,nrow=nrow(train)))
colnames(all_rides)=c('fare_amount','pickup_distance_jfk','pickup_distance_lgr','pickup_distance_ewr','dropoff_distance_jfk','dropoff_distance_lgr','dropoff_distance_ewr')
all_rides$fare_amount=train$fare_amount
all_rides$pickup_distance_jfk=pickup_distance_jfk
all_rides$pickup_distance_lgr=pickup_distance_lgr
all_rides$pickup_distance_ewr=pickup_distance_ewr
all_rides$dropoff_distance_jfk=dropoff_distance_jfk
all_rides$dropoff_distance_lgr=dropoff_distance_lgr
all_rides$dropoff_distance_ewr=dropoff_distance_ewr

#ALL AIRPORT RIDES WITHIN 2.5km OF AIRPORT 
jfk_pickup_airport_rides=all_rides[(all_rides$pickup_distance_jfk<2.5),c(1,2)]
lgr_pickup_airport_rides=all_rides[(all_rides$pickup_distance_lgr<2.5),c(1,3)]
ewr_pickup_airport_rides=all_rides[(all_rides$pickup_distance_ewr<2.5),c(1,4)]

jfk_dropoff_airport_rides=all_rides[(all_rides$dropoff_distance_jfk<2.5),c(1,5)]
lgr_dropoff_airport_rides=all_rides[(all_rides$dropoff_distance_lgr<2.5),c(1,6)]
ewr_dropoff_airport_rides=all_rides[(all_rides$dropoff_distance_ewr<2.5),c(1,7)]

#PLOTS OF FARE FOR AIRPORT RIDES
hist(jfk_pickup_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram pickup location within 2.5 km of jfk airport")
hist(lgr_pickup_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram pickup location within 2.5 km of lgr airport")
hist(ewr_pickup_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram pickup location within 2.5 km of ewr airport")

hist(jfk_dropoff_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram dropoff location within 2.5 km of jfk airport")
hist(lgr_dropoff_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram dropoff location within 2.5 km of lgr airport")
hist(ewr_dropoff_airport_rides$fare_amount, breaks=100, col="blue", xlab="fare$USD", main="Histogram dropoff location within 2.5 km of ewr airport")

#OBSERVING PLOTS WE CAN SAY THAT FARE FOR AIRPORT RIDES IS HIGHER AND GENERALLY FIXED.
#THIS CONFIRMS OUR HYPOTHESIS

#CREATING VARIABLE WITH LOGICAL DATA FOR AIRPORT RIDES FOR ALL 3 AIRPORTS 
train=cbind(train,(all_rides$pickup_distance_jfk<2.5))
train=cbind(train,(all_rides$pickup_distance_lgr<2.5))
train=cbind(train,(all_rides$pickup_distance_ewr<2.5))
names(train)[15]=c("is_pickup_near_jfk")
names(train)[16]=c("is_pickup_near_lgr")
names(train)[17]=c("is_pickup_near_newark")

train=cbind(train,(all_rides$dropoff_distance_jfk<2.5))
train=cbind(train,(all_rides$dropoff_distance_lgr<2.5))
train=cbind(train,(all_rides$dropoff_distance_ewr<2.5))
names(train)[18]=c("is_dropoff_near_jfk")
names(train)[19]=c("is_dropoff_near_lgr")
names(train)[20]=c("is_dropoff_near_newark")

#PLOT TRIP DISTANCE V/S FARE AMOUNT
plot(train$distance_km, train$fare_amount, main = "Trip distance v/s Fare amount",
     xlab = "Trip Distance", ylab = "Fare Amount",
     pch = 19, frame = FALSE)

#closly observing on above plot we observed that some values of fare amount are more than 100$USD for non airport rides this is only possible
#for airport rides in New York. But If they are non-Airport rides than either we have to delete them or impute depending on their importance and no. of such trips
#checking wether they are airport trips.

non_Airport_rides=train[(train$is_dropoff_near_jfk==F)&(train$is_dropoff_near_lgr==F)&(train$is_dropoff_near_newark==F)&(train$is_pickup_near_jfk==F)&(train$is_pickup_near_lgr==F)&(train$is_pickup_near_newark==F),]

#PLOT FOR TRIP DISTANCE V/S FARE AMOUNT EXCLUDING AIRPORT RIDES
plot(non_Airport_rides$distance_km, non_Airport_rides$fare_amount, main = "Trip distance v/s Fare amount (excluding airport rides)",
     xlab = "Trip Distance", ylab = "Fare Amount",
     pch = 19, frame = FALSE)

#looking on plot of non airport rides, we observed that these high fare rides are city rides and in New York City fare_amount>100$USD is not possible
#because in city fare rides are quite low than airport rides

abnormal_non_airport_rides=non_Airport_rides[non_Airport_rides$fare_amount>100,c(1,14)]
View(abnormal_non_airport_rides)

#these non airport rides with abnormal fares for normal distance should be removed because they are only six such rides and fare_amount for these normal distance rides can be calculated with other available distances
#hence these rides are very less and not important. This clearly states that these are outliers and I decided to remove them.
train[rownames(abnormal_non_airport_rides),c(1)]=NA
completeVec=complete.cases(train[, "fare_amount"])
train=train[completeVec,]
non_Airport_rides[rownames(abnormal_non_airport_rides),c(1)]=NA
completeVec=complete.cases(non_Airport_rides[, "fare_amount"])
non_Airport_rides=non_Airport_rides[completeVec,]

#TRIP DISTANCE V/S FARE AMOUNT OF NON AIRPORT RIDES AFTER RWEMOVING ABNORMAL VALUES
plot(non_Airport_rides$distance_km, non_Airport_rides$fare_amount, main = "Trip distance v/s Fare amount (excluding airport rides)",
     xlab = "Trip Distance", ylab = "Fare Amount",
     pch = 19, frame = FALSE)

#TRIP DISTANCE V/S FARE AMOUNT OF ALL RIDES AFTER RWEMOVING ABNORMAL VALUES
plot(train$distance_km, train$fare_amount, main = "Trip distance v/s Fare amount",
     xlab = "Trip Distance", ylab = "Fare Amount",
     pch = 19, frame = FALSE)

#OBSERVING PLOTS WE OBSERVE SOME RIDES WITH HIGHER DISTANCE AND VERY LESS FARES.
non_airport_discount_rides=train[(train$fare_amount<20 & train$distance_km>80),]

#clearly from above two plots we can observe that there are airport some rides with higher and almost same fare_amount
#also there are some non airport rides with trip distance>80km and having very less fare_amount
#these are probably discounted trips by company. In Our hypothesis set we donot make any assumption about discount scheme.
#still Iam observing for other factors like special place, month and special occations these points are very less that a firm result cannot be made.  
#concluding to remove them from data. 
#remove rides with distance_km>80 and fare_amount<20$USD

r=rownames(train[(train$fare_amount<20 & train$distance_km>80),])
train[r,c(1)]=NA
completeVec=complete.cases(train[,])
train=train[completeVec,]

#TREATMENT OF passenger_count 
#a normal cab company has minimum 1 and maximum 6 passengers in ride.# same here.
#but according to our hypothesis set fare_amount does not depends on shared rides.
#thus it is better to remove passenger count
train$passenger_count=NULL

#CONSIDERING CITY BOROUGHS IN MIND
#5 city borughs
#borough name=c(min_lng,min_lat,max_lng,max_lat)
manhattan=c(-74.0479,40.6829,-73.9067,40.8820)
queens=c(-73.9630,40.5431,-73.7004,40.8007)
bronx=c(-73.9339,40.7855,-73.7654,40.9176)
brooklyn=c(-74.0421,40.5707,-73.8334,40.7395)
staten_island=c(-74.2558,40.4960,-74.0522,40.6490)
all_boroughs=c((manhattan),(queens),(bronx),(brooklyn),(staten_island))

borough_names= c("manhattan","queens","bronx","brooklyn","staten_island")
boroughs=data.matrix(frame=matrix(all_boroughs, byrow = TRUE, nrow = 5, ncol = 4))

#GETIING PICKUP AND DROPOFF BOROUGHS
get_borough=function(lng,lat)
{

  for(i in 1:5)
  {
     if(lng>=boroughs[i,1] & lat>=boroughs[i,2] & lng<=boroughs[i,3] & lat<=boroughs[i,4])
       
     {
       return (borough_names[i])
     }
  }

    return ("others")
  
    
}

train = train %>% 
  mutate(pickup_borough = by(train, 1:nrow(train), function(row) { get_borough(row$pickup_longitude,row$pickup_latitude)  }))

train = train %>% 
  mutate(dropoff_borough = by(train, 1:nrow(train), function(row) { get_borough(row$dropoff_longitude,row$dropoff_latitude)  }))

#DISTRIBUTION OF FREQUENCY COUNT FOR BOROUGHS

#PLOTS FOR PICKUP AND DROPOFF 
counts = table(train$pickup_borough)
barplot(counts, main="Distribution of pickup boroughs",
        xlab="boroughs", col=c("darkblue"), horiz =TRUE,
        names.arg=rownames(counts))

counts_1 = table(train$pickup_borough)
barplot(counts_1, main="Distribution of dropoff boroughs",
        xlab="boroughs", col=c("red"), horiz =TRUE,
        names.arg=rownames(counts_1))

#kdes for distribution of fare amount in pickup and dropoff boroughs
for(i in 1:5)
{
d1=density(log(train[train$pickup_borough==borough_names[i],c(1)]))
d2=density(log(train[train$dropoff_borough==borough_names[i],c(1)]))
plot(d1,h=2,main=paste("Fare amount (log scale) for ",borough_names[i]))+lines(d2)+polygon(d2,border='red')
}

#kdes for distribution of Trip Distance in pickup and dropoff boroughs
for(i in 1:5)
{
  d1=density(log(train[train$pickup_borough==borough_names[i],c(13)]))
  d2=density(log(train[train$dropoff_borough==borough_names[i],c(13)]))
  plot(d1,h=2,main=paste("Trip Distance (log scale) for ",borough_names[i]))+lines(d2)+polygon(d2,border='red')
}

#PLOTS FOR TRIP COUNTS AND AVERAGE FARE OVER YEARS, MONTHS, DAYS, WEEKDAYS, 
#no. of trips with year
counts = table(train$pickup_year)
barplot(counts, main="Distribution of number of trips with pickup year",
        xlab="pickup_year", col=c("darkblue"), horiz =TRUE,
        names.arg=rownames(counts))
#avg fare amount over years
counts=train%>%group_by(pickup_year)%>%summarise(avg_Fare=mean(fare_amount))
barplot(counts$avg_Fare,names.arg=counts$pickup_year,xlab="Year",ylab="Average Fare",col="blue",
        main="avg fare amount over years",border="red")

#no. of trips with month
counts = table(train$pickup_month)
barplot(counts, main="Distribution of number of trips with pickup month",
        xlab="pickup_month", col=c("darkblue"), horiz =TRUE,
        names.arg=rownames(counts))

#avg fare amount over month
counts=train%>%group_by(pickup_month)%>%summarise(avg_Fare=mean(fare_amount))
barplot(counts$avg_Fare,names.arg=counts$pickup_month,xlab="month",ylab="Average Fare",col="blue",
        main="avg fare amount over month",border="red")

#no. of trips with days of week
counts = table(train$pickup_day_of_week)
barplot(counts, main="Distribution of number of trips with days of week",
        xlab="pickup_day_of_week", col=c("darkblue"), horiz =TRUE,
        names.arg=rownames(counts))

#avg fare amount over DAYS OF WEEK
counts=train%>%group_by(pickup_day_of_week)%>%summarise(avg_Fare=mean(fare_amount))
barplot(counts$avg_Fare,names.arg=counts$pickup_day_of_week,xlab="day of week",ylab="Average Fare",col="blue",
        main="avg fare amount over days of week",border="red")

#no. of trips with hours
counts = table(train$pickup_hour)
barplot(counts, main="Distribution of number of trips with hours",
        xlab="pickup_hours", col=c("darkblue"), horiz =TRUE,
        names.arg=rownames(counts))

#avg fare amount over HOUR
counts=train%>%group_by(pickup_hour)%>%summarise(avg_Fare=mean(fare_amount))
barplot(counts$avg_Fare,names.arg=counts$pickup_hour,xlab="day of hour",ylab="Average Fare",col="blue",
        main="avg fare amount over hour",border="red")

#encode days of week
encodeDays=function(day_of_week)
{
  if(day_of_week=='Sunday'){return (0)}
  if(day_of_week=='Monday'){return (1)}
  if(day_of_week=='Wednesday'){return (3)}
  if(day_of_week=='Thursday'){return (4)}
  if(day_of_week=='Friday'){return (5)}
  if(day_of_week=='Tuesday'){return (2)}
  if(day_of_week=='Saturday'){return (6)}
}

df = train %>% 
  mutate(pickup_day_of_week = by(train, 1:nrow(train), function(row) { encodeDays(row$pickup_day_of_week)}))
train=df
#CHECK DIMENSIONS 
dim(train)

#save to train to csv file
write.csv(train,file="cleaned_train.csv")

#make changes to test accordingly with test data
#check for missing values
sum(is.na(test))#no missing value
#check types
str(test)
#convert pickup_datetime to PSXIct in test data
test$pickup_datetime=lubridate::ymd_hms(test$pickup_datetime, tz="UTC")
#extract information from pickup_datetime
pickup_hour=format(test$pickup_datetime,"%H")
pickup_day=format(test$pickup_datetime,"%d")
pickup_month=format(test$pickup_datetime,"%m")
pickup_year=format(test$pickup_datetime,"%Y")
pickup_day_of_week=weekdays(as.Date(test$pickup_datetime))
pickup_date=format(as.Date(test$pickup_datetime))

test=cbind(test,pickup_date)
test=cbind(test,pickup_hour)
test=cbind(test,pickup_day)
test=cbind(test,pickup_month)
test=cbind(test,pickup_year)
test=cbind(test,pickup_day_of_week)


test$pickup_hour=as.numeric(as.character(test$pickup_hour))
test$pickup_day=as.numeric(as.character(test$pickup_day))
test$pickup_month=as.numeric(as.character(test$pickup_month))
test$pickup_year=as.numeric(as.character(test$pickup_year))
test$pickup_day_of_week=as.character(test$pickup_day_of_week)
test$pickup_date=as.Date(test$pickup_date)

#calculate trip distance in km
test = test %>% 
  mutate(distance_km = by(test, 1:nrow(test), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude),c(row$dropoff_longitude, row$dropoff_latitude) ,r=6371.137)  
  }))
#calculate airport rides
get_airport_rides = function(long1, lat1, long2, lat2, units = "km") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list_extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  return (distance<2.5)
}

test = test %>% 
  mutate(is_pickup_near_jfk = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$pickup_longitude, row$pickup_latitude, jfk[1],jfk[2])  
  }))

test = test %>% 
  mutate(is_dropoff_near_jfk = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$dropoff_longitude, row$dropoff_latitude, jfk[1],jfk[2])  
  }))
test = test %>% 
  mutate(is_pickup_near_lgr = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$pickup_longitude, row$pickup_latitude, lgr[1],lgr[2])  
  }))
test = test %>% 
  mutate(is_dropoff_near_lgr = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$dropoff_longitude, row$dropoff_latitude, lgr[1],lgr[2])  
  }))
test = test %>% 
  mutate(is_pickup_near_newark = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$pickup_longitude, row$pickup_latitude, ewr[1],ewr[2])  
  }))
test = test %>% 
  mutate(is_dropoff_near_newark = by(test, 1:nrow(test), function(row) { 
    get_airport_rides(row$dropoff_longitude, row$dropoff_latitude, ewr[1],ewr[2])  
  }))
#get dropoff and pickup boroughs
test = test %>% 
  mutate(pickup_borough = by(test, 1:nrow(test), function(row) { get_borough(row$pickup_longitude,row$pickup_latitude)  }))

test = test %>% 
  mutate(dropoff_borough = by(test, 1:nrow(test), function(row) { get_borough(row$dropoff_longitude,row$dropoff_latitude)  }))

#remove passenger_count from test
test$passenger_count=NULL
dim(test)
#save to test to csv file
write.csv(test,file="cleaned_test.csv")

#Here our part one of exploratory analysis and data cleaning is completed.
#Next part will of feature engineering and data modelling.