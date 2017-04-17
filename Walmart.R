install.packages("plyr")
install.packages("lubridate")
install.packages("forecast")
library(lubridate)
library(plyr)
library(forecast)

train <- read.csv("C:/UCONN stuff/Spring 17/BI/walmart data/train.csv")
features <- read.csv("C:/UCONN stuff/Spring 17/BI/walmart data/features.csv")

#head(train)
#head(features)

#summary(train)

#sum(is.na.data.frame(train)) # no missing values

train$Store <- as.factor(train$Store) # dept and store and factors and not integers
train$Dept <- as.factor(train$Dept)


#?join

# join train data and features by store and date
trainData = join(train,features,by=c("Store","Date") ,type="inner") 

#View(df)
#?write.csv
#write.csv(file="C:/UCONN stuff/MergedData.csv",x=df)

# to confirm join
# df$Temperature[df$Store == 1 & df$Dept==1] 
# df$Temperature[df$Store == 1 & df$Dept==2] 

#summary(df)

# removing duplicated isHoliday column
trainData <- subset(trainData, select = -15 )
head(trainData)


# function to return data about any store and any dept
StoreNDept = function(DataFrame,store,dept){
  if(store==0){
    x = subset(DataFrame,DataFrame$Dept==dept)
  }
  if(dept==0){
    x = subset(DataFrame,DataFrame$Store == store)
  }else{
    t = subset(DataFrame,DataFrame$Store == store)
    x = subset(t,t$Dept==dept)
  }

  return(x)
}

# 
# findTrend = function(x){
#   if(all(diff(x$Weekly_Sales)>0)) {
#     return ("increasing")
#   }else if(all(diff(x$Weekly_Sales)<0)){
#     return ("decreasing")
#   }else return("mixed")
# }


# splits all store and depts to let them analyzed seoarately
SplitAll = function(){
  
  separatedFrame = list()
  k=1
  
  for(i in 1:45){
    for(j in 1:99){
      x = StoreNDept(trainData,i,j)
      if(nrow(x)!= 0){
        separatedFrame[[k]] = x
        k = k+1
      }
    }
  }
  return(separatedFrame)
  
}



# function to plot ACFmPACF,Trend, Seasonality, Residuals of given time series x
# x = weekly sales vector of dept/store

plotTSGraphs = function(x){
  
  # use diff() to get rid of trend part
  # from acf find q value
  # from pacf find p value
  
  # use ARIMA(p,d,q)
  decomposedSales = decompose(x)
  
  opar = par()
  
  par(bg = "white", col ="red" , mfrow = c(3,2))
  
  plot(decomposedSales$trend)
  plot(decomposedSales$seasonal)
  plot(decomposedSales$random)
  
  acf(x, lag.max = NULL,type = c("correlation", "covariance", "partial"),
      plot = TRUE, na.action = na.contiguous, demean=TRUE)
  
  Pacf(x, lag.max=NULL,plot = TRUE, na.action = na.contiguous, demean=TRUE)
  
  
  # revert bacck the default graphic settings
  par(opar)
}




store.1.dept.1 = StoreNDept(trainData,1,1)
x = store.1.dept.1
#findTrend(store.1.dept.1)
View(x)



# create a timeseries object
# TBD; replace start = first date from series
tsObj = ts(x$Weekly_Sales, freq=365.25/7,start=decimal_date(ymd("2010-2-5")))
plotTSGraphs(tsObj)

# --------- clustering --------------------------
cols = c(1,4,5,6,7,13,14)
clus = trainData[cols]
head(clus)
clus$Weekly_Sales = scale(clus[2])
clus$IsHoliday = ifelse(clus$IsHoliday==FALSE,0,1)

k.means.fit <- kmeans(clus, 15)
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster


library(cluster)
clusplot(clus, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# --------- clustering --------------------------

#-----------------------------------------
sales = vector()
# counting departments in each store
for(i in levels(df$Store)){
  cat(i," ",length(unique(df$Dept[df$Store==i])),"\n")  
  sales[i] = sum(df$Weekly_Sales[df$Store==i])
}

# storewise sales
plot(levels(df$Store),sales/1000)
plot(df$Temperature,df$Weekly_Sales)

# function - dept wise operations

# time series 
salesTS = ts(df$Weekly_Sales)
plot.ts(salesTS)
plot(df$Date,df$Weekly_Sales)

?subset.data.frame
store.1 = subset(df,df$Store==1)
View(store.1)

store.1.dept.1 = subset(store.1,store.1$Dept==1)
View(store.1.dept.1)
plot(store.1.dept.1$Date,store.1.dept.1$Weekly_Sales)
