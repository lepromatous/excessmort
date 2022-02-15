library(tidyverse)
library(rvest)
library(forecast)
library(dygraphs)

#"/Users/timothywiemken/Dropbox/Work/SLU Research/Wiemken/COVID excess death/app/
#### Historical Data from: CDC wonder and NVSS = 1999 through 2019
df<-read.csv("totaldeaths_timeseries.csv")
df$date<-format(as.Date(paste("1", substr(df$Month.Code, start=6, stop=8), substr(df$Month.Code, start=1, stop=4), sep = "-"), format = "%d-%m-%Y"), "%Y-%m")
df<-df[,c("date", "Deaths")]
names(df)<-c("date", "deaths")


library("RSocrata")
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

df2 <- read.socrata(
  "https://data.cdc.gov/resource/r8kw-7aab.json",
  app_token = tokenz,
  email     = "timothy.wiemken@health.slu.edu",
  password  = "Lepr0m@tou$"
)

df2<-subset(df2, df2$state=="United States" & df2$group=="By Month")
df2<-df2[,c("start_date", "total_deaths")]
names(df2)<-c("date", "deaths")
df2$date<-format(df2$date, "%Y-%m")
df2$deaths<-as.numeric(df2$deaths)

### final data
df <- rbind(df, df2)
### clean
rm(list = ls(pattern="[^df]"))


### convert date back to full date with '1' for day - needed for some functions
df$date<-as.Date(paste("1", substr(df$date, start=6, stop=8), substr(df$date, start=1, stop=4), sep = "-"), format = "%d-%m-%Y")

#### select just data through feb 2020
df.pre<-df[1:254,]
#### post for overlaying will be: (excluding current month as itll always be less than it should be due to provisional counts)
df.post<-df[c(255:(nrow(df)-1)),]
#### create time series object - monthly
pre<-ts(df.pre$deaths, start=c(1999,1), end = c(2020,2), frequency = 12)



#### make actuals overlay
df.post2<-c(rep(NA, times=254), df.post$deaths, rep(NA, times=(36-nrow(df.post))))

pre %>%
  stlf(lambda = 0, h = 36, method="arima") %>%
  {cbind(actuals=.$x, forecast_mean=.$mean,
         lower_95=.$lower[,"95%"], upper_95=.$upper[,"95%"],
         lower_80=.$lower[,"80%"], upper_80=.$upper[,"80%"],
         covid_actuals=df.post2)} -> pre2
#################################################################
#################################################################
##################### end data ##################################
#################################################################
#################################################################


### do some calcs to see how much excess there is
df2<-data.frame(Y=as.matrix(pre2), date=time(pre2))
df2$date<-seq.Date(from = as.Date("1999-01-01"), length.out = nrow(df2), by="month")
df2<-subset(df2, !is.na(df2$Y.covid_actuals))

df2$actual.diff<-df2$Y.covid_actuals-df2$Y.forecast_mean
df2$lower.diff<-df2$Y.covid_actuals-df2$Y.lower_95
df2$upper.diff<-df2$Y.covid_actuals-df2$Y.upper_95


##### DIFFERENCES BETWEEN FORECAST/CI AND ACTUAL March - Dec 2020
##### could say the total differences here represent what extra COVID deaths there are over what is expected
sum(df2$actual.diff)
sum(df2$lower.diff)
sum(df2$upper.diff)


sum(df2$Y.forecast_mean)
sum(df2$Y.lower_95)
sum(df2$Y.upper_95)

### total actual deaths - covid_deaths isnt covid specific just duriing covid transmission time
sum(df2$Y.covid_actuals)

### deaths due to flu = 35k
### add this on since there was no flu season
351097.1 + 35000 # 386,097.1  ... mid/late jan was when actual covid deaths surpassed 400k (first reported jan 19, 2021)

##########################
### NYT

nyt<-read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
nyt$month<-lubridate::month(nyt$date)
nyt$newdeath<- nyt$deaths-lag(nyt$deaths)
nyt %>%
  filter(date>="2020-03-01" & date<"2021-01-01") %>%
  group_by(month) %>%
    summarise(total = sum(newdeath)) %>%
  sum() ->total.death.nyt  #346,113 march through dec 2020 - these are COVID deaths


sum(df2$actual.diff) - total.death.nyt ### predicted covid (death beyond predicted) and covid deaths from NYT = 4,984 more we document than NYT shows
sum(df2$lower.diff) - total.death.nyt ### could be as much as 112,779 more than actual
sum(df2$upper.diff) - total.death.nyt  ### could over predict  by 107,952.3

#1799 diff with ets


##########################
### Hopkins
library(tidyverse)
library(RCurl)

x1 <- RCurl::getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
data1 <- read.csv(text = x1)
data1<-subset(data1, data1$Country.Region=="US")
data2<-data.frame(t(data1))
data2$date<-row.names(data2)
data2<-data.frame(data2[5:nrow(data2),])
data2$date<-as.Date(gsub("X+", "", data2$date), format="%m.%d.%y")
names(data2)<-c("deaths", "date")
data2$deaths<-as.numeric(as.character(data2$deaths))

data2 %>%
  mutate(new.death = deaths - lag(deaths),
         moyr = as.yearmon(date)) ->data3

data3 %>%
  filter(., date>="2020-03-01" & date<="2020-12-31") %>%
  group_by(moyr) %>%
  summarise(n = sum(new.death)) -> yo

### total for time frame
sum(yo$n)  
sum(df2$actual.diff) - sum(yo$n)  
## 1958 diff with ETS

