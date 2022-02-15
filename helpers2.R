library(tidyverse)
library(rvest)
library(forecast)
library(dygraphs)
library(anomalize)
library(scales)
library(plotly)
library(BreakoutDetection)

#devtools::install_github("twitter/BreakoutDetection")

#### Historical Data from: CDC wonder and NVSS = 1999 through 2019
df<-read.csv("totaldeaths_timeseries.csv")
df$date<-format(as.Date(paste("1", substr(df$Month.Code, start=6, stop=8), substr(df$Month.Code, start=1, stop=4), sep = "-"), format = "%d-%m-%Y"), "%Y-%m")
df<-df[,c("date", "Deaths")]
names(df)<-c("date", "deaths")


### live provisional data
## scrape
# url<-"https://www.cdc.gov/coronavirus/2019-ncov/covid-data/covidview/10302020/nchs-mortality-report.html"
# url%>%
#   read_html(url) %>%
#   html_nodes("table") %>%
#   html_table %>%
#   .[[1]]-> out
# 
# ### clean/drop crap
# out <- out[-1,1:3]
# out[,3]<-gsub(",", "", out[,3])
# out[,3]<-as.numeric(out[,3])
# ### make month year
# out$date<-format(as.Date(paste("1", out$Week, out$Year, sep = "-"), format = "%w-%W-%Y"), "%Y-%m")
# ### drop 2019
# out<-subset(out, out$Year!=2019)
# ### clean more
# out<-out[,c(4,3)]
# names(out)<-c("date", "deaths")
# ### sum by month
# 
# out %>%
#   group_by(date) %>%
#   summarise(deaths = sum(deaths, na.rm=T)) -> out2

#### new live provisional data
# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab
library("RSocrata")
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

df2 <- read.socrata(
  "https://data.cdc.gov/resource/r8kw-7aab.json?state=United States",
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  = "ThisIsNotAGoodP@ssw0rd!!!"
)

df2<-subset(df2, df2$group=="By Month")
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


#### start plot
interval_value_formatter <- "function(num, opts, seriesName, g, row, col) {
  value = g.getValue(row, col);
  if(value[0] != value[2]) {
    lower = Dygraph.numberValueFormatter(value[0], opts);
    upper = Dygraph.numberValueFormatter(value[2], opts);
    return '[' + lower + ', ' + upper + ']';
  } else {
    return Dygraph.numberValueFormatter(num, opts);
  }
}"


dyplot<-function(meth="arima"){
pre %>%
  stlf(lambda = "auto", h = 36, method=meth, robust=T) %>%
  {cbind(actuals=.$x, forecast_mean=.$mean,
         lower_95=.$lower[,"95%"], upper_95=.$upper[,"95%"],
         lower_80=.$lower[,"80%"], upper_80=.$upper[,"80%"],
         covid_actuals=df.post2)}  %>%
  dygraph(main="", ylab = "Number of Deaths") %>%
  dyAxis("y", valueFormatter = interval_value_formatter) %>%
  dySeries("actuals", color = "black", label="Pre-COVID-19 Mortality") %>%
  dySeries("forecast_mean", color = "blue", label = "Forecasted Mortality") %>%
  dySeries(c("lower_80", "forecast_mean", "upper_80"),
           label = "80% Confidence Band", color = "blue") %>%
  dySeries(c("lower_95", "forecast_mean", "upper_95"),
           label = "95% Confidence Band", color = "blue") %>%
  dySeries("covid_actuals", color = "red", label="COVID-19 era Mortality") %>%
  dyLegend(labelsSeparateLines=TRUE, show="never") %>%
  dyRangeSelector(dateWindow = c(as.Date("2017-01-01"), Sys.Date())) %>%
  dyOptions(digitsAfterDecimal = 1, axisLabelWidth=75) %>%
  dyCSS(textConnection(".dygraph-legend {background-color: rgba(255, 255, 255, 0.5) !important; }")) %>%
  dyAxis(
    "y",
    label = "Total Number of Deaths (All-Cause)",
    valueFormatter =  'function(d){return  d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
    axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
  ) -> p
  return(p)
}


# ----------------------------------------------------------------------------#
# ANOMALY DETECTION
# ----------------------------------------------------------------------------#
anombreak.mort<-function(alp=0.05, max_anoms=0.2, n.break=7){
  
plotz<-tibble(df[-nrow(df),])
names(plotz) <- c("ds", "y")

#--------------------------------
#### anomaly
x<-time_decompose(data=plotz, target=y,  frequency="auto", trend="auto", method="stl")
z<-anomalize(data=x, target = remainder, alpha = alp, max_anoms = max_anoms, method="gesd")
a<-time_recompose(z)

### make upper limit
upper<-ifelse(max(a$recomposed_l2) > max(plotz$y),
              max(a$recomposed_l2), max(plotz$y))

#### make the plot look nice
anomplot<-ggplot() +
  geom_ribbon(aes(ymin = a$recomposed_l1, ymax = a$recomposed_l2, x=a$ds),
              fill = "#4b0082", alpha=0.5) +
  geom_line(aes(y=a$observed, x=a$ds), color="black", size=0.7) +
  geom_point(aes(y=a$observed, x=a$ds), 
             color=ifelse(a$anomaly=="Yes", "black", "gray35"), 
             fill=ifelse(a$anomaly=="Yes", "red", "gray35"), 
             shape=ifelse(a$anomaly=="Yes", 21, 1), 
             size=ifelse(a$anomaly=="Yes", 2, 0.001)) +
  scale_x_date(date_breaks="1 year", expand = c(0.02, 0.02), name="") +
  scale_y_continuous(expand = c(0.04, 0.03), labels = scales::comma, n.breaks = 6) +
  ylab("Number of Deaths") +
  coord_cartesian(ylim=c(0, (upper + (10 - upper %% 10)))) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(),
        panel.background = element_blank(),
        legend.position="bottom")
  #annotate("text", label=paste0("Total deaths = ", format(total, digits=0, scientific=F, big.mark=",")), x=(min(plotme2$ds)+20), y=max(plotme2$y))

#------------------------------------------------
#### breakout
plotme4<-plotz

names(plotme4)<-c("timestamp", "count")


## run breakout algorithm
res.break = BreakoutDetection::breakout(plotme4, min.size=10, method='multi', beta=.001, degree=1, plot=TRUE)
plotme4$timestamp<-as.Date(plotme4$timestamp)

## create mean segments
segs<-c(0, res.break$loc, nrow(plotme4))
means<-vector("list", length(segs))
group<-NULL
for(i in 1:(length(segs)-1)){
  group<-c(group,rep(i, times=diff(segs)[i]))
}
plotme4$group<-factor(group)
groupmean<-tapply(plotme4$count, plotme4$group, function(x) mean(x))

segment_data = data.frame(
  x = segs[2:length(segs)-1],
  xend = segs[2:length(segs)], 
  y = groupmean,
  yend = groupmean
)
segment_data$x <- ifelse(segment_data$x==0, 1, segment_data$x)
startdate<-as.Date(unlist(lapply(segment_data$x, function(x) plotme4$timestamp[x])), origin="1970-01-01")
enddate<-c(startdate[2:length(startdate)], max(plotme4$timestamp))

plotz2<-anomplot + 
  geom_segment(aes(x = startdate, 
                   y = segment_data$y, 
                   xend = enddate, 
                   yend = segment_data$yend), 
               linetype="dotted",
               color="#5c5b5b", 
               lwd=0.9, 
               show.legend = T)
  
ggplotly(plotz2)
}##### end





