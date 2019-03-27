rm(list=ls())

library(ggplot2)
library(dplyr)
library(Kendall)
library(RColorBrewer)

### Load and pepare data ###

setwd("/Users/owenryerson/Google Drive/Birds!/R data Processing/CSV/LaCruz")

getwd()

files<-dir()
files

sb <- read.csv(files[18], stringsAsFactors = FALSE)  ##"LaCruz_ShoreBirds.csv"

## clean the data

sb$DateTime <- as.POSIXct(sb$DateTime, format = "%m/%d/%y")

sb <- filter(sb, season != "09-10")  ## fall 11 is already gone

sum(sb$Count, na.rm = TRUE)

### make graph of speceis trend

sznsp <- group_by(sb, Species, season) %>%
  summarise(sum(Count, na.rm = TRUE))

sznsp <- rename(sznsp, count = 'sum(Count, na.rm = TRUE)')

ggplot(data = sznsp, aes(x=season, y= count)) + facet_wrap(sznsp$Species ~.,scales="free_y") + 
  geom_point() +
  labs(title="Laguna La Cruz Waterbird Counts 2010-2018",  x= "Season", y="Count") +
  theme_classic() +
  geom_smooth(method="lm", se=FALSE) 


### create data frame with monthly averages, including NA

start = as.Date("2010-10-01")
full <- seq(start, by='1 month', length=93)
View(full)


######## The following section of code attempts to run the seasonal mann-kendall on monthly averages of the shore
#### bird data inculding months with mising data. This did NOT work

### monthly averages
sbmavg <- sb
sbmavg$month <-  months(sbmavg$DateTime)

sbmavg1 <- group_by(sbmavg, yearmonth) %>%
  summarise(sum(Count, na.rm = TRUE))

sbmavg1 <- rename(sbmavg1, count = 'sum(Count, na.rm = TRUE)')

daypermonth <- group_by(sbmavg, yearmonth) %>%
  summarise(count= n_distinct(DateTime, na.rm = TRUE))

daypermonth <- rename(daypermonth, days = count)

sbmavg1 <- left_join(sbmavg1, daypermonth)

sbmavg1$avg <- sbmavg1$count / sbmavg1$days

sbmavg1$yearmonth <- as.Date(paste(sbmavg1$yearmonth,"-01",sep=""))  ### adds day to year month

sbavg <- select(sbmavg1, yearmonth, avg)

sbma <- data.frame(Date=full, Value=with(sbavg, avg[match(full, yearmonth)]))    
sbma$Value <- as.numeric(sbma$Value)
sbma$Value <- as.vector(sbma$Value)

ts <- ts(sbma$Value, start=c(2010, 10),  frequency=12)
ts

sm <- SeasonalMannKendall(ts)
summary(sm)







sbmanona <- filter(sbma, Value != "NA")

sbval <- as.vector(sbmanona$Value)

l <- SeasonalMannKendall(sbval)  ## normal mk for monthly averages with na
summary(l)


?SeasonalMannKendall

summary(ts)



sb15 <- sb[!table(sb$Species)[sb$Species] < 15,] ### removes speceis with less than 15 occorances

birds<-unique(sb15$Species)
num_birds<-length(birds)

bird <- as.data.frame(birds)
write.csv(bird, file = 'lacruz_sb_sp.csv')


### Loop to make a df for all sp.

for (i in 1:length(birds)) {
  assign(paste("df",birds[i], sep="_"), data_frame(subset(sb, Species == birds[i])))
}

### Loop to do seasonal mann kendal on all species

for (i in 1:length(birds)) {
  newdataframe<-(subset(sb15, Species == birds[i]))
  ts <- ts(newdataframe$Count)
  mann <- SeasonalMannKendall(ts)
  summary(mann)
}

## regular mann-kendall

for (i in 1:length(birds)) {
  newdataframe<-(subset(sb15, Species == birds[i]))
  mann <- MannKendall(newdataframe$Count)
  summary(mann)
}


## maybe use a glm ????

summary(m1 <- glm(Count ~ season, family="poisson", data=sb))



### create data frame with all dates, including NA

start = as.Date("2009-10-20")
full <- seq(start, by='1 month', length= 58)

sb <- select(shorebirds, DateTime, count)
sb$DateTime <- as.Date(sb$DateTime)

sbmonth <- data.frame(Date=full, Value=with(sb, count[match(full, DateTime)]))    
sbday$Value <- as.numeric(sbday$Value)
sbday$Value <- as.vector(sbday$Value)





