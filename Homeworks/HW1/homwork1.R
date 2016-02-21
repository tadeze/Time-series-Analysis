#Lecture 03 
#Moving average 
library('dplyr')
library('lubridate')
nfl<-read.csv('HW1/NFL-trend-2004-2016.csv',header=T,comment.char = '#')
nfl$week.no <- 1:nrow(nfl)
qplot(data=nfl,week.no,nfl,geom="line") +xlab("Weeks starting from 2004 to 2016")+
	ylab("nfl word trend")

instagram<-read.csv('HW1/istagram-increase-trend.csv',header=T)
instagram$week.no <-1:nrow(instagram)
qplot(data=instagram,week.no,Instagram,geom="line") +xlab("Weeks starting from 2004 to 2016")+
	ylab("Instagram word trend")

restuarant<-read.csv('HW1/restuarant-past-90days.csv',header=T)
restuarant$restuarant.days <-1:nrow(restuarant)
qplot(data=restuarant,restuarant.days,restuarant,geom="line") +xlab("Last 90 days ")+
	ylab("Restuarant word trend")


ss$date <- ymd(ss$date)
ss<-mutate(ss,year=year(date),month=month(date),day=yday(date))


#Quastion number two coming up with better metric for climate data 
cor <-readRDS('HW1/corv_sub.rds')
str(cor) #check the structure
corv <- mutate(cor, 
							 year = year(date),
							 month = month(date),
							 yday = yday(date))
# dailyTemp <- corv %>% 
# 	group_by(yday) %>% 
# 	summarize(dailyAvg=mean(temp)) %>%	
# 	filter(dailyAvg<32)

#Daily average temperature of 
ggplot(data=corv,aes(yday,temp,group=yday))+
	geom_boxplot(outlier.colour = 'red') +
	theme_bw()
																												 		
#Daily perciption 
dailyRain <- corv %>% group_by(year,yday) %>% summarise(dPrecip=sum(precip,na.rm = T))
#ggplot(data=dailyRain,aes(x=yday,y=dPrecip),group=yday)+geom_line()+theme_bw()
#Average daily temp 
avg.daily.rain <- dailyRain %>% group_by(yday) %>% summarize(mean.rain=mean(dPrecip))
qplot(yday,mean.rain,data=avg.daily.rain,geom='line')

#-------How often I will bike to work below freezing temperature-------#
avg.daily.temp <- corv %>% 
	filter(hour %in% c(6,7,8,9)) %>% 
	group_by(yday) %>%
	summarize(min.temp=median(temp,na.rm = T))
#plot daily with morning temp
qplot(data=avg.daily.temp,yday,min.temp) +geom_line(aes(y=32,color='red'))
cat("Number of days with freezing temperature",nrow(avg.daily.temp%>%filter(min.temp<=32)))

#-----------How many days will I see sunshine------------# 
#Take a typical time say at noon
#How what is the average probability that we will see clear sunshine at noon
sunshine.at.noon <- corv %>% 
	filter(hour==12 & conditions=='Clear') %>%
	group_by(yday) %>%
	summarize(totalDays=length(yday)/30)
qplot(data=sunshine.at.noon,yday,totalDays,geom="point") +geom_line(aes(y=0.5,color='red'))
sunshine.days <-nrow(sunshine.at.noon%>%filter(totalDays>0.5))
cat("Based on average noon weather probability, the number of sunshine days are ",sunshine.days)


#How many days will I need a raincoat during commiting.
morningEveningRain <- corv %>% filter(hour %in% c(7,8,18,19)) %>% group_by(yday) %>%
	summarize(max.rain=max(precip,na.rm = T))
qplot(data=morningEveningRain,yday,max.rain,geom='point')
#Number of rain that need coat.
#Assume we can tolerate rain of 0.07
tolerance <- 0.05 #tolerance of percipitation
number.of.days<-na.omit(morningEveningRain) %>% filter(max.rain>tolerance) %>%nrow
cat(number.of.days,"days will rain\n")
#Frequency  of conditions 
# 72 readings per day
monthlyCount <- corv %>% filter(conditions=="Clear") %>% group_by(year,yday) %>% 
	summarize(sunnyDays=length(conditions)/72)

