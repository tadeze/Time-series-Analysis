#Playing with date
library('lubridate')
#Times and Dates using lubridate
A <- "1-7-14"
B<-"Jan 7 14"
C<-"1:15 AM 2014-01-07"
D<-"3:25 PM"
E<-"Tue Jan 7 2014"

parse_date_time(A,"dmy")
parse_date_time(B,"%m%d%y")
parse_date_time(C,"hm ymd")
parse_date_time(D,"IM!p")
#parse_date_time(E,"")

#Plotting in R 
corv <-read.csv('../corvallis.csv')
qplot(,temp,data=corv,geom="line") #geom type of geomertric line to use 
