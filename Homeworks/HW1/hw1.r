# Monthly motor vehicle and parts dealers
# http://www.census.gov/econ/currentdata/dbsearch?program=MARTS&startYear=1992&endYear=2014&categories%5B%5D=441&dataType=SM&geoLevel=US&notAdjusted=1&submit=GET+DATA

sales <- read.csv("../data/SeriesReport-201401161229.csv", skip = 7, header = TRUE)
names(sales) <- tolower(names(sales))

# pull out month and year
sales$month <- substr(sales$period, 1, 3)
sales$year <- as.numeric(substr(sales$period, 5, 9))

# convert months to numbers
months <- 1:12
names(months) <- month.abb
sales$month <- months[sales$month]

# add a date column, use 15th of month arbitrarily
sales$date <- with(sales, as.Date(ISOdate(year, month, day = 15)))

# just take two columns, and get rid of missing 2014
sales <- subset(sales, date <= as.Date("2014-01-01"), select = c("date", "value"))

save(sales, file = "../data/sales.rda")