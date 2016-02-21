# install.packages("TSA")  # if you need to
library(TSA)

# == Blue bird chips == #
# how does price relate to sales?

data(bluebird)
sales <- bluebird[, 1]
price <- bluebird[, 2]

plot(log(sales))
plot(price)

# the "ignore correlation" approach
summary(lm(log(sales) ~ price))

# == Public transit boardings == #
# how does the price of gas relate to the boardings on public transit?

data(boardings)
log.boardings = boardings[ , 1]
log.price = boardings[ , 2]

summary(lm(log.boardings ~ log.price))

#== US Consumption and Income
# how do changes in personal income relate to personal consumption
# install.packages("fpp")

library(fpp)
consumption <- usconsumption[, 1]
income <- usconsumption[, 2]
# units are percentage change in income/consumption

summary(lm(consumption ~ income))

