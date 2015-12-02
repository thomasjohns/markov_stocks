# install or load the matrix exponential package
# which allows you to raise a matrix A to the power k
# with the command A %^% k

# install.packages("expm")
library(expm)

# set working derectory to the same as the data
setwd("~/math_381_group/markov_stocks")

# read in the data
data <- read.csv('goog.csv')
#data <- read.csv('pbr_a.csv')

# set date at which to stop training data and start simulation
stop_date <- "2015-01-01"

# read in closing prices and training_dates
all_dates <- rev(as.Date(data$Date, format='%d-%b-%y'))
training_dates <- all_dates[all_dates < stop_date]
last_year_dates <- all_dates[all_dates >= stop_date]

all_closing_prices <- rev(data$Close)
training_closing_prices <- all_closing_prices[all_dates < stop_date]
last_year_closing_prices <- all_closing_prices[all_dates >= stop_date]

# # plot the stock over the given dates
# plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
#      xlab='Date', ylab='Closing Price', main='Daily Price for Google Share',
#      xlim=c(all_dates[1], all_dates[length(all_dates)]),
#      ylim=c(min(all_closing_prices), max(all_closing_prices)))
# lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)

# compute the daily change in the stock
daily_change <- diff(training_closing_prices)

# # plot the daily change
# transparent_red <- rgb(t(col2rgb('red')), alpha=65, maxColorValue=255)
# plot(training_dates[2:length(training_dates)], daily_change, cex=0.8,
#      pch=19, col=transparent_red, lwd=1,
#      xlab='Date', ylab='Change from previous date', 
#      main='Daily change in Google stock price')
# # plot a line for the mean daily change
# lines(training_dates[2:length(training_dates)], 
#       rep(mean(daily_change), times=length(daily_change)),
#       col='blue', lwd=3, lty=2)

# First model:
# 2 x 2 transition matrix where the states are:
# 1. increase from the day before
# 2. decrease from the day before
basic_trans_mat <- matrix(c(0, 0, 0, 0), nrow=2, ncol=2)
total_increase_days <- sum(daily_change[1:length(daily_change)-1] >= 0)
total_decrease_days <- sum(daily_change[1:length(daily_change)-1] < 0)

# fill in transition matrix
for (day in 1:(length(daily_change)-1)) {
   if (daily_change[day] >= 0) {  # today there was an increase
      if (daily_change[day+1] >= 0) {  # tomorrow there is an increase
         # increment the increase to increase element of the transition matrix
         basic_trans_mat[1, 1] <- basic_trans_mat[1, 1] + 1
      } else {  # tomorrow there is a decrease
         # increment the increase to decrease element of the transition matrix
         basic_trans_mat[1, 2] <- basic_trans_mat[1, 2] + 1
      }
   } else {  # today there was a decrease
      if (daily_change[day+1] >= 0) {  # tomorrow there is an increase
         # increment the decrease to increase element of the transition matrix
         basic_trans_mat[2, 1] <- basic_trans_mat[2, 1] + 1
      } else {  # tomorrow there is a decrease
         # increment the decrease to decrease element of the transition matrix
         basic_trans_mat[2, 2] <- basic_trans_mat[2, 2] + 1
      }
   }
}

# normalize first row by the number of "from increase" days
basic_trans_mat[1, 1:2] <- basic_trans_mat[1, 1:2] / total_increase_days

# normalize the second row by the number of "from decrease" days
basic_trans_mat[2, 1:2] <- basic_trans_mat[2, 1:2] / total_decrease_days

# look at the transition matrix
print('basic transition matrix:', quote=F)
print(basic_trans_mat)
print('basic transition matrix to power 10:', quote=F)
print(basic_trans_mat %^% 10)


# simulate the last year using a random walk using the value
# in our transition matrix and compare to the actual stock fluctuation
# for the last year
basic_model_simulation <- function() {
   last_change <- daily_change[length(daily_change)]
   last_price <- training_closing_prices[length(training_closing_prices)]
   #mean_change <- mean(abs(daily_change))
   
   #### variable incriments ####
   basic_mean <- mean(abs(daily_change))
   basic_sd <- sd(abs(daily_change))
   #### variable incriments ####
   
   predicted_prices <- vector(length=length(last_year_dates))
   
   for (i in 1:length(predicted_prices)) {
      
      #### variable incriments ####
      mean_change <- rnorm(1, mean=basic_mean, sd=basic_sd)
      #### variable incriments ####
      
      rand <- runif(1)
      if (last_change >= 0) {  # increase yesterday, so use first row of transition matrix
         if (rand < basic_trans_mat[1, 1]) {
            predicted_prices[i] <- last_price + mean_change
         } else {
            predicted_prices[i] <- last_price - mean_change
         }
      } else {  # decrease yesterday, so use second row of transition matrix
         if (rand < basic_trans_mat[2, 1]) {
            predicted_prices[i] <- last_price + mean_change
         } else {
            predicted_prices[i] <- last_price - mean_change
         }
      }
      if (i == 1) {
         last_change <- predicted_prices[i] - last_price
      } else {
         last_change <- predicted_prices[i] - predicted_prices[i-1]
      }
      last_price <- predicted_prices[i]
   }
   return(predicted_prices)
}

# plot the simulation against the actual values
plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
     xlab='Date', ylab='Closing Price', main='Simulation Using Basic Model',
     xlim=c(all_dates[1], all_dates[length(all_dates)]),
     ylim=c(min(all_closing_prices), max(all_closing_prices)))
lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)

num_simulations <- 3
colors <- c('purple', 'green', 'red')
for (i in 1:num_simulations) {
   predicted_prices <- basic_model_simulation()
   # simulated values
   lines(last_year_dates, predicted_prices, col=colors[i], lwd=1)
}
legend('topleft', legend=c('Training data', 'Actual 2015 values', 
                           'simulation 1', 'simulation 2', 'simulation 3'), 
       col=c('blue', 'black', 'purple', 'green', 'red'), lwd=c(2, 2, 2, 2, 2))



# Second model:
# 4 x 4 transition matrix where the states are:
# 1. large increase
# 2. small increase
# 3. small decrease
# 4. large decrease
second_trans_mat <- matrix(rep(0, 36), nrow=4, ncol=4)
change_cutoff <- mean(abs(daily_change))
#change_cutoff <- 4
total_large_increase_days <- sum(daily_change[1:length(daily_change)-1] >= change_cutoff)
total_small_increase_days <- sum(daily_change[1:length(daily_change)-1] < change_cutoff &
                                 daily_change[1:length(daily_change)-1] >= 0)
total_small_decrease_days <- sum(daily_change[1:length(daily_change)-1] < 0 &
                                 daily_change[1:length(daily_change)-1] >= -change_cutoff)
total_large_decrease_days <- sum(daily_change[1:length(daily_change)-1] < -change_cutoff)

# fill in the second transition matrix
for (day in 1:(length(daily_change)-1)) {
   change_today <- daily_change[day]
   change_tom <- daily_change[day+1]
   if (change_today >= change_cutoff) {  # coming from large increase
      if (change_tom >= change_cutoff) {
         # large increase to large increase
         second_trans_mat[1, 1] <- second_trans_mat[1, 1] + 1
      } else if (change_tom >= 0) {
         # large increase to small increase
         second_trans_mat[1, 2] <- second_trans_mat[1, 2] + 1
      } else if (change_tom >= -change_cutoff) {
         # large increase to small decrease
         second_trans_mat[1, 3] <- second_trans_mat[1, 3] + 1
      } else {
         # large increase to large decrease
         second_trans_mat[1, 4] <- second_trans_mat[1, 4] + 1
      }
   } else if (change_today >= 0) {  # coming from small increase
      if (change_tom >= change_cutoff) {
         # small increase to large increase
         second_trans_mat[2, 1] <- second_trans_mat[2, 1] + 1
      } else if (change_tom >= 0) {
         # small increase to small increase
         second_trans_mat[2, 2] <- second_trans_mat[2, 2] + 1
      } else if (change_tom >= -change_cutoff) {
         # small increase to small decrease
         second_trans_mat[2, 3] <- second_trans_mat[2, 3] + 1
      } else {
         # small increase to large decrease
         second_trans_mat[2, 4] <- second_trans_mat[2, 4] + 1
      }
   } else if (change_today >= -change_cutoff) {  # coming from small decrease
      if (change_tom >= change_cutoff) {
         # small decrease to large increase
         second_trans_mat[3, 1] <- second_trans_mat[3, 1] + 1
      } else if (change_tom >= 0) {
         # small decrease to small increase
         second_trans_mat[3, 2] <- second_trans_mat[3, 2] + 1
      } else if (change_tom >= -change_cutoff) {
         # small decrease to small decrease
         second_trans_mat[3, 3] <- second_trans_mat[3, 3] + 1
      } else {
         # small decrease to large decrease
         second_trans_mat[3, 4] <- second_trans_mat[3, 4] + 1
      }
   } else {  # coming from large decrease
      if (change_tom >= change_cutoff) {
         # large decrease to large increase
         second_trans_mat[4, 1] <- second_trans_mat[4, 1] + 1
      } else if (change_tom >= 0) {
         # large decrease to small increase
         second_trans_mat[4, 2] <- second_trans_mat[4, 2] + 1
      } else if (change_tom >= -change_cutoff) {
         # large decrease to small decrease
         second_trans_mat[4, 3] <- second_trans_mat[4, 3] + 1
      } else {
         # large decrease to large decrease
         second_trans_mat[4, 4] <- second_trans_mat[4, 4] + 1
      }
   }
}
#print(second_trans_mat)
# normalize the rows by the number of "coming from days"
second_trans_mat[1, ] <- second_trans_mat[1, ] / total_large_increase_days
second_trans_mat[2, ] <- second_trans_mat[2, ] / total_small_increase_days
second_trans_mat[3, ] <- second_trans_mat[3, ] / total_small_decrease_days
second_trans_mat[4, ] <- second_trans_mat[4, ] / total_large_decrease_days

# look at the transition matrix
print('second transition matrix:', quote=F)
print(second_trans_mat)
print('second transition matrix to power 30:', quote=F)
print(second_trans_mat %^% 30)


# simulate the last year using a random walk using the value
# in our transition matrix and compare to the actual stock fluctuation
# for the last year
second_model_simulation <- function() {
   last_change <- daily_change[length(daily_change)]
   last_price <- training_closing_prices[length(training_closing_prices)]
   daily_change_magnitudes <- abs(daily_change)
   #small_change <- mean(daily_change_magnitudes[daily_change_magnitudes < change_cutoff])
   #large_change <- mean(daily_change_magnitudes[daily_change_magnitudes >= change_cutoff])
   
   #### variable incriments ####
   large_mean <- mean(daily_change_magnitudes[daily_change_magnitudes >= change_cutoff])
   large_sd <- sd(daily_change_magnitudes[daily_change_magnitudes >= change_cutoff])
   small_mean <- mean(daily_change_magnitudes[daily_change_magnitudes < change_cutoff])
   small_sd <- sd(daily_change_magnitudes[daily_change_magnitudes < change_cutoff])
   #### variable incriments ####
   
   predicted_prices <- vector(length=length(last_year_dates))
   
   for (i in 1:length(predicted_prices)) {
      
      #### variable incriments ####
      large_change <- rnorm(1, mean=large_mean, sd=large_sd)
      small_change <- rnorm(1, mean=small_mean, sd=small_sd)
      #### variable incriments ####
      
      rand <- runif(1)
      if (last_change >= change_cutoff) { # from large increase
         if (rand < second_trans_mat[1, 1]) {
            # to large increase
            predicted_prices[i] <- last_price + large_change
         } else if (rand < second_trans_mat[1, 1] + second_trans_mat[1, 2]) {
            # to small increase
            predicted_prices[i] <- last_price + small_change
         } else if (rand < second_trans_mat[1, 1] + second_trans_mat[1, 2] + 
                       second_trans_mat[1, 3]) {
            # to small decrease
            predicted_prices[i] <- last_price - small_change
         } else {
            # to large decrease
            predicted_prices[i] <- last_price - large_change
         }
      } else if (last_change >= 0) { # from small increase
         if (rand < second_trans_mat[2, 1]) {
            # to large increase
            predicted_prices[i] <- last_price + large_change
         } else if (rand < second_trans_mat[2, 1] + second_trans_mat[2, 2]) {
            # to small increase
            predicted_prices[i] <- last_price + small_change
         } else if (rand < second_trans_mat[2, 1] + second_trans_mat[2, 2] + 
                       second_trans_mat[2, 3]) {
            # to small decrease
            predicted_prices[i] <- last_price - small_change
         } else {
            # to large decrease
            predicted_prices[i] <- last_price - large_change
         }
      } else if (last_change >= -change_cutoff) { # from small decrease
         if (rand < second_trans_mat[3, 1]) {
            # to large increase
            predicted_prices[i] <- last_price + large_change
         } else if (rand < second_trans_mat[3, 1] + second_trans_mat[3, 2]) {
            # to small increase
            predicted_prices[i] <- last_price + small_change
         } else if (rand < second_trans_mat[3, 1] + second_trans_mat[3, 2] + 
                       second_trans_mat[3, 3]) {
            # to small decrease
            predicted_prices[i] <- last_price - small_change
         } else {
            # to large decrease
            predicted_prices[i] <- last_price - large_change
         }
      } else { # from large decrease
         if (rand < second_trans_mat[4, 1]) {
            # to large increase
            predicted_prices[i] <- last_price + large_change
         } else if (rand < second_trans_mat[4, 1] + second_trans_mat[4, 2]) {
            # to small increase
            predicted_prices[i] <- last_price + small_change
         } else if (rand < second_trans_mat[4, 1] + second_trans_mat[4, 2] + 
                       second_trans_mat[4, 3]) {
            # to small decrease
            predicted_prices[i] <- last_price - small_change
         } else {
            # to large decrease
            predicted_prices[i] <- last_price - large_change
         }
      }
      if (i == 1) {
         last_change <- predicted_prices[i] - last_price
      } else {
         last_change <- predicted_prices[i] - predicted_prices[i-1]
      }
      last_price <- predicted_prices[i]
   }
   return(predicted_prices)
}

# plot the simulation against the actual values
plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
     xlab='Date', ylab='Closing Price', main='Simulation Using Second Model',
     xlim=c(all_dates[1], all_dates[length(all_dates)]),
     ylim=c(min(all_closing_prices), max(all_closing_prices)))
lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)

num_simulations <- 3
colors <- c('purple', 'green', 'red')
for (i in 1:num_simulations) {
   predicted_prices <- second_model_simulation()
   # simulated values
   lines(last_year_dates, predicted_prices, col=colors[i], lwd=1)
}
legend('topleft', legend=c('Training data', 'Actual 2015 values', 
                           'simulation 1', 'simulation 2', 'simulation 3'), 
       col=c('blue', 'black', 'purple', 'green', 'red'), lwd=c(2, 2, 2, 2, 2))


####### plot with many simulations #######

# # color density plot
# plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
#      xlab='Date', ylab='Closing Price', main='Simulation Using Second Model',
#      xlim=c(all_dates[1], all_dates[length(all_dates)]),
#      ylim=c(min(all_closing_prices), max(all_closing_prices)))
# lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)
# 
# num_simulations <- 250
# my_color <- rgb(t(col2rgb('orange')), alpha=10, maxColorValue=255)
# for (i in 1:num_simulations) {
#    predicted_prices <- second_model_simulation()
#    # simulated values
#    lines(last_year_dates, predicted_prices, col=my_color, lwd=2)
# }

# # plot of averages
# plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
#      xlab='Date', ylab='Closing Price', main='Average of simulations Using Second Model',
#      xlim=c(all_dates[1], all_dates[length(all_dates)]),
#      ylim=c(min(all_closing_prices), max(all_closing_prices)))
# lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)
# 
# num_simulations <- 250
# average_predicted_prices <- rep(0, times=length(last_year_closing_prices))
# for (i in 1:num_simulations) {
#    average_predicted_prices <- average_predicted_prices + second_model_simulation()  
# }
# average_predicted_prices <- average_predicted_prices / num_simulations
# lines(last_year_dates, average_predicted_prices, col='orange', lwd=2)
