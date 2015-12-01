# install.packages("expm")
library(expm)

# set working derectory to the same as the data
setwd("~/math_381_group/markov_stocks")

# read in the data
#data <- read.csv('goog.csv')
data <- read.csv('pbr_a.csv')

# set date at which to stop training data and start simulation
stop_date <- "2015-01-01"

# read in closing prices and training_dates
all_dates <- rev(as.Date(data$Date, format='%d-%b-%y'))
training_dates <- all_dates[all_dates < stop_date]
last_year_dates <- all_dates[all_dates >= stop_date]

all_closing_prices <- rev(data$Close)
training_closing_prices <- all_closing_prices[all_dates < stop_date]
last_year_closing_prices <- all_closing_prices[all_dates >= stop_date]

# compute the daily change in the stock
daily_change <- diff(training_closing_prices)

# Third model:
# 4 x 4 transition matrix where the states are:
# 1. increase then increase
# 2. increase then decrease
# 3. decrease then increase
# 4. decrease then decrease
third_trans_mat <- matrix(rep(0, 36), nrow=4, ncol=4)

# count the total number of occurences of each state
total_inc_inc_days <- 0
total_inc_dec_days <- 0
total_dec_inc_days <- 0
total_dec_dec_days <- 0
for (i in 1:(length(daily_change)-1)) {
   if (daily_change[i] > 0 & daily_change[i+1] > 0) {
      total_inc_inc_days <- total_inc_inc_days + 1
   } else if (daily_change[i] > 0 & daily_change[i+1] < 0) {
      total_inc_dec_days <- total_inc_dec_days + 1
   } else if (daily_change[i] < 0 & daily_change[i+1] > 0) {
      total_dec_inc_days <- total_dec_inc_days + 1
   } else {
      total_dec_dec_days <- total_dec_dec_days + 1
   }
}

# fill in the third transition matrix
for (day in 1:(length(daily_change)-2)) {
   change_1 <- daily_change[day]
   change_2 <- daily_change[day+1]
   change_3 <- daily_change[day+1]
   change_4 <- daily_change[day+2]
   if (change_1 >= 0 & change_2 >= 0) {  # coming from inc inc
      if (change_3 >= 0 & change_4 >= 0) {
         # to inc inc
         third_trans_mat[1, 1] <- third_trans_mat[1, 1] + 1
      } else if (change_3 >= 0 & change_4 < 0) {
         # to inc dec
         third_trans_mat[1, 2] <- third_trans_mat[1, 2] + 1
      } else if (change_3 < 0 & change_4 >= 0) {
         # to dec inc
         third_trans_mat[1, 3] <- third_trans_mat[1, 3] + 1
      } else {
         # to dec dec
         third_trans_mat[1, 4] <- third_trans_mat[1, 4] + 1
      }
   } else if (change_1 >= 0 & change_2 < 0) {  # coming from inc dec
      if (change_3 >= 0 & change_4 >= 0) {
         # to inc inc
         third_trans_mat[2, 1] <- third_trans_mat[2, 1] + 1
      } else if (change_3 >= 0 & change_4 < 0) {
         # to inc dec
         third_trans_mat[2, 2] <- third_trans_mat[2, 2] + 1
      } else if (change_3 < 0 & change_4 >= 0) {
         # to dec inc
         third_trans_mat[2, 3] <- third_trans_mat[2, 3] + 1
      } else {
         # to dec dec
         third_trans_mat[2, 4] <- third_trans_mat[2, 4] + 1
      }
   } else if (change_1 < 0 & change_2 >= 0) {  # coming from dec inc
      if (change_3 >= 0 & change_4 >= 0) {
         # to inc inc
         third_trans_mat[3, 1] <- third_trans_mat[3, 1] + 1
      } else if (change_3 >= 0 & change_4 < 0) {
         # to inc dec
         third_trans_mat[3, 2] <- third_trans_mat[3, 2] + 1
      } else if (change_3 < 0 & change_4 >= 0) {
         # to dec inc
         third_trans_mat[3, 3] <- third_trans_mat[3, 3] + 1
      } else {
         # to dec dec
         third_trans_mat[3, 4] <- third_trans_mat[3, 4] + 1
      }
   } else {  # coming from dec dec
      if (change_3 >= 0 & change_4 >= 0) {
         # to inc inc
         third_trans_mat[4, 1] <- third_trans_mat[4, 1] + 1
      } else if (change_3 >= 0 & change_4 < 0) {
         # to inc dec
         third_trans_mat[4, 2] <- third_trans_mat[4, 2] + 1
      } else if (change_3 < 0 & change_4 >= 0) {
         # to dec inc
         third_trans_mat[4, 3] <- third_trans_mat[4, 3] + 1
      } else {
         # to dec dec
         third_trans_mat[4, 4] <- third_trans_mat[4, 4] + 1
      }
   }
}

# normalize the rows by the number of "coming from days"
third_trans_mat[1, ] <- third_trans_mat[1, ] / total_inc_inc_days
third_trans_mat[2, ] <- third_trans_mat[2, ] / total_inc_dec_days
third_trans_mat[3, ] <- third_trans_mat[3, ] / total_dec_inc_days
third_trans_mat[4, ] <- third_trans_mat[4, ] / total_dec_dec_days

# look at the transition matrix
print('third transition matrix:', quote=F)
print(third_trans_mat)
print('third transition matrix to power 30:', quote=F)
print(third_trans_mat %^% 30)


# simulate the last year using a random walk using the value
# in our transition matrix and compare to the actual stock fluctuation
# for the last year
third_model_simulation <- function() {
   change_before <- daily_change[length(daily_change)]
   change_two_before <- daily_change[length(daily_change) - 1]
   
   last_price <- training_closing_prices[length(training_closing_prices)]
   
   mean_change <- mean(abs(daily_change))
   
   predicted_prices <- vector(length=length(last_year_dates))
   
   for (i in 1:length(predicted_prices)) {
      rand <- runif(1)
      if (change_two_before >= 0 & change_before >= 0) { # from inc inc
         if (rand < third_trans_mat[1, 1]) {
            # to inc inc 
            predicted_prices[i] <- last_price + mean_change
         } else if (rand < third_trans_mat[1, 1] + third_trans_mat[1, 2]) {
            # to inc dec
            predicted_prices[i] <- last_price - mean_change
         } else if (rand < third_trans_mat[1, 1] + third_trans_mat[1, 2] + 
                       third_trans_mat[1, 3]) {
            # to dec inc
            predicted_prices[i] <- last_price + mean_change
         } else {
            # to dec dec
            predicted_prices[i] <- last_price - mean_change
         }
      } else if (change_two_before >= 0 & change_before < 0) { # from inc dec
         if (rand < third_trans_mat[2, 1]) {
            # to inc inc 
            predicted_prices[i] <- last_price + mean_change
         } else if (rand < third_trans_mat[2, 1] + third_trans_mat[2, 2]) {
            # to inc dec
            predicted_prices[i] <- last_price - mean_change
         } else if (rand < third_trans_mat[2, 1] + third_trans_mat[2, 2] + 
                       third_trans_mat[2, 3]) {
            # to dec inc
            predicted_prices[i] <- last_price + mean_change
         } else {
            # to dec dec
            predicted_prices[i] <- last_price - mean_change
         }
      } else if (change_two_before < 0 & change_before >= 0) { # from dec inc
         if (rand < third_trans_mat[3, 1]) {
            # to inc inc 
            predicted_prices[i] <- last_price + mean_change
         } else if (rand < third_trans_mat[3, 1] + third_trans_mat[3, 2]) {
            # to inc dec
            predicted_prices[i] <- last_price - mean_change
         } else if (rand < third_trans_mat[3, 1] + third_trans_mat[3, 2] + 
                       third_trans_mat[3, 3]) {
            # to dec inc
            predicted_prices[i] <- last_price + mean_change
         } else {
            # to dec dec
            predicted_prices[i] <- last_price - mean_change
         }
      } else { # from dec dec
         if (rand < third_trans_mat[4, 1]) {
            # to inc inc 
            predicted_prices[i] <- last_price + mean_change
         } else if (rand < third_trans_mat[4, 1] + third_trans_mat[4, 2]) {
            # to inc dec
            predicted_prices[i] <- last_price - mean_change
         } else if (rand < third_trans_mat[4, 1] + third_trans_mat[4, 2] + 
                       third_trans_mat[4, 3]) {
            # to dec inc
            predicted_prices[i] <- last_price + mean_change
         } else {
            # to dec dec
            predicted_prices[i] <- last_price - mean_change
         }
      }
      change_two_before <- change_before
      if (i == 1) {
         change_before <- predicted_prices[i] - last_price
      } else {
         change_before <- predicted_prices[i] - predicted_prices[i-1]
      }
      last_price <- predicted_prices[i]
   }
   return(predicted_prices)
}

# plot the simulation against the actual values
plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
     xlab='Date', ylab='Closing Price', main='Simulation Using Third Model',
     xlim=c(all_dates[1], all_dates[length(all_dates)]),
     ylim=c(min(all_closing_prices), max(all_closing_prices)))
lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)

num_simulations <- 3
colors <- c('purple', 'green', 'red')
for (i in 1:num_simulations) {
   predicted_prices <- third_model_simulation()
   # simulated values
   lines(last_year_dates, predicted_prices, col=colors[i], lwd=1)
}
legend('topleft', legend=c('Training data', '2015 values', 
                           'simulation 1', 'simulation 2', 'simulation 3'), 
       col=c('blue', 'black', 'purple', 'green', 'red'), lwd=c(2, 2, 2, 2, 2))


# plot of averages
# plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
#      xlab='Date', ylab='Closing Price', main='Average of Simulations Using Third Model',
#      xlim=c(all_dates[1], all_dates[length(all_dates)]),
#      ylim=c(min(all_closing_prices), max(all_closing_prices)))
# lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)
# 
# num_simulations <- 250
# average_predicted_prices <- rep(0, times=length(last_year_closing_prices))
# for (i in 1:num_simulations) {
#    average_predicted_prices <- average_predicted_prices + third_model_simulation()  
# }
# average_predicted_prices <- average_predicted_prices / num_simulations
# lines(last_year_dates, average_predicted_prices, col='orange', lwd=2)


