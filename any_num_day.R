# install.packages("expm")
library(expm)
library(compositions)

# set working derectory to the same as the data
setwd("~/math_381_group/markov_stocks")

# read in the data
data <- read.csv('goog.csv')
#data <- read.csv('pbr_a.csv')

# set date at which to stop training data and start simulation
stop_date <- "2015-01-01"
#stop_date <- "2015-11-12"

# read in closing prices and training_dates
all_dates <- rev(as.Date(data$Date, format='%d-%b-%y'))
training_dates <- all_dates[all_dates < stop_date]
last_year_dates <- all_dates[all_dates >= stop_date]

all_closing_prices <- rev(data$Close)
training_closing_prices <- all_closing_prices[all_dates < stop_date]
last_year_closing_prices <- all_closing_prices[all_dates >= stop_date]

# compute the daily change in the stock
daily_change <- diff(training_closing_prices)


# # create a map sending a state to an index
# # uuu -> 1  (up, up, up) 
# # uud -> 2
# # udu -> 3
# # udd -> 4
# # duu -> 5
# # dud -> 6
# # ddu -> 7
# # ddd -> 8
# s_to_i <- vector(mode="list", length=8)
# names(s_to_i) <- c('uuu', 'uud', 'udu', 'udd',
#                    'duu', 'dud', 'ddu', 'ddd')
# for (i in 1:8) {
#    s_to_i[[i]] <- i
# }

# function takes a vector of daily changes and returns
# a vector of binary representations of those changes
changes_to_bin <- function(changes, num_days) {
   daily_u_d <- vector(length=length(changes))
   for (i in 1:length(changes)) {
      if (changes[i] >= 0) {
         daily_u_d[i] <- '0'
      } else {
         daily_u_d[i] <- '1'
      }
   }
   state_vector <- vector(length=(length(daily_u_d)-(num_days-1)))
   for (i in 1:(length(daily_u_d)-(num_days-1))) {
      this_state <- daily_u_d[i] 
      for (j in (i+1):(i+(num_days-1))) {
         this_state <- paste(this_state, daily_u_d[j], sep='')
      }
      state_vector[i] <- this_state
   }
   return(state_vector)
}

bin_to_index <- function(bin_string) {
   return(unbinary(bin_string) + 1)
}

make_trans_mat <- function(daily_change, num_days) {
   trans_mat <- matrix(rep(0, times=(2^num_days)^2), nrow=2^num_days, ncol=2^num_days)
   row_count <- rep(0, times=2^num_days)
   state_vector <- changes_to_bin(daily_change, num_days)
   for (i in 1:(length(state_vector)-1)) {
      row_index <- bin_to_index(state_vector[i])
      col_index <- bin_to_index(state_vector[i+1])
      trans_mat[row_index, col_index] <- trans_mat[row_index, col_index] + 1
      row_count[row_index] <- row_count[row_index] + 1
   }
   for (row in 1:nrow(trans_mat)) {
      trans_mat[row, ] <- trans_mat[row, ] / row_count[row]
   }
   return(trans_mat)
}

# determine tranistion matrices and compute stationary distributions
# for 3 and 5 day models
three_day_trans_mat <- make_trans_mat(daily_change, num_days=3)
three_day_trans_mat %^% 20

five_day_trans_mat <- make_trans_mat(daily_change, num_days=5)
five_day_trans_mat %^% 20

mean_change <- mean(abs(daily_change))

# simulation with any number of days
any_num_days_simulation <- function(num_days) {
   previous_states <- changes_to_bin(daily_change, num_days=num_days)
   last_state <- previous_states[length(previous_states)]
   last_price <- training_closing_prices[length(training_closing_prices)]
   predicted_prices <- vector(length=length(last_year_dates))
   trans_mat <- make_trans_mat(daily_change, num_days)
   #mean_change <- mean(abs(daily_change))
   
   #### variable incriments ####
   basic_mean <- mean(abs(daily_change))
   basic_sd <- sd(abs(daily_change))
   #### variable incriments ####
   
   for (i in 1:length(predicted_prices)) {
      
      #### variable incriments ####
      mean_change <- rnorm(1, mean=basic_mean, sd=basic_sd)
      #### variable incriments ####
      
      rand <- runif(1)
      last_state_row <- bin_to_index(last_state)
      probabilities <- trans_mat[last_state_row, ][trans_mat[last_state_row, ] > 0]
      if (rand < probabilities[1]) {
         next_state <- substr(last_state, start=2, stop=nchar(last_state))
         next_state <- paste(next_state, '0', sep='')
      } else {
         next_state <- substr(last_state, start=2, stop=nchar(last_state))
         next_state <- paste(next_state, '1', sep='')
      }
      last_change <- substr(next_state, start=nchar(next_state), stop=nchar(next_state))
      if (last_change == '0') {
         predicted_prices[i] <- last_price + mean_change
      } else {
         predicted_prices[i] <- last_price - mean_change
      }
      
      last_price <- predicted_prices[i]
      last_state <- next_state
   }
   
   return(predicted_prices)
}



# plot the simulation against the actual values
plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
     xlab='Date', ylab='Closing Price', main='Google Stock Simulation Using 5-day to 5-day Model',
     xlim=c(all_dates[1], all_dates[length(all_dates)]),
     ylim=c(min(all_closing_prices), max(all_closing_prices)))
lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)

num_simulations <- 3
colors <- c('purple', 'green', 'red')
for (i in 1:num_simulations) {
   predicted_prices <- any_num_days_simulation(3)
   # simulated values
   lines(last_year_dates, predicted_prices, col=colors[i], lwd=1)
}
legend('topleft', legend=c('Training data', '2015 values', 
                           'simulation trial 1', 'simulation trial 2', 'simulation trail 3'), 
       col=c('blue', 'black', 'purple', 'green', 'red'), lwd=c(2, 2, 2, 2, 2))


# # plot of averages
# plot(training_dates, training_closing_prices, type='l', col='blue', lwd=1,
#      xlab='Date', ylab='Closing Price', main='Average of Simulations',
#      xlim=c(all_dates[1], all_dates[length(all_dates)]),
#      ylim=c(min(all_closing_prices), max(all_closing_prices)))
# lines(last_year_dates, last_year_closing_prices, col='black', lwd=1)
# 
# num_simulations <- 100
# average_predicted_prices <- rep(0, times=length(last_year_closing_prices))
# for (i in 1:num_simulations) {
#    average_predicted_prices <- average_predicted_prices + any_num_days_simulation(5)  
# }
# average_predicted_prices <- average_predicted_prices / num_simulations
# lines(last_year_dates, average_predicted_prices, col='orange', lwd=2)


# # zoom in on last week
# last_date <- all_dates[length(all_dates)]
# plot(training_dates[(length(training_dates)-10):length(training_dates)], 
#      training_closing_prices[(length(training_dates)-10):length(training_dates)],
#      type='l', col='blue', lwd=2,
#      xlab='Date', ylab='Closing Price', main='',
#      xlim=c(training_dates[length(training_dates)-10], last_date),
#      ylim=c(min(c(training_closing_prices[(length(training_dates)-10):length(training_dates)], 
#                   last_year_closing_prices)), max(last_year_closing_prices)))
# lines(c(training_dates[length(training_dates)], last_year_dates), 
#       c(training_closing_prices[length(training_closing_prices)], 
#         last_year_closing_prices), 
#       col='black', lwd=2)
# 
# num_simulations <- 3
# colors <- c('purple', 'green', 'red')
# for (i in 1:num_simulations) {
#    predicted_prices <- any_num_days_simulation(5)
#    # simulated values
#    lines(c(training_dates[length(training_dates)], last_year_dates), 
#          c(training_closing_prices[length(training_closing_prices)], predicted_prices), 
#          col=colors[i], lwd=2)
# }
# legend('topleft', legend=c('Training data', '2015 values', 
#                            'simulation 1', 'simulation 2', 'simulation 3'), 
#        col=c('blue', 'black', 'purple', 'green', 'red'), lwd=c(2, 2, 2, 2, 2))


