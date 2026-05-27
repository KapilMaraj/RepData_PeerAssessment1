# 1. Loading and preprocessing the data
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format="%Y-%m-%d")

# 2. Mean total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data = data, FUN = sum, na.rm = TRUE)
hist(steps_per_day$steps, main = "Total Steps Taken Each Day", xlab = "Total Steps", col = "blue", breaks = 20)
print(mean(steps_per_day$steps))
print(median(steps_per_day$steps))

# 3. Average daily activity pattern
steps_per_interval <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)
plot(steps_per_interval$interval, steps_per_interval$steps, type = "l", 
     main = "Average Daily Activity Pattern", xlab = "5-Minute Interval", ylab = "Average Steps", col = "darkgreen")
print(steps_per_interval$interval[which.max(steps_per_interval$steps)])

# 4. Imputing missing values
print(sum(is.na(data$steps)))
imputed_data <- data
for (i in 1:nrow(imputed_data)) {
  if (is.na(imputed_data$steps[i])) {
    interval_val <- imputed_data$interval[i]
    steps_val <- steps_per_interval$steps[steps_per_interval$interval == interval_val]
    imputed_data$steps[i] <- steps_val
  }
}
imputed_steps_per_day <- aggregate(steps ~ date, data = imputed_data, FUN = sum)
hist(imputed_steps_per_day$steps, main = "Total Steps Each Day (Imputed Data)", xlab = "Total Steps", col = "red", breaks = 20)
print(mean(imputed_steps_per_day$steps))
print(median(imputed_steps_per_day$steps))

# 5. Differences in activity patterns between weekdays and weekends
imputed_data$day_type <- weekdays(imputed_data$date)
imputed_data$day_type <- ifelse(imputed_data$day_type %in% c("Saturday", "Sunday"), "weekend", "weekday")
imputed_data$day_type <- as.factor(imputed_data$day_type)
panel_data <- aggregate(steps ~ interval + day_type, data = imputed_data, FUN = mean)

library(lattice)
xyplot(steps ~ interval | day_type, data = panel_data, type = "l", layout = c(1, 2),
       main = "Weekday vs. Weekend Activity Patterns", xlab = "5-Minute Interval", ylab = "Average Steps")
# 