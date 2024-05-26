# Subset the data into two data frames based on Sex
data_female <- subset(df, Sex == "F")
data_male <- subset(df, Sex == "M")
# Display the first few rows of each data frame
head(data_female)
head(data_male)
# calculation of group mean
column_means<-colMeans(df)
f_female<- data_female[c(paste0("W", 1:20), "W24")]
numeric_data_female <- sapply(f_female, as.numeric)
e_female<-as.matrix(numeric_data_female)
f_male<- data_male[c(paste0("W", 1:20), "W24")]
numeric_data_male <- sapply(f_male, as.numeric)
e_male<-as.matrix(numeric_data_male)
column_means_female <- colMeans(e_female)
column_means_male <- colMeans(e_male)
# Create the plot with extended y-axis limits
plot(column_means, type = "o", xlab = "Weeks", ylab = "Mean Value", 
     main = "Change in Mean Value over weeks", xaxt = "n", col = "red", pch = 16,
     ylim = c(min(c(column_means, column_means_female, column_means_male)), 
              max(c(column_means, column_means_female, column_means_male)) * 1.1))
# Add points for female and male means
lines(column_means, type = "o", col = "red", lwd = 2)
lines(column_means_female, type = "o", col = "black", lwd = 2,lty = 5)
lines(column_means_male, type = "o", col = "green4", lwd = 2,lty = 5)
# Add legend
legend("topright", legend = c("Overall Mean", "Mean for Female", "Mean for Male"), col = c("red", "black", "green4"), lty = c(1,3,3), lwd = 2, bty = "n")
# Add x-axis labels
axis(1, at = 1:length(column_means), labels = names(column_means), las = 2)
# Create a data frame for age 40-60
df_age_40_60 <- subset(df, Age >= 40 & Age <= 60)
# Create a data frame for age not in the range 40-60
df_age_less_40 <- subset(df, Age < 40 )
df_age_more_60 <- subset(df, Age > 60 )
# calculation of group mean
f_age_40_60<- df_age_40_60[c(paste0("W", 1:20), "W24")]
numeric_data_age_40_60<- sapply(f_age_40_60, as.numeric)
e_age_40_60<-as.matrix(numeric_data_age_40_60)
f_age_less_40 <- df_age_less_40[c(paste0("W", 1:20), "W24")]
numeric_data_age_less_40 <- sapply(f_age_less_40 , as.numeric)
e_age_less_40 <-as.matrix(numeric_data_age_less_40)
f_age_more_60<- df_age_more_60[c(paste0("W", 1:20), "W24")]
numeric_data_age_more_60<- sapply(f_age_more_60, as.numeric)
e_age_more_60<-as.matrix(numeric_data_age_more_60)
column_means_age_40_60 <- colMeans(e_age_40_60)
column_means_age_less_40 <- colMeans(e_age_less_40)
column_means_age_more_60 <- colMeans(e_age_more_60)
# Create the plot with extended y-axis limits
plot(column_means_age_40_60, type = "o", xlab = "Weeks", ylab = "Mean Value", 
     main = "Change in Mean of different group of Age Value over weeks", xaxt = "n", col = "red", pch = 16,
     ylim = c(min(c(column_means_age_40_60, column_means_age_less_40, column_means_age_more_60)), 
              max(c(column_means_age_40_60, column_means_age_less_40, column_means_age_more_60)) * 1.1))
# Add points for female and male means
lines(column_means_age_40_60, type = "o", col = "red", lwd = 2)
lines(column_means_age_less_40, type = "o", col = "green4", lwd = 3,lty = 3)
lines(column_means_age_more_60, type = "o", col = "violetred", lwd = 3,lty = 2)
# Add legend
legend("topright", legend = c("Mean for Age 40-60", "Mean for Age < 40", "Mean for Age > 60"), col = c("red", "green4", "violetred"), lty = c(1,3,2), lwd = 2, bty = "n")
# Add x-axis labels
axis(1, at = 1:length(column_means), labels = names(column_means), las = 2)
