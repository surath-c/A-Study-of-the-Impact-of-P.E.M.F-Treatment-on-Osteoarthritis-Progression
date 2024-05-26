install.packages("readxl")
install.packages("philentropy")
library(readxl)
library(dplyr)
library(philentropy)
library(stats)
o<-as.data.frame(read_xls(choose.files()))
head(o,20)
# Define the vector (0, 0, 0, 0, 0)
zero_vector <- c(0, 0, 0, 0, 0)
# Initialize an empty data frame to store the euclidean distances
euclidean_df <- data.frame(Patient_No. = integer(),
                           Age = integer(),
                           Sex = character(),
                           W1 = numeric(),
                           W2 = numeric(),
                           W3 = numeric(),
                           W4 = numeric(),
                           W5 = numeric(),
                           W6 = numeric(),
                           W7 = numeric(),
                           W8 = numeric(),
                           W9 = numeric(),
                           W10 = numeric(),
                           W11 = numeric(),
                           W12 = numeric(),
                           W13 = numeric(),
                           W14 = numeric(),
                           W15 = numeric(),
                           W16 = numeric(),
                           W17 = numeric(),
                           W18 = numeric(),
                           W19 = numeric(),
                           W20 = numeric(),
                           W24 = numeric())
# Iterate over each patient
for (i in unique(o$`Patient No.`)) {
  # Extract the data for the current patient
  patient_data <- o[o$`Patient No.` == i, c("Age", "Sex", paste0("W", 1:21))]
  # Extract Age and Sex
  age <- patient_data$Age[1]
  sex <- patient_data$Sex[1]
  patient_data <- patient_data[, -c(1, 2)]  # Remove Age and Sex columns from patient_data
  
  euclidean_dist <- numeric(21)
  
  # Loop through each column of the patient data
  for (j in 1:21) {
    x <- patient_data[, j]
    
    d <- round(dist(rbind(x, zero_vector), method = "euclidean"), digits = 5)
    euclidean_dist[j] <- d
  }
  # Add the euclidean distances to the data frame
  euclidean_df <- rbind(euclidean_df, c(i, age, sex, euclidean_dist))
}
# Rename the columns
colnames(euclidean_df) <- c("Patient_No.", "Age", "Sex", paste0("W", 1:20), "W24")
# Print the euclidean distances for all patients
print(euclidean_df)