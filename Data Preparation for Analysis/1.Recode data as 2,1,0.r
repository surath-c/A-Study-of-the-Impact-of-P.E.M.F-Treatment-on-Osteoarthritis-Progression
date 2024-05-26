# Load necessary library
library(readr)

# Read the dataset (assuming it's in a CSV file named 'data.csv')
# Replace 'data.csv' with your actual file name
oa <- read_csv(choose.files())

# Display the first few rows of the original data
head(oa)

# Recode the data
recode_data <- function(x) {
  ifelse(x == "Mo", 2, ifelse(x == "Mi", 1, ifelse(x == "Ni", 0, x)))
}

# Apply the recoding function to the entire data frame
o<- as.data.frame(lapply(oa, recode_data))

# Display the first few rows of the recoded data
head(o)

