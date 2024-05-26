## Exploratory Data Analysis 
## Date 24/02/2024
## @ Surath Chakraborti
## A Study of the Impact of P.E.M.F Treatment on Osteoarthritis Progression
eda<-read.csv(file.choose())
head(eda)
#for Sex
sex<-table(as.factor(eda$Sex))
# Calculate percentages
gender_percentage <- prop.table(sex) * 100
# Create pie chart with percentage labels
pie(sex, labels = paste( "\n", round(gender_percentage, 2), "%", sep = ""), col = rainbow(length(sex)))
# Add legend
legend("topright", legend = names(sex), fill = rainbow(length(sex)))
title("Sex of patients")
# Combine "Business" and "Businessman" into "Business"
eda$Occupation[eda$Occupation %in% c("Business", "Businessman")] <- "Business"
# Calculate frequencies and percentages
occu <- table(eda$Occupation)
if (any(occu < 5)) {
  # Get groups with count less than 5
  occu["Others"] <- sum(occu[occu < 5])
  occu <- occu[occu >= 5]
}
occu_percentage <- prop.table(occu) * 100
# Create pie chart with distinct colors for each group
pie(occu, labels = paste("\n", round(occu_percentage, 2), "%", sep = ""), col = rainbow(length(occu)))
# Add legend
legend("topleft", legend = names(occu), fill = rainbow(length(occu)), cex = 0.8)
title("Occupation of patients")
#for age 
ages<-eda$Age
# Create age groups
age_groups <- cut(ages, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70), labels = c("30-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))
# Count the frequencies
age_group_counts <- table(age_groups)
# Convert counts to percentages
age_group_percentages <- prop.table(age_group_counts) * 100
# Create pie chart with distinct colors for each group and percentage labels
pie(age_group_counts, labels = paste( "\n", round(age_group_percentages, 2), "%", sep = ""), col = rainbow(length(age_group_counts)))
# Add legend
legend("topleft", legend = paste(names(age_group_counts)), fill = rainbow(length(age_group_counts)), cex = 0.9)
title("Age of patients")
#for age for female
ages1<-eda$Age[eda$Sex == "F"]
# Create age groups
age_groups1 <- cut(ages1, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70), labels = c("30-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))
# Count the frequencies
age_group_counts1 <- table(age_groups1)
# Convert counts to percentages
age_group_percentages1 <- prop.table(age_group_counts1) * 100
# Create pie chart
# Create pie chart with distinct colors for each group and percentage labels
pie(age_group_counts, labels = paste( "\n", round(age_group_percentages1, 2), "%", sep = ""), col = rainbow(length(age_group_counts1)))
# Add legend
legend("topleft", legend = paste(names(age_group_counts1)), fill = rainbow(length(age_group_counts1)), cex = 0.9)
title("Age of female patient")
#for age for male
ages2<-eda$Age[eda$Sex == "M"]
# Create age groups
age_groups2 <- cut(ages2, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70), labels = c("30-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))
# Count the frequencies
age_group_counts2 <- table(age_groups2)
# Convert counts to percentages
age_group_percentages2 <- prop.table(age_group_counts2) * 100
# Create pie chart with distinct colors for each group and percentage labels
pie(age_group_counts, labels = paste( "\n", round(age_group_percentages2, 2), "%", sep = ""), col = rainbow(length(age_group_counts2)))
# Add legend
legend("topleft", legend = paste(names(age_group_counts2)), fill = rainbow(length(age_group_counts2)), cex = 0.9)
title("Age of Male patient")
#for age for female
ages3<-eda$Age[eda$Occupation == "Housewife"]
# Create age groups
age_groups3 <- cut(ages3, breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70), labels = c("30-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70"))
# Count the frequencies
age_group_counts3 <- table(age_groups3)
# Convert counts to percentages
age_group_percentages3 <- prop.table(age_group_counts3) * 100
# Create pie chart with distinct colors for each group and percentage labels
pie(age_group_counts, labels = paste( "\n", round(age_group_percentages3, 2), "%", sep = ""), col = rainbow(length(age_group_counts3)))
# Add legend
legend("topleft", legend = paste(names(age_group_counts3)), fill = rainbow(length(age_group_counts3)), cex = 0.9)
title("Age  of Housewife")
 
