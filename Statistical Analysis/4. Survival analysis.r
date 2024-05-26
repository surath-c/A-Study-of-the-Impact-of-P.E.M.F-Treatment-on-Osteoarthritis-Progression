### Survival Analysis
install.packages("survival")
install.packages("flexsurv")
install.packages("eha")
library(survival)
library(flexsurv)
library(eha)
# read the data for survival analysis
sur<-read.csv(file.choose())
sur
#cox Proportional Hazard Model.
survival_model1<-coxph(Surv(start,stop,succes)~distance+age+sex,data=sur)
summary(survival_model1)
# success curve of the cox model
survival_prob <- survfit(survival_model1)
plot(survival_prob, xlab = "Week", ylab = "1-Succes Probability",main = "Succes Curve from Cox Model")
## hazard plot

# Extract the baseline hazard
baseline_hazard <- basehaz(survival_model1, centered = FALSE)


# Predict the linear predictor for each individual in the data
lp <- predict (survival_model1 , type = " lp " )
# Calculate the hazard function for each individual
hazard_function <- baseline_hazard $ hazard * exp ( lp )
# Plot the baseline hazard function
plot(baseline_hazard$time,baseline_hazard$hazard , type = "l" , col = "green" ,xlab = " Time " , ylab = " Hazard " , main = " Baseline and Individual
Hazard Functions " ,lwd = 4 , ylim =c(min(baseline_hazard$hazard ) , max(hazard_function)))
# Add individual hazard functions to the plot
for ( i in 1: nrow ( sur )) {
  lines(baseline_hazard$time , baseline_hazard$hazard * exp (lp[i]) , col =rgb (1 , 0 , 0 , alpha = 0.1) , lty =1)
}
# Adding a legend
legend ("topleft" , legend = c ("Baseline Hazard" , "Individual Hazards" ) , col = c("green" , "red" ) , lty = 1 , lwd = 2 , bty = " n " )
grid ()
