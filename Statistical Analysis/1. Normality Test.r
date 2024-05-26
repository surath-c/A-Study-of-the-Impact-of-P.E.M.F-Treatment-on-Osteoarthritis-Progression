## Normality test
f<- euclidian_df[c(paste0("W", 1:20), "W24")]
numeric_data <- sapply(f, as.numeric)
e<-as.matrix(numeric_data)
P_value <- numeric(21)
for (j in 1:21) {
  x <- e[, j]
  if(length(unique(x)) == 1) {
    P_value[j] <- 0.002
  } else {
    t <- shapiro.test(x)
    P_value[j] <- t$p.value
  }
}
columns=colnames(e)
normality_vector <- ifelse(P_value > 0.05, "Normal", "Not Normal")
normality_df <- data.frame(columns,P_value,normality_vector)
print(normality_df)