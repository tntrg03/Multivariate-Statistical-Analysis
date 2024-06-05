df <- data.frame(patient=rep(1:5, each=4),
                 drug=rep(c("A","B","C","D"), times=5),
                 response=c(30, 28, 16, 34,
                            14, 18, 10, 22,
                            24, 20, 18, 30,
                            38, 34, 20, 44,
                            26, 28, 14, 30))

df
#fit repeated measures ANOVA model
model <- aov(response~factor(drug)+Error(factor(patient)), data = df)

#view model summary
summary(model)

summary<-df %>%
  group_by(drug) %>%
  get_summary_stats(response, type = "mean_sd")
data.frame(summary)

install.packages("dplyr")
library(dplyr)
install.packages("rstatix")
library(rstatix)


