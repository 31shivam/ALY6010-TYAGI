##MODULE 2
data <- read.csv("C:\\Users\\Shivam\\Downloads\\StudentsPerformance.csv")
summary(data)
m1 <- mean(data$math.score)
m2 <- mean(data$reading.score)
m3 <- mean(data$writing.score)
s1 <- sd(data$math.score)
s2 <- sd(data$reading.score)
s3 <- sd(data$writing.score)
mi1 <- min(data$math.score)
mi2 <- min(data$reading.score)
mi3 <- min(data$writing.score)
ma1 <- max(data$math.score)
ma2 <- max(data$reading.score)
ma3 <- max(data$writing.score)
datat <- matrix(c(m1,m2,m3,s1,s2,s3,mi1,mi2,mi3,ma1,ma2,ma3),ncol=3,byrow = TRUE)
rownames(datat) <- c("mean","sd","min","max")
colnames(datat) <- c("Math.score", "Reading.score", "Writing.score")
datat <- as.table(datat)
datat
library(dplyr)
group_by(data,gender,race.ethnicity)
library(psych)
psych::describe(data$math.score)
psych::describe(data$reading.score)
psych::describe(data$writing.score)

#histograms and boxplots
par(mfrow = c(3,2))
hist(data$math.score,main = "Histogram : Math_Score",col = "red")
boxplot(data$math.score ~ data$gender, horizontal = TRUE,main = "Boxplot : Math_Score",col = "red")
hist(data$reading.score,main = "Histogram : Reading_Score",col = "violet")
boxplot(data$reading.score ~ data$gender, horizontal = TRUE,main = "Boxplot : Reading_Score",col = "violet")
hist(data$writing.score,main = "Histogram : Writing_Score",col = "blue2")
boxplot(data$writing.score ~ data$gender, horizontal = TRUE,main = "Boxplot : Writing_Score",col = "blue2")



#Math Vs. Reading
reg<-lm(math.score ~ reading.score, data = data)
plot(data$math.score, data$reading.score, pch = 19, col = c("blue","green"),main = "Math Score Vs. Reading Score",
     xlab = "Reading Score",ylab = "Math Score")
legend(x=10,y=100,c("Female","Male"),cex=.8,col=c("blue","green"),pch=c(19,19))
abline(reg, col="red")

#Reading Vs. Writing
reg<-lm(reading.score ~ writing.score, data = data)
plot(data$reading.score, data$writing.score, pch = 19, col = c("blue","green"),main = "Writing Score Vs. Reading Score",
     xlab = "Writing Score",ylab = "Reading Score")
legend(x=20,y=100,c("Female","Male"),cex=.8,col=c("blue","green"),pch=c(19,19))
abline(reg, col="black")

#Math vs. Writing

reg<-lm(math.score ~ writing.score, data = data)
plot(data$math.score, data$writing.score, pch = 19,col=c("blue","green"),main = "Math Score Vs. Writing Score",
     xlab = "Writing Score",ylab = "Math Score")
legend(x=10,y=100,c("Female","Male"),cex=.8,col=c("blue","green"),pch=c(19,19))
abline(reg, col="black")

ggplot(data, aes(x = math.score, y = reading.score)) +
  geom_jitter() +
  stat_summary(
    fun = median,
    geom = "point",
    size = 2,
    color = "red"
  )  
  labs(
    title = "Comparison of Math and Reading Score",
    x = "Math Score",
    y = "Reading Score"
  
  )
  install.packages("cowplot")
  library(cowplot)
  graph1 <- ggplot(data, aes(x = math.score, y = reading.score)) +
    geom_jitter(aes(colour = race.ethnicity))
  graph2 <- ggplot(data, aes(x = reading.score, y = writing.score)) +
    geom_jitter(aes(colour = race.ethnicity)) 
  graph3 <- ggplot(data, aes(x = writing.score, y = math.score)) +
    geom_jitter(aes(colour = race.ethnicity))
  plot_grid(graph1, graph2, graph3, labels = "AUTO")              
  