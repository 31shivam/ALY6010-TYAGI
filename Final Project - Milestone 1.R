df <- read.csv("C:\\Users\\Shivam\\Downloads\\score.csv")
summary(df)
file.info("C:\\Users\\Shivam\\Downloads\\score.csv")$size
str(df)
library(dplyr)
##removing duplicates
df <- df %>% distinct(student_id, .keep_all = TRUE)

##removing age
df <- df[-c(which(df$age< 5)), ]

##Removing String inconsistencies
levels(df$tuition)[levels(df$tuition)=="Y"] <-"Yes"
levels(df$tuition)[levels(df$tuition)=="N"] <-"No"
levels(df$CCA)[levels(df$CCA)=="ARTS"] <-"Arts"
levels(df$CCA)[levels(df$CCA)=="CLUBS"] <-"Clubs"
levels(df$CCA)[levels(df$CCA)=="NONE"] <-"None"
levels(df$CCA)[levels(df$CCA)=="SPORTS"] <-"Sports"

## filling NA with means
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

##dropping irrelevant columns
df <- select (df,-c(n_male,n_female))

#removing outliers
hist(df$age, main = "Histogram of Age", col = "blue", xlab="Age")

##age
outliers <- boxplot(df$age, plot=FALSE)$out
x<-df 
x<- x[-which(x$age %in% outliers),]
hist(x$age, main = "Histogram of Age", col = "blue", xlab="Age")
df <- x

##jitter plot for attendance rate vs sleeping time 
ggplot(df, aes(x = sleep_time, y = attendance_rate)) +
  geom_jitter()


##stack plot for bag colours
ggplot(df, 
       aes(x = bag_color, 
           fill = gender)) + 
  geom_bar(position = "stack") + labs(title = "Stacked chart between Bag_Color and Gender")

##plotting jitter for Final test makes VS attendance Rate with Keeping gender as a factor of differentiation 
ggplot(df, aes(y = final_test, x = attendance_rate)) +
  geom_jitter(aes(color=gender))

##plotting hours per week vs CCA activities
ggplot(df, aes(x=hours_per_week, color = CCA, fill = CCA))+
  geom_histogram( position = "identity",alpha = 0.5)

## Jitter plot between Mode of Transport and the Waking up time
ggplot(df, aes(x = mode_of_transport, y = wake_time)) +
  geom_jitter()


ggplot(df, aes(x=hours_per_week, color = gender, fill = gender))+
  geom_histogram( position = "identity",alpha = 0.5) 

summary(df)

##Subsetting out Data
sub <- df[c(1000:2000),c(4,7,9,12)]

## CCA V/S Attendance Rate
ggplot(sub, aes(x=attendance_rate, color = CCA, fill = CCA))+
  geom_histogram( position = "identity",alpha = 0.5)

ggplot(sub, aes(x=attendance_rate, y=CCA) ) +
  geom_bin2d() +
  theme_bw()

##grouped box plot with mean
ggplot(sub, aes(y=attendance_rate, x=CCA, fill=gender)) + 
  geom_boxplot(alpha=0.7)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="green", fill="green") +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")


