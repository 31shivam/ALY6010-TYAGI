##Loading Data into R
df <- read.csv("C:\\Users\\Shivam\\Downloads\\Lake_Partners4.csv")
summary(df)
str(df)

## NA's with means
for(i in 1:ncol(df)){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}

##boxplot for removing outliers
boxplot(df$Chloride..mg.L., main="Box-Plot",ylab="Chloride values")

outliers <- boxplot(df$Chloride..mg.L., plot=FALSE)$out
x<-df 
x<- x[-which(x$Chloride..mg.L. %in% outliers),]
df <- x

##QQ plot
qqnorm(df$Chloride..mg.L., pch = 1, frame = FALSE)
qqline(df$Chloride..mg.L., col = "steelblue", lwd = 2)

summary(df$Chloride..mg.L)

##Test 1 One-Sample T-Tests.
t.test(df$Chloride..mg.L.,mu=9)

## sampling out on the basis of Lakes needed for Test 2
samp1 <- df %>% filter(Lake.Name=="HURON LAKE")
samp2 <- df %>% filter(Lake.Name=="SUPERIOR LAKE")         

##Test 2 for P-value
df_2 <- t.test(samp1$Chloride..mg.L., samp2$Chloride..mg.L., var.equal = TRUE)
df_2
