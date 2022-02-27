df <- read.csv("C:\\Users\\Shivam\\Downloads\\kickstarter_projects.csv")
summary(df)
install.packages("lubridate")
library(lubridate)
df$ID = as.integer(df$ID) # Changing type of this variable as it will always be a full number
df$Launched = ymd_hms(df$Launched)
df$Deadline = ymd(df$Deadline)

# Creating additional variables for a weekday and a month
df$month = format(df$Launched,"%B")
df$year = format(df$Launched,"%Y")
df$month = factor(df$month, levels = c("January","February","March","April","May",
                                       "June","July","August","September",
                                       "October","November","December"))

# Creating the variables
goal_mean = round(mean(df$Goal),2)
goal_median = round(median(df$Goal),2)
goal_min = min((df$Goal),2)
goal_max = round(max(df$Goal),2)
goal_range = goal_max - goal_min
goal_sd = round(sd(df$Goal),2)

pledged_mean = round(mean(df$Pledged),2)
pledged_median = round(median(df$Pledged),2)
pledged_min = min((df$Pledged),2)
pledged_max = round(max(df$Pledged),2)
pledged_range = pledged_max - pledged_min
pledged_sd = round(sd(df$Pledged),2)

backers_mean = round(mean(df$Backers),2)
backers_median = round(median(df$Backers),2)
backers_min = min((df$Backers),2)
backers_max = round(max(df$Backers),2)
backers_range = backers_max - backers_min
backers_sd = round(sd(df$Backers),2)

# Creating the table
Variable = c("Goal", "Pledged", "Backers")
Mean = c(goal_mean, pledged_mean, backers_mean)
Median = c(goal_mean, pledged_median, backers_median)
sd = c(goal_sd, pledged_sd, backers_sd)
Minimum = c(goal_min, pledged_min, backers_min)
Maximum = c(goal_max, pledged_max, backers_max)
Range = c(goal_range, pledged_range, backers_range)

stats = data.frame(Variable, Mean, Median, sd, Minimum, Maximum, Range, 
                   stringsAsFactors=FALSE)

# Analyzing on histograms and boxplots:
par(mfrow=c(2,1))   # showing one graph under the other
par(mar=c(2,2,2,2)) # changing margins within the figure

boxplot(df$Backers, 
        horizontal = TRUE, 
        col = "blue2")

install.packages("RColorBrewer")
library(RColorBrewer)
hist(df$Backers,
     breaks = 20,
     col = brewer.pal(11, "Spectral"), 
     main = "Distribution of the numbers of backers",
     xlim = c(0,220000))

# Let's see if with time more projects were created
x = barplot(table(df$year), main = "In which year the most kickstarter projects were created?" 
            col = brewer.pal(9, "BuPu"),
            ylim = c(0,80000),
            xlab = "Year",
            ylab = "Number of projects")
text(y = table(df$year), 
     x, 
     table(df$year), 
     cex=0.8, 
     pos = 3)


x = barplot(table(df$month), 
            main = "What month corresponds to launching the most projects?", 
            col = brewer.pal(9, "YlGnBu"),
            ylim = c(0,40000),
            las=2,
            xlab = "",
            ylab = "")
text(y = table(df$month), 
     x, 
     table(df$month), 
     cex=0.8, 
     pos = 3)

x = barplot(table(df$State), 
            main = "How many projects are successful?"
            col = brewer.pal(9, "PuRd"),
            ylim = c(0,220000),
            xlab = "Status",
            ylab = "Number of projects")
text(y = table(df$State), 
     x, 
     table(df$State), 
     cex=0.8, 
     pos = 3)

# Now let's see average amount of pledged money by the category of a project
avg_pledged = tapply(df$Pledged, df$Category, mean)
dotchart(avg_pledged, pch = 22, bg = "dodgerblue2", cex = 1.3, 
         xlab="Average amount pledged",
         main = "What is the mean amount pledged \nfor projects from different categories?")

table(df$Category)

# Box plots
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = Category, y = Backers, fill = Category)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Spectral") + 
  labs(title = "The number of backers per project category", x = "Category", y = "Number of backer
s") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust=0.6))
