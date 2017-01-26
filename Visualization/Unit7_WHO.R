setwd("C:/Users/Бригада/Downloads/New/Analytical Edge/Unit 7 Visualization")

# Read in data
WHO = read.csv("WHO.csv")
str(WHO)

# Fertility rate VS Gross National Income
plot(WHO$GNI, WHO$FertilityRate)

# Let's redo this using ggplot 
# Install and load the ggplot2 library:
library(ggplot2)
# Create the ggplot object with the data and the aesthetic mapping:
g <- ggplot(WHO, aes(x = GNI, y = FertilityRate))
# Add the geom_point geometry
g + geom_point()
# Make a line graph instead:
g + geom_line()

# Switch back to our points:
g + geom_point()

# Redo the plot with blue triangles instead of circles:
g + geom_point(color = "blue", size = 3, shape = 17) 

# Another option:
g + geom_point(color = "darkred", size = 3, shape = 15) 

# Add a title to the plot:
g + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

# Save our plot:
fertilityGNIplot <- g + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()


# Color the points by region (factor): 
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Color the points according to life expectancy (continous):
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

# Let's try a log transformation:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model to predict the percentage of the population under 15, using the log of the fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(mod)

# Add this regression line to our plot:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

# 99% confidence interval
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

# Change the color of the regression line:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", colour = "orange")

#quiz
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point()
