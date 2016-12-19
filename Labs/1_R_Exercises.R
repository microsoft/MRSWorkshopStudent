


###################################################
# Mini R Tutorial
###################################################



# Welcome to R! This is an R script. If a line of code in R starts
# with a hash, like these, R will ignore it and you can type whatever
# you like.

# In addition to this script, you should also see the R console - 
# probably with some information about which version of R you're using.
# The console reads R code and runs the instructions.
# You can send code from this script to the console using Ctrl+Enter.
# Here's a very simple R function - send it to the console with
# Ctrl+Enter:
seq(1, 10)

# seq() generates a sequence of integers - in this case, from 1 to 10.
# You can see the built-in documentation for the function with this:
?seq

# The results print in the console, but to use them, we need to 
# assign them to our workspace with this funky arrow operator
# (Try pressing alt-hyphen to create one)
x <- seq(1, 10)

x



# Now the values persist in the workspace. We can see what's in the
# workspace using the ls() command:
ls()


# In R, a sequence of numbers or characters is called a vector.
# We can do a variety of math operations on it:
x + 5
x * -1
x^3
length(x)
sum(x)


###################
# Exercise 1: Mess with Vectors
###################

# Create a different vector of ten numbers called "y".
# What do you think will happen if you subtract y from x?
# Try it:





# Most data is still tabular, so we'll usually work with collections
# of vectors called "data frames" - and we'll usually create them by
# reading in tabular data from somewhere else:
flights <- read.csv("flights_q1.csv")


# There are a few useful ways to inspect data frames:
head(flights)
nrow(flights)
str(flights)
summary(flights)



# What makes a plane late? Let's inspect a few possible relationships
# using the superb ggplot2 package. 
# Packages are stored in your library:
library(ggplot2)

# First, use ggplot() and aes() to specify your dataset and 
# variables, then add your chosen geometry:
ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
    geom_point()


# Try another geometry
ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
    geom_point() +
    geom_smooth()



###################
# Exercise 2: Plot arr_delay vs. categorical variable
###################

# Identify a categorical variable.
# What kind of plot would be suitable for comparing arr_delay
#    between groups?
# Try to build it in ggplot










# This dataset is missing one really obvious predictor: day of week.
# We have the date of each flight, split into three columns.
# To calculate day of week, we need to combine them into a proper
# Date variable, and then format for day of week.

# First, let's inspect each variable. We can access variables in
# a data frame with the dollar sign:
flights$year


# But it might be more useful to see a table:
table(flights$year)
table(flights$month)
table(flights$day)






# paste() is a powerful function for wrangling text data.
# Just pass it the names of all the variables you want to combine:
paste(flights$year, flights$month, flights$day)

# It would probably look nicer with a different separator, though:
paste(flights$year, flights$month, flights$day, sep = "-")


# Next, convert the pasted-together date into a Date with as.Date:
flights$flight_date <- as.Date(
    paste(flights$year, flights$month, flights$day, sep = "-")
)



# Check the results
flights$flight_date


# Now, format() that Date to display day of the week.
# Most of R's date and time functions rely on the POSIX formats,
# which are easiest to look up with:
?strptime

# Looks like we want %A.
flights$dayOfWeek <- format(flights$flight_date, format = "%A")

# Check the results
table(flights$dayOfWeek)




# Looks good! Except... the days aren't in the right order.
# In R, ordering categories (aka levels) is a job for factors:
?factor

# I just need to specify the order I want:
flights$dayOfWeek <- factor(flights$dayOfWeek, 
    levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday")
)

# Check again
table(flights$dayOfWeek)







###################
# Exercise 3: Calculate Speed
###################

# To make sure you've got the hang of creating variables,
# calculate each flight's speed in miles per hour














# Let's build a model! Here's a very basic regression:
mod1 <- lm(arr_delay ~ dep_delay + dayOfWeek, data = flights)
summary(mod1)
AIC(mod1)



###################
# Exercise 4: Model Arrival Delay
###################

# Build a model to your satisfaction.
yourmod <- 



# When you're happy, test its performance on this dataset:
flights_q234 <- read.csv("flights_q234.csv")

# Using the predict() function:
predict(yourmod, newdata = flights_q234)







