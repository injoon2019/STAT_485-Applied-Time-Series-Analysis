rm(list=ls()) # Clear the Environment / History

install.packages("TSA") # Errors?

install.packages("leaps")
install.packages("locfit")

# Go to https://cran.r-project.org/src/contrib/Archive/TSA/ and click on "TSA_1.2.tar.gz"

# ---- You have to specify the location where the file downloaded. For me it is
#     "/Users/trevorthomson/Downloads/", 
#           for Mac users, its probably got a similar name,
#           for Windows users, probably starts with something like "C:/Users/..."
#   type in "getwd()", cut and paste the directory and add in "Downloads/TSA_1.2.tar.gz"

install.packages('/Users/trevorthomson/Downloads/TSA_1.2.tar.gz', 
                 lib= .libPaths(), 
                 repos = NULL)

library(TSA)

# -----------------------------------------------------------------------
#  1.   Plot "larain" dataset
# larain: Annual precipitation (in inches) in Los Angeles, 1878-1992
# -----------------------------------------------------------------------

data(larain) # load the dataset
class(larain)
larain
#?ts

plot(larain,
     ylab='Inches',
     xlab='Year',
     type='o')

# ----- Question: Do you see any patterns?

# To examine the relationship between successive years of rainfall,
# plot each year's rainfall on the y-axis, and previous year in the x-axis

# zlag(larain) will help!

plot(x=zlag(larain),
     y=larain,
     xlab='Previous Year Inches',
     ylab='Inches',)


# ----- Question: Do you see any patterns?



# -----------------------------------------------------------------------
#  2.   Plot "hare" dataset
# hare: These are yearly hare abundances for the main drainage of 
#       the Hudson Bay, based on trapper questionnaires.
# -----------------------------------------------------------------------

data(hare)
hare

plot(hare,
     xlab='Year',
     ylab='Abundance',
     type='o')

# ----- Question: Do you see any patterns?

# Let's plot each year's hare abundance on the y-axis and
#   previous year's hare abunfance on the x-axis

plot(x=zlag(hare),
     y=hare,
     xlab='Previous Year Abundance',
     ylab='Abundance')

# ----- Question: Do you see any patterns?



# -----------------------------------------------------------------------
#  3.   Load "color" dataset
# color: Color property from 35 consecutive batches in an industrial process
# -----------------------------------------------------------------------

data(color)
color

# -----------------------------------------------------------------------
#  4.   Load "tempdub" dataset
# tempdub: Monthly average temperature (in degrees Fahrenheit) recorded in
#           Dubuque, Iowa, USA from January 1964 to December 1975.
# -----------------------------------------------------------------------

data(tempdub)
tempdub


# -----------------------------------------------------------------------
#  5.   Load "oilfilters" dataset
# oilfilters: Monthly wholesale specialty oil filters sales, 
#             Deere & Co. from July 1983 to June 1987.
# -----------------------------------------------------------------------

data(oilfilters)
oilfilters
