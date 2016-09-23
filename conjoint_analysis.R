#' ---
#' title: "Conjoint Analysis"
#' author: 
#'  name: "Amanda Dobbyn"
#'  email: aedobbyn@uchicago.edu
#' output:
#'  html_document:
#'    fig_caption: yes
#'    keep_md: true
#' ---

#+
# sample conjoint analysis setup
# based on Aizaki & Nishimura (2008)


# ------ set up the two design codes -----

library(AlgDesign)

# create the full factorial design
# a 2 x 2 x 4
ffd <- gen.factorial(levels = c(2, 2, 4), varNames = c("hac", "eco", "price"), 
                     factors = "all")


# set same seed as in paper for replicability
set.seed(54321)

# pare down ffd to 8 rows using Federov's exchange algorithm
des <- optFederov(~ ., ffd, 8) # all variables in data to be used linearly

# pull out the design from the list contained in des and store in object alt1
alt1 <- des$design

# make a copy of alt1
alt2 <- alt1


library(dplyr)
# add a column containing a random float between 0 and 1 to alt1
alt1 <- alt1 %>% 
  mutate(
    r1 = runif(nrow(alt1))
  )

# order rows by r1 ascending
alt1 <- alt1 %>% 
  arrange(r1)


# same for alt2
alt2 <- alt2 %>% 
  mutate(
    r2 = runif(nrow(alt2))
  ) %>% 
  arrange(r2)



# ------- recreate Table 4 --------

# create a random dataset for 10 participants
# 8 rows, 10 columns, values that vary from 1 to 3
df <- data.frame(replicate(8, 
                           (sample(c(1:3), 10, replace = TRUE))
))

# give columns names from Q1 to Q8
names(df) <- c(paste0("Q", 1:8))

# add a column for gender that randomly gives 1 or 0
df[(ncol(df) + 1)] <- sample(c(0:1), 10, replace = TRUE)

# rename the last column Gender
names(df)[ncol(df)] <- c("Gender")



# ------ read in dataset ------

# library(readr)
# dat <- read_csv("./fake_dat.csv")

# check out structure with str(dat) and change datatypes as necessary


# since we don't have a dataset...
# ------- generate fake data --------

# ----- str
# create the str variable
# we need to repeat each question 3 times for each respondent
# 10 respondents, 8 questions
# respondent is the first digit question is the second two digits
ones <- rep(1:8, each=3)
hundreds <- c(1:10)

# add (each value in hundreds)*100 to (each value in ones)
out <- sapply(ones, function(x) x + hundreds*100)

# make out a vector
out <- as.vector(out)

# sort into ascending order
str <- sort(out)

# make this the first column in our dataframe
dat <- data.frame(str=str)


# ----- res, asc, hac, and eco
# sample randomly from the vector c(0, 0, 1) 80 times
# make the result a vector

res <- replicate(80, sample(c(0, 0, 1), replace = FALSE)) # replace = FALSE because Ss can only choose 1 once and others must be 0
res <- as.vector(res)

asc <- replicate(80, c(1, 1, 0)) # asc is alternative specific constant (always 1 for A and B and 0 for neither) so no need for sample()
asc <- as.vector(asc)

hac <- replicate(80, sample(c(0, 0, 1), replace = TRUE)) # replace = TRUE because we can have more than one 1 per every 3 consecutive rows
hac <- as.vector(hac)

eco <- replicate(80, sample(c(0, 0, 1), replace = TRUE))
eco <- as.vector(eco)

# make these into a temporary dataframe
temp <- data.frame(res=res, asc=asc, hac=hac, eco=eco)

# add these variables to our dataframe
dat <- cbind(dat, temp)

# ----- price
# make a function that selects two of the four prices at random and adds 0
make_price <- function() {
  prices <- seq(145, 160, by=5)
  pick_two_prices <- sample(prices, 2, replace=FALSE)
  p <- c(pick_two_prices, 0) # add 0 for the third price
  p
}

# make a price column
price <- as.vector(replicate(80, make_price()))

# and add the price column to our dataframe
dat <- cbind(dat, price)


# ----- gender
# add a column for gender that randomly gives 1 or 0
dat[(ncol(dat) + 1)] <- sample(c(0:1), 240, replace = TRUE)

# rename the last column Gender
names(dat)[ncol(dat)] <- c("Gender")

# check out our dataframe
head(dat)




# ------- apply condiditional logit model -------

library(survival)

# make a model without demographic variable
clogout1 <- clogit(res ~ asc + hac + eco + price + strata(str), data = dat)

# check out the coefficients
clogout1

# extract out the log likelihood
# first value is at 0 and second is at convergence
clogout1$loglik


# make a model including the interaction between price and Gender (price:Gender)
clogout2 <- clogit(res ~ asc + hac + eco + price
                   + price:Gender + strata(str), data = dat)
clogout2

# extract out the log likelihood
clogout2$loglik



