setwd("/Users/meredithsanders/Downloads/mTurk-comparison-data")

library(readr)
library(dplyr)
library(mirt)
library(matrixcalc)
library(RobustIRT)


########################## Data Processing #####################################

# load amt data
amt_missing <- read_tsv("data_amt.csv")

# extract only Q1–Q26
items <- amt_missing %>% select(Q1:Q26)
ind <-apply(items, 1, function(x) all(x!=0))
items<- items[ind,] # 1203 out of 1402

# convert to a numeric matrix for IRT
amt <- as.matrix(items)
amt_converted <- items %>% select(Q1:Q10)
amt_converted <- as.matrix(amt_converted)

#reverse code items properly
K <- 5  # maximum Likert category
irc_items <- c(2, 3, 7, 9) # items that are negatively worded (imply individual is short, not tall)

for (j in irc_items) {
  amt_converted[, j] <- K + 1 - amt_converted[, j]
}

# remove aberrant responders
mod_full <- mirt(amt_converted, 1, itemtype = "graded")
per.fit <- personfit(mod_full, method = "ML")$Zh
flagged <- which(per.fit < -1.64)
items_cleaned <- amt_converted[-flagged, ]

# fit a graded response model with the cleaned data
mod_clean <- mirt(items_cleaned, 1, itemtype = "graded")

# extract item parameters
pars <- coef(mod_clean, IRTpars = TRUE, simplify = TRUE)$items

# convert to matrix
a <- as.matrix(pars[, "a"])
rownames(a)<-NULL
# convert to numeric
d <- matrix(as.numeric(pars[, c("b1", "b2", "b3", "b4")]), nrow=10)
rownames(d)<-NULL
colnames(d)<-NULL

############################ Theta Estimation ##################################

# estimate for everyone, aberrant and not

mle <- theta.est.grm(amt_converted, a, d, iter=30, cutoff=.01, init.val=0, weight.type = "equal", tuning.par=NULL)
theta_mle <- mle$theta[, 1]

bisquare <- theta.est.grm(amt_converted, a, d, iter=30, cutoff=.01, init.val=0, weight.type = "bisquare", tuning.par=4)
theta_bisquare <- bisquare$theta[, 1]

huber <- theta.est.grm(amt_converted, a, d, iter=30, cutoff=.01, init.val=0, weight.type = "Huber", tuning.par=1)
theta_huber <- huber$theta[, 1]


##################### Analysis of Aberrant Responders ##########################

# compute differences
diff_huber  <- theta_huber  - theta_mle
diff_bisq   <- theta_bisquare - theta_mle

absdif_huber <- abs(diff_huber)
absdif_bisq  <- abs(diff_bisq)

order_huber <- order(absdif_huber, decreasing = TRUE)
head(order_huber)


# connect to bogus items

# select bogus items from the original data frame
bogus <- items %>% select(Q11:Q12)
bogus_mat <- as.matrix(bogus)

# weird response = anything other than 1
bogus_aberr <- round(bogus_mat != 1)
bogus_aberr<-ifelse(rowSums(bogus_aberr)<1, 0, 1)

top_index <- order_huber[1:20]

data.frame(
  id          = top_index,
  mle         = theta_mle[top_index],
  huber       = theta_huber[top_index],
  absdif_huber = absdif_huber[top_index],
  bogus_aberr = bogus_aberr[top_index]
)

# percent overall who responded incorrectly to the bogus questions
mean(bogus_aberr) * 100

# look at percentiles
N <- length(absdif_huber)

percentile_1 <- floor(0.01 * N)
percentile_5  <- floor(0.05 * N)
percentile_10 <- floor(0.10 * N)
percentile_25 <- floor(0.25 * N)
percentile_50 <- floor(0.50 * N)
percentile_75 <- floor(0.75 * N)
percentile_90 <- floor(0.90 * N)

top1_index <- order_huber[1:percentile_1]
top5_index  <- order_huber[1:percentile_5]
top10_index <- order_huber[1:percentile_10]
top25_index <- order_huber[1:percentile_25]
top50_index <- order_huber[1:percentile_50]
top75_index <- order_huber[1:percentile_75]
top90_index <- order_huber[1:percentile_90]

pct1 <- mean(bogus_aberr[top1_index])  * 100
pct5  <- mean(bogus_aberr[top5_index])  * 100
pct10 <- mean(bogus_aberr[top10_index]) * 100
pct25 <- mean(bogus_aberr[top25_index]) * 100
pct50 <- mean(bogus_aberr[top50_index]) * 100
pct75 <- mean(bogus_aberr[top75_index]) * 100
pct90 <- mean(bogus_aberr[top90_index]) * 100
pctAll <- mean(bogus_aberr) * 100

data.frame(
  Group = c("Top 1%", "Top 5%", "Top 10%", "Top 25%", "Top 50%", "Top 75%", "Top 90%", "All respondents"),
  Pct_incorrect = c(pct1, pct5, pct10, pct25, pct50, pct75, pct90, pctAll)
)
