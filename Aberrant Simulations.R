 ################ Simulating Datasets For Aberrant Responding ###################

set.seed(123) # set the seed to ensure reproducibility

N <- 1000 # number of individuals
J <- 30 # number of items
theta <- rnorm(N, 0, 1) # generate theta values from normal distribution

a <- runif(J, 1.0, 2.0) # generate discrimination values between 1.0 and 2.0
b <- rnorm(J) # generate difficulty values


#' Suboptimal Responding with the 2PL Model
#' 
#' @description 
#' Simulates dichotomous item responses under a 2PL model in which a subset of
#' individuals respond suboptimally by having their latent trait values reduced.
#' @details
#' Extracts item parameters from uniform (discrimination) and normal
#' (difficulty) distributions. Forty percent of individuals are randomly selected
#' to respond suboptimally. Their latent ability values are shifted downward by
#' one standard unit before computing 2PL response probabilities. Responses are
#' generated using data from the GDINA package.
#' @param N Number of individuals.
#' @param J Number of items.
#' @param theta Vector of latent ability traits.
#' @param a Item discrimination parameters.
#' @param b Item difficulty parameters.
#' @param subopt_ids Individuals responding suboptimally.
#' @param prob_2pl Function computing 2PL probabilities.
#' @param dat Simulated response matrix.
#' @references C. Schuster and K.-H. Yuan. Robust Estimation of Latent Ability in Item Response Models. Journal of Educational and Behavioral Statistics, 36(6):720–735, Dec. 2011. ISSN 1076-9986, 1935-1054. doi: 10.3102/1076998610396890. URL http://journals.sagepub.com/doi/10.3102/1076998610396890.

# 1. Identify 40% of examinees to respond suboptimally
n_subopt <- floor(0.40 * N)
subopt_ids <- sample(1:N, n_subopt)

# 2. Create updated theta vector
theta_new <- theta
theta_new[subopt_ids] <- theta_new[subopt_ids] - 1

# 2PL probabilities function
prob_2pl <- function(theta, a, b) {
  exp(1.7 * a * (theta - b)) / (1 + exp(1.7 * a * (theta - b)))
}

# Create a matrix with 2PL generated probabilities
prob_matrix <- sapply(1:J, function(j) {
  prob_2pl(theta_new, a[j], b[j])
})

# Generate the data using the dat.gen function and the probabilities above
dat <- dat.gen(prob_matrix) 

save(simdat=dat, aberrant_ids=subopt_ids, ipars=cbind(a,b), file = "simSuboptimal2PL.RData")


#' Back Random Responding (BRR) with the 2PL Model
#' 
#' @description
#' Simulates dichotomous responses where a subset of items at the end of the test
#' are replaced with random responses for a subset of individuals.
#' @details
#' Responses are first generated under a standard 2PL model. The last 40% of
#' items are designated as BRR items. A prevalence rate determines which
#' individuals exhibit BRR. For these individuals, responses to BRR items are
#' overwritten with probability 0.20.
#' @param prev_rate Proportion of individuals showing BRR.
#' @param last_items Items overwritten with random responses.
#' @param brr_ids Individuals exhibiting BRR.
#' @param brr_prob The response probability applied to BRR items for affected individuals.
#' @references Clark, M. E., Gironda, R. J., & Young, R. W. (2003). Detection of back random responding: Effectiveness of MMPI-2 and Personality Assessment Inventory validity indices. Psychological Assessment, 15(2), 223–234. https://doi.org/10.1037/1040-3590.15.2.223
#' @references Yu, X., & Cheng, Y. (2019). A change-point analysis procedure based on weighted residuals to detect back random responding. Psychological Methods, 24(5), 658–674. https://doi.org/10.1037/met0000212
#' 
theta <- rnorm(N, 0, 1)
a <- runif(J, 1.0, 2.0) # generate discrimination values between 1.0 and 2.0
b <- rnorm(J) # generate difficulty values

# Create a matrix with 2PL generated probabilities
prob_matrix <- sapply(1:J, function(j) {
  prob_2pl(theta, a[j], b[j])
})

# Generate the data using the dat.gen function and the probabilities above
dat <- dat.gen(prob_matrix) 

# Identify the last 40% of items
last_items <- tail(1:J, floor(0.40 * J))

# Probability of BRR
brr_prob <- 0.20

# Overwrite those items with BRR responses
for (j in last_items) {
  dat[, j] <- rbinom(N, 1, brr_prob)
}

# subset of people, prevalence rate
prev_rate <- 0.20
brr_ids <- sample(1:N, size = floor(prev_rate * N))

simdat = dat
aberrant_ids = brr_ids
ipars = cbind(a, b)

save(simdat=dat, aberrant_ids=brr_ids, ipars=cbind(a, b), file = "simBRR2PL.RData")

#' Cheating with the 2PL Model
#' 
#' @description
#' Simulates dichotomous responses where low‑ability individuals are given perfect
#' scores on the most difficult items.
#' @details
#' After generating 2PL responses, the lowest 20% of individuals are identified. 
#' The most difficult 20% of items are also identified. For these individual-item combinations, 
#' responses are overwritten with correct answers to reflect cheating behavior.
#' @param low_theta_ids Individuals with lowest ability.
#' @param dif_items Most difficult items.
#' @param dat Response matrix with cheating applied.
#' @references Wang, C., Xu, G., Shang, Z., & Kuncel, N. (2018). Detecting Aberrant Behavior and Item Preknowledge: A Comparison of Mixture Modeling Method and Residual Method. Journal of Educational and Behavioral Statistics, 43(4), 469–501. https://doi.org/10.3102/1076998618767123
#' 
theta <- rnorm(N, 0, 1)
a <- runif(J, 1.0, 2.0) # generate discrimination values between 1.0 and 2.0
b <- rnorm(J) # generate difficulty values

# Create a matrix with 2PL generated probabilities
prob_matrix <- sapply(1:J, function(j) {
  prob_2pl(theta, a[j], b[j])
})

# Generate the data using the dat.gen function and the probabilities above
dat <- dat.gen(prob_matrix) 

# Identify the 20% lowest thetas (lowest ability)
n_low <- floor(0.20 * N)
low_theta_ids <- order(theta)[1:n_low]

# Identify the 20% most difficult items
n_dif <- floor(0.20 * J)
dif_items <- order(b, decreasing = TRUE)[1:n_dif]

# Overwrite those items with 
for (j in dif_items) {
  dat[low_theta_ids, j] <- rbinom(n_low, 1, 1)
}

save(simdat=dat, aberrant_ids=low_theta_ids, ipars=cbind(a,b), file = "simCheating2PL.RData")

#' Warm-up Responding with the 2PL Model
#' @description
#' Simulates individuals who perform poorly on early items due to warm-up effects.
#' @details
#' Responses are generated under a 2PL model. A random 20% of individuals are
#' selected to exhibit warm‑up behavior. For these individuals, responses to the
#' first 30% of items are overwritten with incorrect responses.
#' @param warmup_ids Individuals showing warm‑up behavior.
#' @param warmup_items Early items overwritten with zeros.
#' @references Meijer, R. R. (2002). Outlier Detection in High-Stakes Certification Testing. Journal of Educational Measurement, 39(3), 219–233. https://doi.org/10.1111/j.1745-3984.2002.tb01175.x
#' 
theta <- rnorm(N, 0, 1)
a <- runif(J, 1.0, 2.0) # generate discrimination values between 1.0 and 2.0
b <- rnorm(J) # generate difficulty values

# Create a matrix with 2PL generated probabilities
prob_matrix <- sapply(1:J, function(j) {
  prob_2pl(theta, a[j], b[j])
})

# Generate the data using the dat.gen function and the probabilities above
dat <- dat.gen(prob_matrix) 

# Identify 30% of participants to "warm-up"
prev_rate <- 0.20
warmup_ids <- sample(1:N, size = floor(prev_rate * N)) #MAKE RANDOM

# Identify the first 30% of items
n_items_warmup <- floor(0.30 * J)
warmup_items <- 1:n_items_warmup

# Overwrite those responses as incorrect (0)
for (j in warmup_items) {
  dat[warmup_ids, j] <- 0
}

save(simdat=dat, aberrant_ids=warmup_ids, ipars=cbind(a, b), file = "simWarmUp2PL.RData")

#' Improper Reverse Coding with the Graded Response Model (GRM)
#' 
#' @description
#' Generates polytomous responses under a graded response model (GRM) and applies
#' improper reverse coding to a subset of individuals and items.
#' @details
#' Item thresholds are generated and then sorted within each item to maintain the
#' required ordering. GRM category probabilities are obtained. A random 20% of individuals 
#' and 30% of items are selected, and responses are recoded using \eqn{X' = K + 1 - X}, 
#' yielding improperly reversed categories.
#' @param K Number of response categories.
#' @param irc_ids Individuals with IRC behavior.
#' @param irc_items Items subjected to reverse coding.
#' @references Hughes, G. D. (2009). The Impact of Incorrect Responses to Reverse-Coded Survey Items. Research in the Schools, 14
#' 
K <- 5  # maximum Likert category

theta <- rnorm(N, 0, 1)
a <- runif(J, 1.0, 2.0) # generate discrimination values between 1.0 and 2.0
b_raw <- matrix(rnorm(J + (K-1)), nrow = J)
b <- t(apply(b_raw, 1, sort)) # Sort thresholds within each item (row-wise)

# Create a matrix with GRM generated probabilities
prob_matrix <- item.prob(theta, model = "GRM", ipars = cbind(a, b), D=1.7)

# Generate the data using the dat.gen function and the probabilities above
dat <- dat.gen(prob_matrix$P, anchor=1, polytomous = TRUE) 

# Identify 30% of items
n_irc <- floor(0.30 * J)
irc_items <- sample(1:J, n_irc)

# Do this for percentage of people
prev_rate <- 0.20
irc_ids <- sample(1:N, size = floor(prev_rate * N))

# Apply improperly reverse coded responding: observed = K + X - 1
for (j in irc_items) {
  dat[irc_ids, j] <- K + 1 - dat[irc_ids, j]
}

save(simdat=dat, aberrant_ids=irc_ids, ipars=cbind(a, b), file = "simIRCGRM.RData")

#' Back Random Responding with the Multidimensional Graded Response Model (MGRM)
#' 
#' @description
#' Generates multidimensional polytomous responses under an MGRM and applies
#' random responding to a subset of individuals and items.
#' @details
#' A simple‑structure loading matrix assigns each item to one dimension. After
#' generating MGRM responses, 40% of items and 20% of individuals are selected.
#' For these individual–item combinations, responses are replaced with uniformly random
#' category selections.
#' @param L Number of latent dimensions.
#' @param brrmgrm_items Items with BRR.
#' @param brrmgrm_ids Individuals exhibiting BRR.
#' @param brrmgrm_prob The probability assigned to each response category under BRR.
#' @references Clark, M. E., Gironda, R. J., & Young, R. W. (2003). Detection of back random responding: Effectiveness of MMPI-2 and Personality Assessment Inventory validity indices. Psychological Assessment, 15(2), 223–234. https://doi.org/10.1037/1040-3590.15.2.223
#' @references Yu, X., & Cheng, Y. (2019). A change-point analysis procedure based on weighted residuals to detect back random responding. Psychological Methods, 24(5), 658–674. https://doi.org/10.1037/met0000212
#' 
N <- 2000 # number of individuals
J <- 50 # number of items
K <- 5  # maximum Likert category
L <- 3 # number of dimensions
theta <- matrix(rnorm(N * L), ncol = L)

# Each item loads on exactly one dimension (simple structure)
a <- matrix(0, nrow = J, ncol = L)

# Assign each dimension an equal number of items
items_per_dim <- ceiling(J / L)
dim_assign <- rep(1:L, each = items_per_dim)[1:J]

for (j in 1:J) {a[j, dim_assign[j]] <- runif(1, 1.0, 2.0)}
b_raw <- matrix(rnorm(J * (K - 1)), nrow = J)
b <- t(apply(b_raw, 1, sort))

ipars <- cbind(a, b)
prob_matrix <- item.prob(theta, model = "MGRM", ipars = ipars, D = 1.7)
dat <- dat.gen(prob_matrix$P, anchor = 1, polytomous = TRUE)

#Apply back random responding
n_brrmgrm_items <- floor(0.40 * J)
brrmgrm_items <- sample(1:J, n_brrmgrm_items)

# Choose 20% of people to respond randomly
prev_rate <- 0.20
brrmgrm_ids <- sample(1:N, size = floor(prev_rate * N))

# Random responding probability
brrmgrm_prob <- 1 / K   # uniform over categories

for (j in brrmgrm_items) {
  dat[brrmgrm_ids, j] <- sample(1:K, size = length(brrmgrm_ids), replace = TRUE)
}

save(simdat=dat, aberrant_ids=brrmgrm_ids, ipars, file = "simBRRMGRM.RData")

#' Cheating with the Multidimensional Item Response Theory Model (MIRT)
#' 
#' @description
#' Generates polytomous responses under a multidimensional graded response model
#' and applies cheating behavior to low‑ability individuals on the hardest items.
#' @details
#' Individuals falling in the lowest 20% on the first dimension are identified.
#' Item difficulty is computed as the mean of the category thresholds, and the
#' hardest 20% of items are selected. For these examinees and items, responses
#' are replaced with the highest category.
#' @param low_theta_mirt_ids Individuals who are cheating.
#' @param dif_items Hardest items.
#' @references Cui, Y., & Li, J. (2015). Evaluating Person Fit for Cognitive Diagnostic Assessment. Applied Psychological Measurement, 39(3), 223–238. https://doi.org/10.1177/0146621614557272
#' @references Meijer, R. R. (1996). Person-Fit Research: An Introduction. Applied Measurement in Education, 9(1), 3–8. https://doi.org/10.1207/s15324818ame0901

# J = 50 items, N = 2000, L = 2, not simple structure

N <- 2000 # number of individuals
J <- 50 # number of items
K <- 5  # maximum Likert category
L <- 2 # number of dimensions
theta <- matrix(rnorm(N * L), ncol = L)

a <- matrix(runif(J * L, 1.0, 2.0), nrow = J, ncol = L)
b_raw <- matrix(rnorm(J * (K - 1)), nrow = J)
b <- t(apply(b_raw, 1, sort))
ipars <- cbind(a, b)

prob_matrix <- item.prob(theta, model = "MGRM", ipars = ipars, D = 1.7)
dat <- dat.gen(prob_matrix$P, anchor = 1, polytomous = TRUE)

# Apply cheating

# Identify lowest 20% of examinees by ability on dimension 1
n_low <- floor(0.20 * N)
low_theta_mirt_ids <- order(theta[, 1])[1:n_low]

# Identify hardest 20% of items (highest average threshold)
item_difficulty <- rowMeans(b)
n_dif <- floor(0.20 * J)
dif_items <- order(item_difficulty, decreasing = TRUE)[1:n_dif]

# Overwrite those responses with perfect scores (cheating)
for (j in dif_items) {
  dat[low_theta_mirt_ids, j] <- K   # highest category
}

save(simdat=dat, aberrant_ids=low_theta_mirt_ids, ipars=cbind(a, b), file = "simCheatingMirt.RData")