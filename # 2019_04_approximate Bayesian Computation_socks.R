# 2019_04_12_approximate_bayesian_computation_practice_with_small_example
# * reference: http://www.di.fc.ul.pt/~jpn/r/abc/index.html

# 1. Construct a generative model that produces the same type of data as you are trying to model. 
#    Assume prior probability distributions over all the parameters that you want to estimate.
#    Just as when doing standard Bayesian modelling these distributions represents the model¡¯s information
#    regarding the parameters before seeing any data.

# 2. Sample tentative parameters values from the prior distributions, plug these into the generative model
#    and simulate a dataset.

# 3. Check if the simulated dataset matches the actual data you are trying to model. 
#    If yes, add the tentative parameter values to a list of retained probable parameter values, if no, throw them away.

# 4. Repeat step 2 and 3 a large number of times building up the list of probable parameter values.

# 5. Finally, the distribution of the probable parameter values represents the posterior information
#    regarding the parameters. Intuitively, the more likely it is that a set of parameters generated data
#    identical to the observed, the more likely it is that that combination of parameters ended up being 
#    retained.



# The Parameters (which we do not know, but assume the following values as an eg)
n_socks  <- 18  
n_pairs  <- 7
n_odd    <- 4   # total socks = 2*n_pairs + n_odd


# The Data
n_picked <- 11

? rep

# The Generative Model
socks <- rep(seq_len(n_pairs + n_odd), rep(c(2,1), c(n_pairs,n_odd))) # label socks (same pair, same label)

socks
# [1]  1  1  2  2  3  3  4  4  5  5  6  6  7  7  8  9 10 11

min(n_picked, n_socks) # 11
pick_socks <- sample(socks, min(n_picked, n_socks))
pick_socks
class(pick_socks)
length(pick_socks) #11

sock_counts <- table(pick_socks)
sock_counts

c(unique=sum(sock_counts==1), pairs=sum(sock_counts=2))

# The generative model above needs adequate prior distributions for its parameters, 
# ** parameters ~~~ (1) n_socks, (2) n_pairs, (3) n_odd 
# (n_picked is the data and will be used to estimate the parameter¡¯s values).


# # (1) n_sock prior is negative binomial -> a negative binomial random generation, rnbinom(mu, size)

prior_mu <- 30
prior_sd <- 15
prior_size <- -prior_mu^2/(prior_mu-prior_sd^2)
n_socks <-rnbinom(10, mu=prior_mu, size = prior_size)

n_socks # parameter valuues

xs<-seq(0,75)
barplot(dnbinom(xs, mu= prior_mu, size =prior_size), names.arg=xs, lwd=2, col="lightgreen",
        main="prior ratio for total number of socks", xlab ="total socks", ylab ="probability")


# prior for the ratio of n_pairs(parameter) on total (n_pairs*2+n_odd = total)

estBetaParms <- function(mu, var) { # estimate beta parameter from input mu and var
  alpha <- ((1-mu)/var-1/mu)*mu^2
  beta <- alpha*(1/mu -1)
  c(alpha, beta)
}

beta_params <- estBetaParms(mu=0.875, var=0.08^2)
beta_params # [1] alpha = 14.07861  beta = 2.01123


prior_alpha <- round(beta_params[1]) 
prior_alpha # 14

prior_beta <- round(beta_params[2])
prior_beta # 2

xs <- seq(0,1,len=101)

plot(xs, dbeta(xs, prior_alpha, prior_beta), lwd=2, col="red", type ="l",
     main ='prior ratio for paired socks', xlab ='ratio', ylab ='density')


## with the selection of model and priors, we can make a sample function

sock_sample <- function(n_picked) {
  # generate a sample of prior parameter values (a)
  prior_mu <- 30
  prior_sd <- 15
  prior_size <- -prior_mu^2 / (prior_mu - prior_sd^2)
  n_socks <- rnbinom(1, mu=prior_mu, size = prior_size) # n=1 number of observation
  
  prior_alpha <- 14
  prior_beta <- 2
  prop_pairs <- rbeta(1, prior_alpha, prior_beta)
  n_pairs    <- round(prop_pairs * floor(n_socks/2), 0)
  n_odd <- n_socks - 2*n_pairs
  
  # simulate picking socks (b) <- (a) 3 papameters 1. n_socks, 2. n_pairs, 3. n_odd
  
  socks <- rep(seq_len(n_pairs + n_odd), rep(c(2,1), c(n_pairs, n_odd)))
  pick_socks <- sample(socks, min(n_picked, n_socks))
  sock_count <- table(pick_socks)
  
  # returning parameters values and the counting of odd and paired socks
  
  c(unique = sum(sock_counts ==1),
    pairs = sum(sock_counts ==2),
    n_socks = n_socks,
    n_pairs = n_pairs,
    n_odd = n_odd,
    prop_pairs = prop_pairs
    )
}

sock_sample(11)  


# plug the data and iterate and get buch of smaples

n_picked <- 11 # observed data size
sock_sim <- t(replicate(5e4, sock_sample(n_picked))) # 5e4 =50,000
    
#(replicate) is a wrapper for the common use of sapply 
#for repeated evaluation of an expression (which will usually involve random number generation).

length(sock_sim)
head(sock_sim,10)

post_samples <- sock_sim[sock_sim[,"unique"]==11 & sock_sim[,"pairs"]==0, ] # select samples where x*==x

median_hist <- function(x, ...) {
  hist(x, ...)
  abline(v = median(x), col = "darkred", lty = 2, lwd = 2)
}

median_hist(post_samples[,"n_socks"], 40, xlim = c(0, 90), yaxt = "n", ylab = "", breaks=50,
            xlab = "Number of socks", main = "Posterior on n_socks", col = "lightblue")

median(post_samples[,"n_socks"]) 

occ <- table(post_samples[,"n_socks"])
as.numeric(row.names(occ)[which.max(occ)]) # mode guess
