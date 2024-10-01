
# Load necessary library
library(MASS)

inv_logit <- function(x) exp(x)/(1 + exp(x))

# Set parameters
S <- 50
Ti <- 50
v_min <- 5
delta_v <- 1
phi <- 5
G_beta_0 <- 1
G_var <- 0.2
S_beta_0 <- 0
F_beta_0 <- 20
R_beta_0 <- -2.9

# Generate size vectors
v <- v_min + (0:S) * delta_v
v_mid <- sqrt(v[-(S + 1)] * v[-1])

# Leslie matrix
les_mat <- matrix(0, nrow = S, ncol = S)
les_mat[1, ] <- F_beta_0 * inv_logit(R_beta_0)

for (i in 2:S) {
  for (j in 1:S) {
    if (i >= j) {
      exp_growth <- (v_mid[j] / v[S + 1]) ^ G_beta_0
      les_mat[i, j] <- dnorm(v_mid[i], mean = v_mid[j] + exp_growth, sd = G_var) * 
        inv_logit(S_beta_0)
    } else {
      les_mat[i, j] <- 0
    }
  }
}

# Normalize the rows of les_mat
#les_mat <- les_mat / rowSums(les_mat)

# Simulate data
obs_count <- matrix(0, nrow = S, ncol = Ti)
obs_count[, 1] <- rpois(S, lambda = 50)

for (t in 2:Ti) {
  mu <- obs_count[, t - 1] %*% les_mat
  obs_count[, t] <- rnorm(S, mu, phi)
}

# Inspect the simulated data
print(obs_count)

# Prepare data list for Stan
stan_data <- list(S = S, Ti = Ti, obs_count = obs_count, v_min = v_min, delta_v = delta_v)

stan("Stan_Scripts/Inv_Mat.stan", data = stan_data, 
     cores = 2, chains = 2, iter = 500)

