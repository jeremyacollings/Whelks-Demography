
library(fastDummies)

# Some fake parameters

# g_coefs <- fixef(M_global)[,1]
# g_coefs <- unlist(c(g_coefs, ranef(M_global)[[1]][,1,], summary(M_global)[[15]][1]))
# g_coefs[1] <- 0 # set intercept to 0 to add in site specific intercepts (ie ranef)
g_coefs <- unlist(c(fixef(M_null)[,1], 0, summary(M_null)[[15]][1]))
g_coefs2 <- c(beta = .99, size_max = 49, sd = 9.06)

s_int <- -3.36-2.36
s_size <- 0
s_temp <- 0
s_coefs <- c(s_int, s_size, s_temp)

f_int <- 10.3
f_temp <- 0
f_coefs <- c(f_int, f_temp)

r_int <- 0
r_temp <- 0
r_coefs <- c(r_int, r_temp)

g_mod(43, 42.9, .6, g_coefs)
s_mod(28, 27, .6, s_coefs)
t_mod(28, 27, .6, .6, g_coefs, s_coefs)
f_mod(.6, f_coefs)
p_mod(.6, r_coefs)

# g_covariates = c(temp = mean(cage_dat$mean_temp, na.rm = TRUE), 
#                  total = mean(cage_dat$total, na.rm = TRUE),
#                   Campo_Kennedy = 0, Cape_Mendocino = 0, 
#                   Cape_Mendocino_South = 0, Dana_Point = 0, 
#                   Punta_Morro = 1, Scripps = 0)
g_covariates = s_covariates = f_covariates = r_covariates = c(temp = mean(cage_dat$mean_temp, na.rm = TRUE))
covariates = list(growth = g_covariates, surv = s_covariates, 
                  fec = f_covariates, rec = r_covariates)
coefs = list(growth = g_coefs2, surv = s_coefs, fert = f_coefs, rec = r_coefs)

mat <-bigmatrix(covariates, coefs, min(cage_dat$prev_length, na.rm = TRUE), 
          max(cage_dat$prev_length, na.rm = TRUE), 100, 
          g_mod2, s_mod, p_mod, f_mod)

IPM <- log(mat$IPMmat)
IPM <- ifelse(IPM == -Inf, NA, IPM)
dat2 <-
  t(IPM) %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:101),
    Var2 = factor(gsub("V", "", Var2), levels = 1:101)
  )

ggplot(dat2[which(dat2$Var2 != 1),], 
       aes(as.numeric(Var1), as.numeric(Var2))) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(name = "Probability", low = "white", high = "red", 
                      na.value="white") + 
  scale_y_continuous(trans = "reverse") + 
  ylab("Size at N+1") + xlab("Size at N") + 
  theme_classic(base_size = 15)

n0 <- c(rep(100, 100))
n1 <- IPM %*% n0
n2 <- IPM %*% n1
n3 <- IPM %*% n2

lambdas <- c(lambdas, eigen(mat$IPM)$values[1])

w <- eigen(mat$IPM)$vectors
v <- Conj(solve(w))
senmat <- Re(v[1,] %*% t(w[,1]))
emat <- (1/(Re(eigen(mat$IPM)$values[1]))) * senmat * mat$IPM

emat2 <- log(emat)
emat2 <- ifelse(emat2 == -Inf, NA, emat2)
emat3 <-
  t(emat2) %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:101),
    Var2 = factor(gsub("V", "", Var2), levels = 1:101)
  )

ggplot(emat3[which(emat3$Var2 != 1),], 
       aes(as.numeric(Var1), as.numeric(Var2))) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(name = "log(Elasticity)", low = "white", high = "red", 
                      na.value="white") + 
  scale_y_continuous(trans = "reverse") + 
  ylab("Size at N+1") + xlab("Size at N") + 
  theme_classic(base_size = 15)
