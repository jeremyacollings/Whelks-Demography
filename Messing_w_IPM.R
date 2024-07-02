
# Some fake parameters

g_int <- 28.22
g_size <- -.4164
g_temp <- -0.9485
g_sd <- 0.05*365
g_coefs <- c(g_int, g_size, g_temp, g_sd)

s_int <- 0
s_size <- .1
s_temp <- -.01
s_coefs <- c(s_int, s_size, s_temp)

f_int <- 20
f_temp <- -.01
f_coefs <- c(f_int, f_temp)

r_int <- 0
r_temp <- -.01
r_coefs <- c(r_int, r_temp)

g_mod(28, 27, .6, g_coefs)
s_mod(28, 27, .6, s_coefs)
t_mod(28, 27, .6, .6, g_coefs, s_coefs)
f_mod(.6, f_coefs)
p_mod(.6, r_coefs)

covariates = list(growth = .6, surv = .6, fert = .6, rec = .6)
coefs = list(growth = g_coefs, surv = s_coefs, fert = f_coefs, rec = r_coefs)

mat <-bigmatrix(covariates, coefs, min(cage_dat$prev_length, na.rm = TRUE), 
          max(cage_dat$prev_length, na.rm = TRUE), 10)

IPM <- mat$IPMmat
dat2 <-
  t(IPM) %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var1 = factor(Var1, levels = 1:11),
    Var2 = factor(gsub("V", "", Var2), levels = 1:11)
  )

ggplot(dat2, aes(as.numeric(Var1), as.numeric(Var2))) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "white", high = "red") + 
  scale_y_continuous(trans = "reverse")

n0 <- c(1000, rep(100, 10))
n1 <- IPM %*% n0
n2 <- IPM %*% n1
n3 <- IPM %*% n2
