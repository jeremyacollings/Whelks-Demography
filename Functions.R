
########## FUNCTIONS ##########

make_IPM <- function(n_bin = 10, min_size, max_size, 
                           eggs_per_whelk, larvae_per_egg, recruitment, 
                           growth0, growth1, growth_shape, survival){
  
  # create mesh points
  n <- n_bin # number of bins
  L <- min_size # lowest size
  U <- max_size # highest size
  h <- (U-L)/n # bin size
  b <- L + (0:n)*h # meshpoints
  y <- 0.5*(b[1:n] + b[2:(n+1)]) # midpoints
  
  # calculate transition rates
  
  # fecundity
  f = eggs_per_whelk * larvae_per_egg * recruitment
  
  # growth & survival
  grid <- expand.grid(size_y = y, size_x = y)
  
  grid$growth_prob <- dgamma(grid$size_y, 
                             shape = growth_shape,
                             rate = growth_shape / exp(growth0 + growth1 * grid$size_x))
  grid$survival <- survival
  grid$transition <- exp(log(grid$growth_prob) + log(grid$survival))
  grid$transition[which(grid$size_y <= grid$size_x)] <- 0
  
  # compile into IPM
  
  IPM_matrix <- matrix(data = grid$transition, nrow = n, ncol = n)
  IPM_matrix[1,] <- rep(f, each = n)
  
  IPM_matrix
}

make_IPM_array <- function(n_bin = 10, min_size, max_size, 
                     eggs_per_whelk, larvae_per_egg, recruitment, 
                     growth0, growth1, growth_shape, survival){
    
  # create mesh points
    n <- n_bin # number of bins
    L <- min_size # lowest size
    U <- max_size # highest size
    h <- (U-L)/n # bin size
    b <- L + (0:n)*h # meshpoints
    y <- 0.5*(b[1:n] + b[2:(n+1)]) # midpoints
    
  # calculate transition rates
    
    # fecundity
    f = eggs_per_whelk * larvae_per_egg * recruitment
    
    # growth & survival
    grid <- expand.grid(size_y = y, size_x = y, draw = 1:length(growth0))
    
    grid$growth_prob <- dgamma(grid$size_y, 
                               shape = growth_shape,
                               rate = growth_shape / exp(growth0 + growth1 * grid$size_x))
    grid$survival <- survival
    grid$transition <- exp(log(grid$growth_prob) + log(grid$survival))
    grid$transition[which(grid$size_y <= grid$size_x)] <- 0
    
  # compile into IPM
    
    IPM_array <- array(data = grid$transition, dim = c(n, n, length(growth0)))
    IPM_array[1,,] <- rep(f, each = n)
    
    IPM_array
}

get_lambda <- function(x){
  eigen(x)$values[1]
}

pseudo_log <- function(x) { asinh(x/2)/log(10) }
