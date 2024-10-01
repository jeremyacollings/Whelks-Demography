data {
  int N;
  int S;
  int In;
  int site[In];
  int ind[N];
  real sizet[N];
  real size_t0[N];
  real days[N];
  real size_max;
}

parameters {
  real ind_raw;
  real site_raw;
  real site_mean;
  real ind_sd;
  real site_sd;
  real growth_sd;
}

model {
}