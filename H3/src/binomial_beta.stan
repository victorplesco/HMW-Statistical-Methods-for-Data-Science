data {
  int <lower=0> N;
  int <lower=0> n;
  real <lower=0> a;
  real <lower=0> b;
}

parameters {
  real <lower=0, upper=1> p;
}

model {
  target+=binomial_lpmf(n | N, p);
  target+=beta_lpdf(p|a,b);
}
