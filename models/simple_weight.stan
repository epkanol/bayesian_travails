data {
  int<lower=1> N;       // number of observations
  vector[N] log_gest;    // log gestational age
  vector[N] log_weight;     // log birth weight
}
parameters {
  vector[2] beta;           // coefs
  real<lower=0> sigma;  // error sd for Gaussian likelihood
}
model {
  // Log-likelihood
  target += normal_lpdf(log_weight | beta[1] + beta[2] * log_gest, sigma);

  // Log-priors
  target += normal_lpdf(sigma | 0, 1)
          + normal_lpdf(beta | 0, 1);
}
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] log_weight_rep; // replications from posterior predictive dist

  for (n in 1:N) {
    real log_weight_hat_n = beta[1] + beta[2] * log_gest[n];
    log_lik[n] = normal_lpdf(log_weight[n] | log_weight_hat_n, sigma);
    log_weight_rep[n] = normal_rng(log_weight_hat_n, sigma);
  }
}
