n_t = 40
n_s = 5
n_p = 2
n_c = 1000

v_state_names = c("Healthy", "Diseased1", "Diseased2", "Diseased3+", "Dead")

m_p = matrix(c(0.96, 0.03, 0.00, 0.00, 0.01,
               0.00, 0.00, 0.92, 0.00, 0.08,
               0.00, 0.00, 0.00, 0.94, 0.06,
               0.00, 0.00, 0.00, 0.96, 0.04,
               0.00, 0.00, 0.00, 0.00, 1.00),
             nrow = n_s, ncol = n_s, byrow = TRUE,
             dimnames = list(from = v_state_names,
                             to = v_state_names))
state_membership = array(NA_real_,
                         dim = c(n_t, n_s),
                         dimnames = list(cycle = 1:n_t,
                                         state = v_state_names))

state_membership[1, ] = c(n_c, rep(0, n_s - 1))

for (i in 2:n_t) {
  state_membership[i, ] = state_membership[i - 1, ] %*% m_p
}
v_payoff_names = c("Cost", "QALY")

m_payoffs = matrix(c(50, rep(1000, 3), 0,
                     0.9, rep(0.6, 3), 0),
                   nrow = n_s, ncol = n_p, byrow = FALSE,
                   dimnames = list(state = v_state_names,
                                   payoff = v_payoff_names)
                   )

payoff_trace = state_membership %*% m_payoffs

colSums(payoff_trace) / n_c
