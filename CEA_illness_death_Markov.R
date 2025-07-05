n_t = 40
n_s = 3
n_c = 1000
v_state_names = c("Healthy", "Diseased", "Dead")
m_p = matrix(c(0.96, 0.03, 0.01,
               0.00, 0.95, 0.05,
               0.00, 0.00, 1.00),
             nrow = 3,
             ncol = 3,
             byrow = TRUE,
             dimnames = list(from = v_state_names, to = v_state_names))
state_membership = array(NA_real_,
                          dim = c(n_t, n_s), 
                          dimnames = list(cycle = 1:n_t, state = v_state_names)) 
state_membership[1, ] = c(n_c, 0, 0)
for (i in 2:n_t) {
  state_membership[i, ] = state_membership[i-1, ] %*% m_p
}
state_membership

m_paoffs = matrix(c(50, 1000, 0, 0.9, 0.6, 0),
                  nrow = 3,
                  ncol = 2,
                  byrow = FALSE,
                  dimnames = list(state = v_state_names, payoff = c("Cost", "QALY")))
payoff_trace = state_membership %*% m_paoffs
colSums(payoff_trace)/n_c
