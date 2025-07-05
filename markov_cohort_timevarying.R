n_t = 40
n_s =3
n_c = 1000

v_state_names = c("Healthy", "Diseased", "Dead")

trans_mat = array(NA_real_,
                  dim = c(n_s, n_s, n_t),
                  dimnames = list(from = v_state_names,
                                  to = v_state_names,
                                  cycle = 1:n_t))

trans_mat[2, 1, ] = 0 # cannot back from diseased to healthy
trans_mat[3, 1, ] = 0 # not possible to come back frmo dead to healthy
trans_mat[3, 2, ] = 0 # not possible to come back frmo dead to diseased

trans_mat[1, 2, ] = 0.03 # healthy to diseased
trans_mat[3, 3, ] = 1 # to remains in diseased

trans_mat[1, 3, 1:10] = 0.01 # prob of healthy to diseased depends on cycle
trans_mat[1, 3, 11:20] = 0.02
trans_mat[1, 3, 21:30] = 0.04
trans_mat[1, 3, 31:40] = 0.08

trans_mat[2, 3, ] = trans_mat[1, 3, ] + 0.04

trans_mat[2, 2, ] = 1 - trans_mat[2, 3, ]
apply(trans_mat[1, , ], 2, sum, na.rm = TRUE)
1-apply(trans_mat[1, , ], 2, sum, na.rm = TRUE)

trans_mat[1, 1, ] = 1 - apply(trans_mat[1, , ], 2, sum, na.rm = TRUE)

trans_mat

state_membership = array(NA_real_,
                         dim = c(n_t, n_s),
                         dimnames = list(cycle = 1:n_t,
                                         state = v_state_names))

state_membership[1, ] = c(n_c, 0, 0)

for (i in 2:n_t) {
  state_membership[i, ] = state_membership[i - 1, ] %*% trans_mat[ , , i-1]
}

matplot(1:n_t, state_membership, type = 'l')

payoffs = array(NA_real_,
                dim = c(n_s, 2, n_t),
                dimnames = list(state = v_state_names,
                                payoff = c("Cost", "QALY"),
                                cycle = 1:n_t))

#payoffs[1, 1, 1:10] = 10 # cost of being in first state(healthy)
#payoffs[1, 2, 1:10] = 0.95 # QALYs of healthy state per cycle
# above line will create too many lines so instead of above two lines 

payoffs[, , 1:10] = c(10, 800, 0, 0.95, 0.65, 0) # this consist both cost and QALYs for 3 state
payoffs[, , 11:20] = c(25, 1000, 0, 0.92, 0.60, 0)
payoffs[, , 21:30] = c(40, 1200, 0, 0.88, 0.55, 0)
payoffs[, , 31:40] = c(80, 1000, 0, 0.85, 0.50, 0)

payoff_trace = array(NA_real_,
                     dim = c(n_t, 2),
                     dimnames = list(cycle = 1:n_t, payoff = c("Cost", "QALY")))

for (i in 1:n_t) {
  payoff_trace[i, ] = state_membership[i, ] %*% payoffs[, , i]
}

colSums(payoff_trace) / n_c

matplot(1:n_t, payoff_trace, type = 'l' )

