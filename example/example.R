library(sir)

# Set time step and initial numbers of susceptible and infected individuals
dt <- 0.25
S_ini <- 1000
I_ini <- 10

# Create dust object for model
sir_model <- sir$new(
    list(dt = dt,S_ini = S_ini,I_ini = I_ini,
         beta = 0.4,gamma = 0.1),
    step = 0,n_particles = 1,n_threads = 1,seed = 1)

# Run epidemic forward
n_particles <- 10
n_steps <- 200
x <- array(NA, dim = c(sir_model$info()$len, n_particles, n_steps))

for (t in seq_len(n_steps)) {
    x[ , , t] <- sir_model$run(t)
}
time <- x[1, 1, ]
x <- x[-1, , ]

# Plot trajectories
par(mar = c(4.1, 5.1, 0.5, 0.5), las = 1)
cols <- c(S = "#8c8cd9", I = "#cc0044", R = "#999966")
matplot(time, t(x[1, , ]), type = "l",
        xlab = "Time", ylab = "Number of individuals",
        col = cols[["S"]], lty = 1, ylim = range(x))
matlines(time, t(x[2, , ]), col = cols[["I"]], lty = 1)
matlines(time, t(x[3, , ]), col = cols[["R"]], lty = 1)
legend("left", lwd = 1, col = cols, legend = names(cols), bty = "n")