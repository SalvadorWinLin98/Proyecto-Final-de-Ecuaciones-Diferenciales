

# Load necessary library
library(deSolve)

# Define the ODE model function
EcModeloq <- function(tiempoq, yq, parametrosq) {
  with(as.list(c(yq, parametrosq)), {
    dq <- k * q - 20000
    list(dq)
  })
}

# Set initial values and parameters
yq <- c(q = 200000)  # Initial value of q
parametrosq <- c(k = 0.05)
taños <- seq(0, 15, 1)
tiempoq <- taños

# Calculate the model solution using ode function
Modelotq <- ode(yq, tiempoq, EcModeloq, parametrosq)

# Set the position of the Y-axis title
par(mgp = c(3, 1, 0))  # Set the distance between axis title and axis line

# Plot the results with modified Y-axis title position
plot(Modelotq,
     xlab = "Tiempo (años)", ylab = "Monto (MXN)",
     xlim = c(0, 14), ylim = c(0, 210000), pch = 2, col = "blue",
     tcl = 0.1,
     main = "Gráfica del monto en pesos", las = 1, cex = 1.1, axes = TRUE)

# Add lines to the points in the plot
lines(Modelotq[,1], Modelotq[,2], col = "blue", lwd = 0.5)

# Euler method for solving the differential equation
euler1 <- function(f, t0, y0, h, n) {
  t <- seq(t0, t0 + (n - 1) * h, by = h)
  y <- rep(NA, times = n)
  y[1] <- y0
  for (i in 2:n) y[i] <- y[i - 1] + h * f(t[i - 1], y[i - 1])
  data.frame(t, y)  # Use data.frame to create a dataframe with t and y
}

# Test the Euler method
f <- function(t, y) 0.05 * y - 20000
euler_result <- euler1(f, 0, 200000, 1, 16)

# Plot the Euler method results on the same graph
points(euler_result$t, euler_result$y, pch = 19, col = "red")

# Add a vertical and horizontal line at x = 13.8629 and y = 0
abline(v = 13.8629, col = "purple", lwd = 2, lty = 2)
abline(h = 0, col = "purple", lwd = 2, lty = 2)

# Add a box with the label "t = 13.86 años" at a different position
text(x = 10, y = 180000, labels = "t = 13.86 años", pos = 1, col = "purple")
text(x = 10, y = 160000, labels = "M = 0 pesos", pos = 1, col = "purple")

