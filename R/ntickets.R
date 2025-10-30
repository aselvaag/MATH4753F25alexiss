#' ntickets
#'
#' @param N The amount of seats available on the flight
#' @param gamma The probability of overbooking
#' @param p The probability that a ticketed passenger shows up for the flight
#'
#' @importFrom stats qbinom pbinom qnorm
#' @importFrom graphics abline barplot points
#'
#' @returns A list of values for nd, nc, N, p, and gamma, and two plots: discrete and continuous objectives vs n
#' @export
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p){

  # Set up a ticket range (n) to search for the optimal number of tickets to be sold for the discrete and continuous methods
  ticket_range = seq(N, N + 50, by = 1)

## Discrete Objective
  # Formula for the Discrete Objective: 1 - gamma - P(X <= N), where X~Binomial(n,p)
  discrete_objective = 1 - gamma - pbinom(N, size = ticket_range, prob = p)

  # Find the minimum discrete objective index (closest to zero)
  discrete_index = which.min(abs(discrete_objective))

  # Find the minimum n-value for the discrete method using the range of N's
  nd = ticket_range[discrete_index]

  # Plot the line of the discrete objective
  plot(ticket_range, discrete_objective, type = "l", lty = "dotdash", col = "black", main = paste("Objective Vs n to find optimal tickets sold\n(", ticket_range[discrete_index],") gamma=", gamma, ", N=", N, " discrete", sep = ""), xlab = "n", ylab = "Objective")

  # Plot points along the line of the discrete objective
  points(ticket_range, discrete_objective, pch = 21, col = "black", bg = "blue", cex = 0.75)

  # Plot the point to show what is the optimal number of n tickets to be sold
  points(ticket_range[discrete_index], discrete_objective[discrete_index], pch = 21, bg = "yellow", cex = 1)

  # Plot the vertical and horizontal lines that intersect with the optimal point
  abline(h = 0, col = "red", lty = 1, lwd = 2.25)
  abline(v = ticket_range[discrete_index], col = "red", lty = 1, lwd = 2.25)


## Continuous Approximation Objective
  # Continuous objective approximation through P(X <= N) by taking the z-score
  z_score = (N + 0.5 - ticket_range * p) / sqrt(ticket_range * p * (1 - p))
  approximate_p = pnorm(z_score)
  continuous_objective = 1 - gamma - approximate_p

  # Find the minimum continuous objective index  (that is closest to zero)
  continuous_index = which.min(abs(continuous_objective))

  # Find the minimum n-value for the continuous method using the range of tickets (n)
  nc = ticket_range[continuous_index]

  # Plot the line of the continuous objective
  plot(ticket_range, continuous_objective, type = "l", col = "black", lwd = 1, main = paste("Objective Vs n to find optimal tickets sold\n(", ticket_range[continuous_index],") gamma=", gamma, ", N=", N, " continuous", sep = ""), xlab = "n", ylab = "Objective")

  # Plot the point to show what is the optimal number of n tickets to be sold
  points(ticket_range[continuous_index], continuous_objective[continuous_index], pch = 21, bg = "yellow", cex = 1)

  # Plot the vertical and horizontal lines that intersect with the optimal point
  abline(h = 0, col = "blue", lty = 1)
  abline(v = ticket_range[continuous_index], col = "blue", lty = 1)

  # Return a list that prints the values of nd, nc, N, p, and gamma
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
