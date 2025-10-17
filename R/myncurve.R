#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value to compute P(X <= a)
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @autoglobal
#'
#' @returns A named list with components 'mu', 'sigma', 'a', and 'area' (the cumulative probability P(X <= a)).
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu = 10, sigma = 5, a = 6)}
myncurve = function(mu, sigma, a) {
  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("P(X <=", a, ")"),
        ylab = "Density",
        xlab = "x")

  # Shade area from -Inf to a
  x_vals = seq(mu - 3 * sigma, a, length = 1000)
  y_vals = dnorm(x_vals, mean = mu, sd = sigma)
  polygon(c(x_vals, a), c(y_vals, 0), col = "#7a81ff")

  # Calculate cumulative probability
  area = pnorm(a, mean = mu, sd = sigma)
  area = round(area, 4)

  # Return named list
  return(list(mu = mu, sigma = sigma, a = a, area = area))
}
