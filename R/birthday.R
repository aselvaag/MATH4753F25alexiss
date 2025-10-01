#' Birthday
#'
#' @param x amount of people
#'
#' @returns The probability that at least two people share a birthday in a group of x people
#' @export
#'
#' @examples
#' birthday(20:25)
birthday = function(x){
  1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
}
