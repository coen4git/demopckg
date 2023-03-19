#' report_p
#'
#' report_p returns p in APA style with the number of digits provides.
#'
#' @param p a number netween 0 and 1
#' @param digits the number of digits
#'
#' @return Returns the value of \code{arg1}
#' @examples
#'
#' report_p(0.346, 3) # returns p = .346
#'
#' @export

report_p <- function(p, digits = 3) {
  if (!is.numeric(p)) stop("p must be a number between 0 an 1")
  if (p<0) stop("p cannot be less than 0")
  if (p>1) stop("p cannot be greater than 1")
  if (!(digits %in% 1:5)) {
    warning("digits should probably be an integer between 1 and 5")
    digits = 3
  }
  if (p < .001) return("p < .001")

  p_round <- round(p, digits) %>%
    as.character() %>%
    # omit leading zero for APA-style
    stringr::str_replace("0.", ".") %>%
    # pad right with zeros
    stringr::str_pad(digits+1, "right", 0)

  p_string <- paste("p =", p_round)

  return(p_string)
}
