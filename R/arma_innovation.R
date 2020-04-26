arma.innovation <- function(x, arma.model, ar.truncation=10) {
  p <- arma.model$arma[1]
  q <- arma.model$arma[2]
  ar.coef <- arma.model$coef[seq_len(p)]
  ma.coef <- arma.model$coef[p + seq_len(q)]
  if (q == 0) {
    infinite.ar.coef <- ar.coef
  } else {
    infinite.ar.coef <- -ARMAtoMA(-ma.coef, -ar.coef, ar.truncation)
  }
  return(as.vector(filter(x, c(1, -infinite.ar.coef), side=1)))
}
