random_projection <- function(n,d=2) {
  I <- diag(n)
  w <- matrix(rnorm(2*n), nrow=n)
  A <- I %*% w
  A_stand <- (A - mean(A[,1]))/sd(A[,1]) # center and rescale
  dt <- data.table(A_stand, type="proj")
  v <- matrix(rnorm(2*n), nrow=n)
  dt <- rbind(dt, data.table(v, type="normal"))
  dt[,n:=n]
  return(dt)
}
n <- round(exp(4:9))
dt <- rbindlist(
  lapply(
    n,
    function(i) {
      random_projection(i)
    }
  )
)
p <- ggplot(data = dt, aes(x=V1, y=V2, colour=type)) +
  geom_point() +
  facet_wrap(~factor(n), ncol=3)
p
