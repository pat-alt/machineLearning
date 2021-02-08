knn_classifier <- function(X,y,k,...) {
  row_idx <- 1:nrow(X)
  distances <- data.table(t(combn(row_idx,2)))
  distances[,dist:=c(dist(X,...))]
  distances_rev <- copy(distances)
  setnames(distances_rev, c("V1", "V2"), c("V2", "V1"))
  distances <- rbind(distances, distances_rev)
  setorder(distances, V1, dist)
  setnames(distances, c("V1", "V2"), c("X", "neighbour"))
  distances[,y_neighbour:=y[neighbour]]
  fitted <- distances[,median(y_neighbour[1:k]),by=X]$V1
  return(fitted)
}