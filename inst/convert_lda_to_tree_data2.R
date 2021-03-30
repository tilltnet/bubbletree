library(data.tree)

hc <- hclust(dist(USArrests), "ave")
ndhc <- as.Node(dhc)
as.igraph.Node(ndhc)
 v <-  as.list(ndhc, mode = "explicit")
jsonlite::toJSON(as.list(ndhc, mode = "explicit"), uname = TRUE)
dhc <- as.dendrogram(hc)
class(dhc)
hc$merge
hc$labels
class(dhc) <- "list"

