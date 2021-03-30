#' @import data.tree

# i think I only use asNode.data.frame and as.list
convert_treemap <- function( treemap, rootname = "root" ){
  #  find where attributes start
  attrPos <- match("size", names(treemap) )


  # get rid of factors
  treemap2 <- as.data.frame(lapply(
    as.data.frame(treemap)
    ,function(x){
      if(is.factor(x)) {
        as.character(x)
      } else {
        x
      }
    }
  ),stringsAsFactors = F)

  if(attrPos > 2){
    treemap$pathString <- apply(
      treemap[,1:(attrPos-1)]
      ,MARGIN = 1
      ,function(row){
        paste0(
          c( rootname, row[which(!is.na(row))] )
          ,sep = "/~>/" # assume this will not exist
          ,collapse=""
        )
      }
    )
  } else {
    treemap$pathString <- paste(
      rootname
      ,treemap[,1]
      ,sep = "/~>/"
    )
  }

  dt <- as.Node(
    treemap[,-(1:(attrPos-1))]
    ,pathDelimiter = "/~>/"
  )

  # give an id to each node
  dt$Set( id = 1:dt$totalCount )

  # set size = to vSize
  dt$Set( size = dt$Get("vSize") )

  as.list(dt, unname = TRUE, mode = "explicit" )
}

convert_treemap(treemap = x, rootname = "clusterContainer")

HCtoJSON<-function(hc){

  labels<-hc$labels
  merge<-data.frame(hc$merge)

  for (i in (1:nrow(merge))) {

    if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }

  #eval(parse(text=paste0("JSON<-rjson::toJSON(node",nrow(merge), ")")))

  #return(JSON)
  merge
}
HCtoJSON(clust)
