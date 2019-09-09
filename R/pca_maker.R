#' @title pca_maker
#' @description Returns a dataframe with the two feature columns resulting from the PCA (principal components analysis) dimension reduction and a sample names column
#' @param count_data  A dataframe with the input data
#' @return a dataframe with the two feature columns resulting from the PCA dimension reduction and a sample names column (where sample names are obtained from the rownames of count_data)
#' @examples 
#'  p<-pca_maker(iris[,1:4])
#' @seealso 
#'  \code{\link[prcomp]
#' @rdname pca_maker
#' @export 
pca_maker=function(count_data){
  pca_results<-prcomp(count_data)
  p<-pca_results$x[,1:2]
  sample.labels<-rownames(count_data)
  p<-data.frame(cbind.data.frame(p, sample.labels, stringsAsFactors=FALSE))
  return(p)
}
