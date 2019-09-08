#' @title pca_maker
#' @description This function takes in your count data and returns a dataframe with the two feature columns resulting from the PCA dimension reduction and a sample names column
#' @param count_data  A dataframe holding the data that you want to apply umap to
#' @return a dataframe with the two feature columns resulting from the PCA dimension reduction and a sample names column
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pca_maker
#' @export 
pca_maker=function(count_data){
  pca_results<-prcomp(count_data)
  p<-pca_results$x[,1:2]
  sample.labels<-rownames(count_data)
  p<-data.frame(cbind.data.frame(p, sample.labels, stringsAsFactors=FALSE))
  print(str(p))
  return(p)
}
