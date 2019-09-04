#' @title umakea
#' @description This function takes in your count data and returns a dataframe with the two feature columns resulting from the UMAP dimension reduction and a sample names column
#' @param count_data A dataframe holding the data that you want to apply umap to
#' @return a dataframe with the two feature columns resulting from the UMAP dimension reduction and a sample names column
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[umap]{umap}}
#' @rdname umakea
#' @export 
#' @importFrom umap umap
umakea=function(count_data){
  #making sure that there are no completely identical cols, otherwise umap won't work
  filt_set<-count_data[ , apply(count_data, 2, var) != 0]
  #running umap
  umap_set<-umap::umap(filt_set)
  #extracting the names of the samples from the original count dataframe
  sample.labels=rownames(count_data)
  #making a dataframe that has the two layout outputs from umap and the sample names
  umap_data<-data.frame(umap1=umap_set$layout[,1], umap2=umap_set$layout[,2], sample.labels, stringsAsFactors = FALSE)
  #function now outputs the resulting dataframe
  return(umap_data)
}
