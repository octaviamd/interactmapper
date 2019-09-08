#' @title interact_qual
#' @description FUNCTION_DESCRIPTION
#' @param count_data PARAM_DESCRIPTION
#' @param qual_data PARAM_DESCRIPTION
#' @param dim_red_meth PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname interact_qual
#' @export 
#' @importFrom ggplot plotly
interact_qual=function(count_data, qual_data, dim_red_meth){
  if(toupper(dim_red_meth)=="UMAP"){
    dim_data<-umakea(count_data)
  }
  if(toupper(dim_red_meth)=="PCA"){
    dim_data<-pca_maker(count_data)
  }
  sample_ggplot <- ggplot2::ggplot(dim_data, aes(x=dim_data[,1], y=dim_data[,2], colour=qual_data)) + geom_point()
  umap_plot<-plotly::ggplotly(sample_ggplot)
  return(umap_plot)
}
