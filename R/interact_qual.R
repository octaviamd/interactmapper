#' @title interact_qual
#' @description Generates an interactive dimension reduction plot that is color-coordinated with qualitative data related to your dataset.
#' @param count_data dataframe of input data
#' @param qual_data qualitative data used to color points of plot
#' @param dim_red_meth dimension reduction method: "pca" or "umap" are the options available now
#' @return an interactive dimension reduction plot that is color-coordinated with qualitative data
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  interact_qual(iris[,1:4], iris$Species, "UMAP")
#'  }
#' }
#' @rdname interact_qual
#' @export 
interact_qual=function(count_data, qual_data, dim_red_meth=c("UMAP", "PCA")){
  if(toupper(dim_red_meth)=="UMAP"){
    dim_data<-umakea(count_data)
  }
  if(toupper(dim_red_meth)=="PCA"){
    dim_data<-pca_maker(count_data)
  }
  sample_ggplot <- ggplot2::ggplot(dim_data, ggplot2::aes(x=dim_data[,1], y=dim_data[,2], colour=qual_data, sample.name=dim_data$sample.labels)) + ggplot2::geom_point()
  umap_plot<-plotly::ggplotly(sample_ggplot, tooltip = c("sample.name", "colour"))
  return(umap_plot)
}
