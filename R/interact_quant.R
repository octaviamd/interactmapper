#' @title interact_quant
#' @description Generates an interactive dimension reduction plot that is color-coordinated with quantitative data related to your dataset.
#' @param count_data dataframe of input data
#' @param quant_info_name title of quantitative data
#' @param quant_info quantitative data used to color points of plot
#' @param dim_red_meth dimension reduction method: "pca" or "umap" are the options available now
#' @param your_palette the palette of your choice (please see \code{\link[colourvalues]{colour_values}} documentation to see options)
#' @return  an interactive dimension reduction plot that is color-coordinated with quaantitative data related to your dataset.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  interact_quant(iris[,1:4], "petal Width",  iris[,4], "UMAP")
#'  }
#' }
#' @rdname interact_quant
#' @export 
interact_quant=function(count_data, quant_info_name, quant_info, dim_red_meth=c("UMAP", "PCA"), your_palette){
  if(toupper(dim_red_meth)=="UMAP"){
    dim_data<-umakea(count_data)
  }
  if(toupper(dim_red_meth)=="PCA"){
    dim_data<-pca_maker(count_data)
  }
  dim_data<-color_me_grad(dim_data, quant_info, your_palette)
  p1<-plotly::plot_ly(dim_data, x = dim_data[,1], y = dim_data[,2], type = 'scatter', mode = 'markers', marker=list(color= ~colours),
              hoverinfo = 'text', text = ~paste('Sample Name: ', dim_data$sample.labels, '<br>', quant_info_name, ':', quant_info))
  
  return(p1)
}
