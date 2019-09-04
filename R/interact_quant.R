#' @title interact_quant
#' @description FUNCTION_DESCRIPTION
#' @param count_data PARAM_DESCRIPTION
#' @param quant_info_name PARAM_DESCRIPTION
#' @param quant_info PARAM_DESCRIPTION
#' @param dim_red_meth PARAM_DESCRIPTION
#' @param your_palette PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname interact_quant
#' @export 
interact_quant=function(count_data, quant_info_name, quant_info, dim_red_meth, your_palette){
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
