#' @title interact_multi
#' @description FUNCTION_DESCRIPTION
#' @param count_data PARAM_DESCRIPTION
#' @param main_info_data PARAM_DESCRIPTION
#' @param sec_info_data PARAM_DESCRIPTION
#' @param dim_red_meth PARAM_DESCRIPTION
#' @param your_palette PARAM_DESCRIPTION
#' @param main_info_name PARAM_DESCRIPTION
#' @param sec_info_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname interact_multi
#' @export 
interact_multi=function(count_data, main_info_data, sec_info_data, dim_red_meth, your_palette, main_info_name, sec_info_name){
  if(toupper(dim_red_meth)=="UMAP"){
    dim_data<-umakea(count_data)
  }
  if(toupper(dim_red_meth)=="PCA"){
    dim_data<-pca_maker(count_data)
  }
  dim_data<-color_me_grad(dim_data, main_info_data, your_palette)
  if(missing(main_info_name)==TRUE){
    if(is.null(colnames(main_info_data))==FALSE){
      main_info_name<-colnames(main_info_data)
    }
    else{
      if(is.null(colnames(main_info_data))==FALSE){
        main_info_name<-names(main_info_data)
      }
      else{
        main_info_name<-"Main Feature of Interest"
      }
    }
  }
  if(missing(sec_info_name)==TRUE){
    if(is.vector(sec_info_data)==TRUE){
      sec_info_name<-"Secondary info:"
    }
    else{
    sec_info_name<-colnames(sec_info_data)
    }
  }
  a<-c()
  print(str(sec_info_data))
  print(dim(sec_info_data))
  if(is.vector(sec_info_data)==TRUE){
    a<-paste(a,sec_info_name,': ', sec_info_data, '<br>', sep="")
  }
  else{
  for(i in 1:ncol(sec_info_data)){
    a<-paste(a,sec_info_name[i],': ', sec_info_data[,i], '<br>', sep="")
  }
  }
  p1<-plotly::plot_ly(dim_data, x = dim_data[,1], y = dim_data[,2], type = 'scatter', mode = 'markers', marker=list(color= ~colours),
             hoverinfo = 'text', text = ~paste('Sample Name: ', dim_data$sample.labels, '<br>', main_info_name, ': ', main_info_data,'<br>', a))
  
  return(p1)
}
