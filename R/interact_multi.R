#' @title interact_multi
#' @description Generates an interactive dimension reduction plot that is color-coordinated with a main feature of interest related to your dataset, and displays additional secondary information upon mouse hover over points.
#' @param count_data dataframe of input data
#' @param main_info_data data for main feature of interest for samples of dataset
#' @param sec_info_data data for secondary feature of interest for samples of dataset
#' @param dim_red_meth dimension reduction method: "pca" or "umap" are the options available now
#' @param your_palette the palette of your choice (please see \code{\link[colourvalues]{colour_values}} documentation to see options)
#' @param main_info_name title for main feature of interest
#' @param sec_info_name title (or vector of titles in the case of multiple) for the secondary features of interest
#' @return interactive dimension reduction plot that is color-coordinated with a main feature of interest related to your dataset, and displays additional secondary information upon mouse hover over points.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  interact_multi(iris[,1:4], iris$Sepal.Length, iris$Species, "UMAP", "viridis", "Sepal Length", "Species")
#'  interact_multi(iris[,1:4], iris$Species, iris[,1:2], "UMAP", "viridis", "Species", c("Sepal Length", "Sepal Width"))
#'  }
#' }
#' @rdname interact_multi
#' @export 
interact_multi=function(count_data, main_info_data, sec_info_data, dim_red_meth=c("UMAP", "PCA"), your_palette, main_info_name, sec_info_name){
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
    if(is.vector(sec_info_data)==TRUE | is.null(dim(sec_info_data))==TRUE){
      sec_info_name<-"Secondary info"
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
    if(is.null(dim(sec_info_data))==TRUE){
      a<-paste(a,sec_info_name,': ', as.character(sec_info_data), '<br>', sep="")
    }
    else{
  for(i in 1:ncol(sec_info_data)){
    a<-paste(a,sec_info_name[i],': ', sec_info_data[,i], '<br>', sep="")
  }
    }
  }
  p1<-plotly::plot_ly(dim_data, x = dim_data[,1], y = dim_data[,2], type = 'scatter', mode = 'markers', marker=list(color= ~colours),
             hoverinfo = 'text', text = ~paste('Sample Name: ', dim_data$sample.labels, '<br>', main_info_name, ': ', main_info_data,'<br>', a))
  
  return(p1)
}
