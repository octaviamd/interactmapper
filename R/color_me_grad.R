#' @title color_me_grad
#' @description function to make color info dataframe corresponding to your feature of interest for your data that you want represented via color, then adding it to the dimension reduced dataframe
#' @param u_data dataframe holding output of dimension reduction
#' @param info_overlay numerical data that color_me_grad will represent with color gradations
#' @param your_palette the palette of your choice (please see colourvalues documentation to see options)
#' @return dataframe with original dimension reduction and column with color values, as well as the info_overlay column
#' @examples 
#' color_me_grad(umakea(iris[,1:4]), iris$Sepal.Length, "viridis")
#' @seealso 
#'  \code{\link[colourvalues]{colour_values}}
#' @rdname color_me_grad
#' @export 
#' @importFrom colourvalues colour_values
color_me_grad=function(u_data, info_overlay, your_palette){
  #making the dataframe for the colours
  df<-data.frame()
  df<-cbind.data.frame(x=as.numeric(info_overlay), col="hi")
  if(nrow(df)!=nrow(u_data)){
    print("The length of your info to overlay on your plot doesn't match the number of points to plot")
    print("Number of rows in info_overlay (for coloring points):")
    print(nrow(df))
    print("Number of rows in umap data (points to plot):")
    print(nrow(u_data))
    return(u_data)
  }
  if(missing(your_palette)==TRUE){
    your_palette<-"matlab_like2"
  }
  df$col <- colourvalues::colour_values(df$x, palette = your_palette)
  if(length(which(colnames(u_data)=="colours"))==0){
    u_data<-data.frame(u_data, colours=df$col)
  }
  else{
    u_data$colours<-df$col
  }
  u_data<-data.frame(u_data, info_overlay)
  return(u_data)
}
