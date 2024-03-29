% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interact_quant.R
\name{interact_quant}
\alias{interact_quant}
\title{interact_quant}
\usage{
interact_quant(count_data, quant_info_name, quant_info,
  dim_red_meth = c("UMAP", "PCA"), your_palette)
}
\arguments{
\item{count_data}{dataframe of input data}

\item{quant_info_name}{title of quantitative data}

\item{quant_info}{quantitative data used to color points of plot}

\item{dim_red_meth}{dimension reduction method: "pca" or "umap" are the options available now}

\item{your_palette}{the palette of your choice (please see \code{\link[colourvalues]{colour_values}} documentation to see options)}
}
\value{
an interactive dimension reduction plot that is color-coordinated with quaantitative data related to your dataset.
}
\description{
Generates an interactive dimension reduction plot that is color-coordinated with quantitative data related to your dataset.
}
\examples{
\dontrun{
if(interactive()){
 interact_quant(iris[,1:4], iris[,4], "UMAP")
 }
}
}
