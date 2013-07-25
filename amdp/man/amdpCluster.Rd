\name{amdpCluster}
\alias{amdpCluster}
\title{
Does clustering of an amdp object, plots it.
}
\description{
Uses kmeans to do clustering on the rows of an amdp object and makes a plot.
}
\usage{
amdpCluster(apdp_obj, nClusters, plot = TRUE, plot_margin = 0.05, colorvec, plot_pdp=FALSE,
			x_quantile = FALSE, avg_lwd = 3, ...)
}
\arguments{
  \item{amdpobj}{
	Object of class \code{amdp}.
}
\item{nClusters}{
Number of clusters to find.
}
  \item{plot}{
	If \code{TRUE}, makes a plot of the clusters.
}
  \item{plot_margin}{
	Additional margin to add to vertical axis.
}
  \item{colorvec}{
	Optional vector of colors to use for the curves. If not passed one is generated.
}
\item{plot_pdp}{
If TRUE, plot the partial dependence plot in black with extra thickness in addition to the clusters.
}
  \item{x_quantile}{
	If \code{TRUE} the x-axis is transformed to quantiles.  This is useful because (a) it emphasizes regions
	of the marginal x empirical distribution of high mass and (b) tree-based methods are functions of x quantiles rather
	than the actual x anyway.
}
\item{avg_lwd}{
Average line width of the curves corresponding to each cluster. The thickness is
proportional to the number of observations in each cluster.
}
\item{\dots}{
	Additional arguments, for example arguments to the \code{kmeans} routine.
}
}
\value{
If assigned to a variable, the output of \code{kmeans} is returned.
}

\examples{
\dontrun{
require(randomForest)
require(mlbench)
library(amdp)
data(PimaIndiansDiabetes)

#fit a random forest
pima_rf = randomForest(diabetes~., PimaIndiansDiabetes)

#define a predict function that returns Prob(diabetes=pos)
pima_predict = function(object, newdata){
	preds = predict(object, newdata, type="prob")
	col_idx = which(colnames(preds)=="pos")
	preds[,col_idx] 
}

#get amdp object for the 'glucose' predictor, but converted to logit.
pima_amdp = amdp(pima_rf, X = PimaIndiansDiabetes[, 1 : 8], j = "glucose", predictfcn = pima_predict, frac_to_build = 0.25, logodds = TRUE)

clusters = cluster(pima_amdp, nClusters = 2)
}
}
