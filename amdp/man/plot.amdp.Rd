\name{plot.amdp}
\alias{plot.amdp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots an amdp object.
}
\description{
Plots an amdp object.
}
\usage{
plot.amdp(amdpobj, xtest_margin = 0, plot_margin = 0.05, frac_to_plot = 1, 
		 plot_orig_pts = TRUE, colorvec, x_quantile = FALSE, plot_pdp = FALSE, ...)
}
\arguments{
  \item{amdpobj}{
	Object of class \code{amdp}.
}
  \item{xtest_margin}{
	Additional margin to add to horizontal axis.
}
  \item{plot_margin}{
	Additional margin to add to vertical axis.
}
  \item{frac_to_plot}{
	A number in (0,1] expressing the fraction of all curves to plot. Useful for un-cluttering plots with many observations.
}
  \item{plot_orig_pts}{
	If \code{TRUE} plots a dot on each curve for that observation's prediction at it's actual x value.
}
  \item{colorvec}{
	Optional vector of colors to use for the curves. If not passed it is generated randomly.
}
  \item{x_quantile}{
	If \code{TRUE} the x-axis is transformed to quantiles.  This is useful because (a) it emphasizes regions
	of the marginal x empirical distribution of high mass and (b) tree-based methods are functions of x quantiles rather
	than the actual x anyway.
}
\item{plot_pdp}{
If TRUE, plot the partial dependence plot in black with extra thickness in addition to all the individual lines.
Note that if \code{amdpobj} was built with \code{frac_to_build} less than 1, the line plotted is not the "true"
pdp because the averaging is done only over those observations sampled to build \code{amdpobj}.
}
  \item{\dots}{
	Additional arguments.
}
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

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(pima_amdp, x_quantile = T, plot_pdp = T, frac_to_plot = .5)
plot(pima_amdp, x_quantile = T, plot_pdp = T, frac_to_plot = .5, centered = TRUE)

pima_amdp = amdp(pima_rf, X = PimaIndiansDiabetes[, 1 : 8], j = "age", predictfcn = pima_predict, frac_to_build = 0.25, logodds = TRUE)

set.seed(1984)
plot(pima_amdp, x_quantile = T, plot_pdp = T, frac_to_plot = .1)
set.seed(1984)
plot(pima_amdp, x_quantile = T, plot_pdp = F, frac_to_plot = .008, centered = TRUE, centered_percentile = 0.02)
abline(a = 0, b = 0, lwd = 5, col = "red")

cluster_pdps(pima_amdp, centers = 2)

###also regression??
}
}
