\name{amdp}
\alias{amdp}
\title{
Create amdps
}
\description{
Create amdps
}
\usage{
amdp(object, X, y, j, predictfcn, frac_to_build = 1, verbose = TRUE, 
		plot = FALSE, logodds = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
A fitted model of any kind.
}
\item{X}{
Matrix or data frame of predictors (observations along rows).  To the extend that \code{object} requires
predictors in a certain format, \code{X} must obey that format. Note that \code{X} does not include the response.
}
\item{y}{
Optional vector of responses of the dataset, used to return the range of observed values. If missing, returns the 
range over all points in the amdp grid.
}
\item{j}{
Column number of \code{X} for which to calculate (... some noun goes here).  Alternatively \code{j}
can be a string corresponding to a column name in \code{X}.
}
\item{predictfcn}{
Optional predict function to use with \code{object}. Must accept arguments \code{object} and \code{newdata}. The
software does minimal further checking -- it is up to the user to ensure that \code{predictfcn(object = object, newdata = X)}
does something sensible. If not passed, the function looks for the default \code{predict} function associated with \code{class(object)}.
}
\item{frac_to_build}{
Fraction of the observations to build the curves for. If less than 1, the observations are chosen
to be equally distributed throughout the quantiles of xj.
}
\item{verbose}{
	If \code{verbose} is \code{TRUE} we print updates.
}
  \item{plot}{
	If \code{plot} is \code{TRUE} we make a plot using default plot settings at the end of the routine.
}
  \item{\dots}{
	Other parameters to be passed.
}
\item{num_grid_pts}{
The number of points along \code{X[, j]} the program will predict at. If unspecified, it will predict at all unique points.
If the number of unique points is less than the number of grid points specified, the program will warn the user and the number
of unique points will be used.
}
\item{logodds}{
If TRUE, the predicted values (which should be probabilities), are converted to logits
before the object is returned. 
}
}
\value{If assigned, returns an object of class \code{amdp}. The object is a list with elements:\cr
\item{apdps}{An \code{nrow(X)} by \code{length(gridpts)} matrix where row i is observations i marginal dependence curve.}
\item{gridpts}{A vector of the unique grid points at which all marginal curves are evaluated.}
\item{predictor}{The predictor used. If possible, returns the predictor's name; if not, returns column index in \code{X}.}
\item{xj}{The actual value of \code{X[, j]} for each observation.}
\item{actual_prediction}{The predicted value for each observation at it's actual vector of predictors.}
\item{logodds}{The \code{logodds} specified by the user.}
\item{xlab}{The x-axis label used for plotting}
\item{nominal_axis}{Whether or not this variable is treated as a nominal variable for plotting purposes (if it has more than 5 unique values, it's not).}
\item{N}{The number of rows in the inputted matrix or data frame}
\item{N}{The number of rows in the inputted matrix or data frame}

	amdp_obj = list(apdps = apdps, gridpts = grid_pts, predictor = predictor, xj = xj, actual_prediction = actual_predictions, 
			logodds = logodds, xlab = xlab, nominal_axis = nominal_axis, N = N, range_y = range_y)
			
			
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
pima_amdp = amdp(pima_rf, X = PimaIndiansDiabetes[,1:8], j = "glucose", predictfcn = pima_predict, logodds = T)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(pima_amdp, x_quantile = T, plot_pdp = T, frac_to_plot = .1)
}
}
