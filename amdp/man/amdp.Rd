\name{amdp}
\alias{amdp}
\title{
Create amdps
}
\description{
Create amdps
}
\usage{
amdp(object, X, j, predictfcn, frac_to_build = 1, verbose = TRUE, plot = FALSE, plot_logit = F, ...)
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
\item{plot_logit}{
If True, the predicted values (which should be probabilities), are converted to logits
before the object is returned. 
}
}
\value{If assigned, returns an object of class \code{amdp}. The object is a list with elements:\cr
\item{apdps}{An \code{nrow(X)} by \code{length(gridpts)} matrix where row i is observations i marginal dependence curve.}
\item{gridpts}{A vector of the unique grid points at which all marginal curves are evaluated.}
\item{predictor}{The predictor used. If possible, returns the predictor's name; if not, returns column index in \code{X}.}
\item{xj}{The actual value of \code{X[,j]} for each observation.}
\item{actual_prediction}{The predicted value for each observation at it's actual vector of predictors.}
}
\examples{
\dontrun{
	#example goes here. 
}
}
