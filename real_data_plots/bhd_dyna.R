library(dynaTree)
library(MASS)
library(amdp)

#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

dtree_mod_lstat = dynaTrees(X, y, rprop = "luall", model = "linear", R = 10)

amdp_obj_dt = amdp(dtree_mod_lstat, X, predictor = "lstat", frac_to_build = 1, predictfcn = function(object, newdata){predict(object, newdata)$mean})

plot(amdp_obj_dt, x_quantile = F, plot_pdp = T, frac_to_plot = 0.5)
cluster.amdp(amdp_obj_rf, nClusters = 2)