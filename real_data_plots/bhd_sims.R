library(randomForest)
library(missForest)
library(MASS)
library(amdp)


###########GRID COMPUTING
LAST_NAME = "kapelner"
NOT_ON_GRID = length(grep("wharton.upenn.edu", Sys.getenv(c("HOSTNAME")))) == 0

if (NOT_ON_GRID){
	directory_where_code_is = "C:\\Users\\kapelner\\workspace\\CGMBART_GPL"
} else {
	directory_where_code_is = getwd()
}
setwd(directory_where_code_is)

source("bartMachine/R/bart_package_inits.R")
source("bartMachine/R/bart_package_builders.R")
source("bartMachine/R/bart_package_predicts.R")
source("bartMachine/R/bart_package_data_preprocessing.R")
source("bartMachine/R/bart_package_plots.R")
source("bartMachine/R/bart_package_variable_selection.R")
source("bartMachine/R/bart_package_f_tests.R")
source("bartMachine/R/bart_package_summaries.R")

#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

bart_machine = build_bart_machine(X, y)

#create amdp's for all features in the boston housing data
amdb_bart_objs = list()
for (j in colnames(X)){
	amdb_bart_objs[[j]] = amdp(bart_machine, X, j, num_grid_pts = 100)
}

for (j in colnames(X)){
	windows()
	par(mfrow = c(1, 3))
	plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
	plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02)
	cluster_amdp(amdb_bart_objs[[j]], nClusters = 2)
}


lm_mod = lm(medv ~ ., Boston)
apdp(lm_mod, X, 6, num_grid_pts = 30)

lm_mod = lm(medv ~ ., Boston)
apdp(lm_mod, X, 6, num_grid_pts = 30)
apdp(lm_mod, X, "lstat", num_grid_pts = 30)

library(randomForest)
rf_mod = randomForest(medv ~ ., Boston)
head(X)
#crim zn indus chas   nox    rm  age    dis rad tax ptratio  black lstat

apdp(rf_mod, X, "crim", num_grid_pts = 50, pct_to_plot = 0.2)
apdp(rf_mod, X, "zn", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "indus", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "chas", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "nox", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "rm", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "age", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "dis", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "tax", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "ptratio", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "black", num_grid_pts = 50, pct_to_plot = 0.05)
apdp(rf_mod, X, "lstat", num_grid_pts = 50, pct_to_plot = 0.05)

#library(gbm)
#gbm_mod = gbm(medv ~ ., data = Boston)
#apdp(gbm_mod, X, 6, num_grid_pts = 30, n.trees = 500)


rf_mod = randomForest(medv ~ ., Boston)
apdp_obj = apdp(rf_mod, X, "rm", num_grid_pts = 30)

#scramble this
#apdps_scrambled = matrix(NA, nrow = nrow(apdp_obj$apdps), ncol = ncol(apdp_obj$apdps))
#for (j in 1 : ncol(apdp_obj$apdps)){
#	apdps_scrambled[, j] = sample(apdp_obj$apdps[, j])
#}
#
#for (b in 1 : num_permutes){
#	#permute apdps_diffs
#	
#	apdps_diffs = t(apply(apdps_scrambled, 1, diff))
#	for (j in 1 : ncol(apdps_scrambled) - 1){
#		apdps_diffs[, j] = sample(apdps_diffs[, j])
#		
#	}
#	apdp_diff_plot(apdp_obj, apdps_diffs, plot_margin)
#}


apdp_diffs(apdp_obj)






