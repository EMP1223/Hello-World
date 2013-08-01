###### script for example 1 in the paper
library(randomForest)
library(gbm)
library(amdp)

# generates X and y:
ex1_sim = function(n,seednum=NULL){
	if(!is.null(seednum)){
		set.seed(seednum)
	}
	p = 3
	X = as.data.frame(matrix(runif(n * p, -1, 1), ncol = p))
	X = X[order(X[, 3]), ]
	colnames(X) = paste("x_", 1 : p, sep = "")
	bbeta1 = as.matrix(c(0.2, -5, 0))
	bbeta2 = as.matrix(c(0.2, 5, 0))

	y = array(NA, n)
	for (i in 1 : n){
		if (X[i, 3] < 0){
			y[i] = as.matrix(X[i, ]) %*% bbeta1
		} else {
			y[i] = as.matrix(X[i, ]) %*% bbeta2
		}
	}
	y = y + rnorm(n)
	Xy = as.data.frame(cbind(X, y))
	return(list(Xy=Xy,X=X,y=y))
}

#generate data
ex1_data = ex1_sim(1000)
Xy = ex1_data$Xy
X  = ex1_data$X
y  = ex1_data$y

#build all models
rf_mod = randomForest(X, y)

#how does random forest do in mse sense?
rf_mse = rep(NA,100)
for(i in 1:100){
	train = ex1_sim(1000)
	test  = ex1_sim(1000)
	fhat = randomForest(train$X, train$y)
	rf_mse[i] = mean( (test$y - predict(fhat, test$X) )^2 )
	cat(i); cat(" ")
}

gbm_mod = gbm(y ~ ., data = Xy, n.tree = 500, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5)
ntree = gbm.perf(gbm_mod, method = "cv")
summary(gbm_mod)
lm_mod = lm(y ~ . * . * ., Xy)
summary(lm_mod)

library(amdp)
amdp_obj_rf = amdp(rf_mod, X, predictor = 3, frac_to_build = 0.5)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(amdp_obj_rf, x_quantile = F, plot_pdp = T, frac_to_plot = 0.5)
amdpCluster(amdp_obj_rf, nClusters = 2)

set.seed(1989)
amdp_obj_gbm = amdp(gbm_mod, X, predictor = 3, predictfcn = function(object, newdata){predict(object, newdata, n.tree = ntree)}, frac_to_build = 1)
set.seed(1989)
plot(amdp_obj_gbm, plot_pdp = T, frac_to_plot = 0.01)
windows()
cluster.amdp(amdp_obj_gbm, nClusters = 2, colorvec = c(rgb(0.4, 0.4, 0.4), rgb(0.8, 0.8, 0.8)))

set.seed(1989)
amdp_obj_lm = amdp(lm_mod, X, predictor = 3, frac_to_build = 0.5)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
windows()
set.seed(1989)
plot(amdp_obj_lm, plot_pdp = T, frac_to_plot = 0.015)
amdpCluster(amdp_obj_lm, nClusters = 2)



