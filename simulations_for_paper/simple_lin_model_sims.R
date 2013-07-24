

n = 1000
p = 2
X = matrix(runif(n * p, -1, 1), ncol = p)
X = cbind(X, rbinom(n, 1, 0.5)) #indicator variable
colnames(X) = paste("x_", 1 : (p + 1), sep = "")
bbeta1 = as.matrix(c(0.2, 5, 0))
bbeta2 = as.matrix(c(0.2, -5, 0))

y = array(NA, n)
for (i in 1 : n){
	if (X[i, 3] == 1){
		y[i] = X[i, ] %*% bbeta1
	} else {
		y[i] = X[i, ] %*% bbeta2
	}
}

y = y + rnorm(n, 1)

Xy = as.data.frame(cbind(X, y))
X = as.data.frame(X)
lm_mod = lm(y ~ x_1 + x_2 + x_2 : x_3, Xy)
summary(lm_mod)


library(randomForest)
rf_mod = randomForest(X, y)


library(amdp)
amdp_obj = amdp(rf_mod, X, predictor = 2, frac_to_build = 1)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
colorvec = array(NA, nrow(X))
for (i in 1 : nrow(X)){
	colorvec[i] = ifelse(X[i, 3] == 1, "red", "green")
}
plot(amdp_obj, x_quantile = F, plot_pdp = T, frac_to_plot = 0.1, colorvec = colorvec)
windows()
cluster.amdp(amdp_obj, nClusters = 2)




lm_amdp_obj = amdp(lm_mod, as.data.frame(X), predictor = 2, frac_to_build = 1)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
colorvec = array(NA, nrow(X))
for (i in 1 : nrow(X)){
	colorvec[i] = ifelse(X[i, 3] == 1, "red", "green")
}
windows()
plot(lm_amdp_obj, x_quantile = F, plot_pdp = T, frac_to_plot = 0.1, colorvec = colorvec)
windows()
cls = cluster.amdp(lm_amdp_obj, nClusters = 2)


gbm_mod = gbm(y ~ ., data = Xy, n.tree = 500, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5)
ntree = gbm.perf(gbm_mod, method = "cv")
summary(gbm_mod)

amdp_obj_gbm = amdp(gbm_mod, X, predictor = 2, predictfcn = function(object, newdata){predict(object, newdata, n.tree = ntree)}, frac_to_build = 1)
plot(amdp_obj_gbm, x_quantile = F, plot_pdp = T, frac_to_plot = 0.1, colorvec = colorvec)