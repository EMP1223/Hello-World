

n = 1000
p = 3
X = matrix(rnorm(n * p), ncol = p)
colnames(X) = paste("x_", 1 : p, sep = "")
bbeta1 = as.matrix(c(5, -2, 0))
bbeta2 = as.matrix(c(-5, -2, 0))

y = array(NA, n)
for (i in 1 : n){
	if (X[i, 2] < 0.5){
		y[i] = X[i, ] %*% bbeta1
	} else {
		y[i] = X[i, ] %*% bbeta2
	}
}

Xy = as.data.frame(cbind(X, y))

library(randomForest)
rf_mod = randomForest(X, y)
lm_mod = lm(y ~ x_1 * x_2, Xy)
summary(lm_mod)


amdp_obj = amdp(rf_mod, X, j = 2, frac_to_build = 1)

#plot only 10% of curves with quantiles, actual pdp, and original points. 
plot(amdp_obj, x_quantile = F, plot_pdp = T, frac_to_plot = 0.01)
cluster_pdps(amdp_obj, centers = 2)