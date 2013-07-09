

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
apdp(rf_mod, X, 1, num_grid_pts = 50, pct_to_plot = 0.05)
apdp_obj = apdp(rf_mod, X, 2, num_grid_pts = 50, pct_to_plot = 0.005)
apdp_obj = apdp(rf_mod, X, 3, num_grid_pts = 50, pct_to_plot = 0.05)


apdp_obj = apdp(lm_mod, X, 1, num_grid_pts = 50, pct_to_plot = 0.05)

apdp_obj
