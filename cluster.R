cluster_pdps = function(apdp_obj, plot = TRUE, plot_margin = 0.05,...){
		
	#make all curves have avg value = 0.
	apdps = t(scale(t(apdp_obj$apdps), center=T, scale=F))  #sum(apdps[i,]) = 0 for all i

	#cluster
	cl = kmeans(apdps, ...)
	
	if(plot){
		rg = range(cl$centers); dist = rg[2]-rg[1]
		rg_min = rg[1] - plot_margin*dist;  rg_max = rg[2] + plot_margin*dist
		plot(apdp_obj$pred_test_values, cl$centers[1,], type='n', ylim = c(rg_min, rg_max), ylab = paste("cluster",expression(hat(y))))
		for(i in 1:nrow(cl$centers)){
			points(apdp_obj$pred_test_values, cl$centers[i, ], col = COLORS[i %% length(COLORS)], type = "l")
		}
	}
	invisible(cl)
}
