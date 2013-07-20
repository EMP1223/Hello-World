cluster_pdps = function(apdp_obj, plot = TRUE, plot_margin = 0.05, colorvec, ...){
		
	#make all curves have avg value = 0.
	apdps = t(scale(t(apdp_obj$apdps), center=T, scale=F))  #sum(apdps[i,]) = 0 for all i


	#cluster
	cl = kmeans(apdps, ...)
	nclust = nrow(cl$centers)
	if(missing(colorvec)){
		colorvec = rgb(runif(nclust, 0, 0.7), runif(nclust, 0, 0.7), runif(nclust, 0, 0.7))
	}
	
	if(plot){
		rg = range(cl$centers); dist = rg[2]-rg[1]
		rg_min = rg[1] - plot_margin*dist;  rg_max = rg[2] + plot_margin*dist
		plot(apdp_obj$gridpts, cl$centers[1,], type='n', ylim = c(rg_min, rg_max), ylab = paste("cluster",expression(hat(y))))
		for(i in 1:nrow(cl$centers)){
			points(apdp_obj$gridpts, cl$centers[i, ], col = colorvec[i], type = "l")
		}
	}
	invisible(cl)
}
