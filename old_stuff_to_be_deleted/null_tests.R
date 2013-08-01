
var_test = function(apdp_obj, npair = 1000, ... ){
	apdps = apdp_obj$apdps #each row is an observation in original training data.
						   #each row is a curve in the plot
	
	#make all curves have avg value = 0.
	apdps = t(scale(t(apdps), center=T, scale=F))  #sum(apdps[i,]) = 0 for all i
		
	N = nrow(apdps)
	
	#Now we need to randomly sample pairwise diffs. can't use combinat
	#because the number of pairs takes off. Rather we take two samples A & B
	#each of size=npair2. Then eliminate those where A[i]=B[i]. To ensure
	#we get at least npair valid pairs with high probability, we set
	#npair2 > npair. 
	
	# Pr(A[i]=B[i])=1/N ==> Pr(A[i]!=B[i])=Pr(valid pairwise dif) = (N-1)/N
	# E(#valid) = npair2*(N-1)/N, SD(#valid)=sqrt( npair2*(N-1)/(N^2) )
	# #valid ~N(npair2*(N-1)/N,npair2 * (N-1)/(N^2))
	# So...
	# Pr(#valid >= npair) = k 
	# <==> Pr( (#valid-E(#valid)) / SD(#valid) >= (npair-E(#valid)) / SD(#valid) ) = k
	# <==> Pr( Z >= (npair-E(#valid)) / SD(#valid) ) = k
	# <==> (npair-E(#valid)) / SD(#valid) ) = qnorm(k) #solve for npair2!
	# turns out to be a quadratic eqn.
	
	gridpt_pd = function(gridpt_vals){
			A = sample(N,size=npair*1.3,replace=T)
			B = sample(N,size=npair*1.3,replace=T)
			difs = gridpt_vals[A[(A!=B)]] - gridpt_vals[B[(A!=B)]]
			return(difs)
	}
	difs_sample = apply(apdps,2,gridpt_pd)  #each is in a list. this is good because they can have different lengths...
	all_difs = unlist(difs_sample)          #list --> single vector

	#compare variance of pairwise difs to chisq.
	d_scale = all_difs/sd(all_difs)
	ssd = sum(d_scale^2)
	t2 = 1 - pchisq(ssd, df = (length(all_difs)-1))
	return(t2)
}


