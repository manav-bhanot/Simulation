#modify this function when sampling a different random variable
sample = function()
{
	x = runif(1)
	return(cos(x)*exp(sin(x)))
}
	
# n: number of samples, delta: the degree of confidence sought (e.g. delta = 0.95), sample: function used to generate a single sample
imc = function(n,delta,sample)
{
	sum = 0
	sumsq = 0
	
	for(i in 1:n)
	{
		x = sample()
		sum = sum + x
		sumsq = sumsq + x*x
	}

	lambda = sum/n
	sigmasq = (sumsq + (lambda*lambda*n))/(n-1)
	se = sqrt(sigmasq/n)
	re = se/lambda
	qdelta = qnorm((1+delta)/2)
	ci_left = lambda-qdelta*se
	ci_right = lambda+qdelta*se
	
	cat("Number of samples:",n,"\n")
	cat("Sample mean:",lambda,"\n")
	cat("Sample variance:",sigmasq,"\n")
	cat("Standard error:",se,"\n")
	cat("Relative error:",re,"\n")
	cat(delta,"-confidence interval: [",ci_left,",",ci_right,"]\n",sep="")
}
	
		
	
