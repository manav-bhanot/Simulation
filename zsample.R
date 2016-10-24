#This function returns a sample of the form ((1/n)(X1+...+Xn) - lambda)/sqrt(sigma/n)
#In this case X~E(lambda), with lambda = 1.
zsample = function(n)
{
	x = rexp(n)
	lambda = sum(x)/n
	z = (lambda-1)*sqrt(n)
	return(z)
}

#m = number of z samples, n = number of random samples used in one z sample
rzsample = function(m,n)
{
	z = vector(mode="numeric",m)

	for(i in 1:m)
		z[i] = zsample(n)

	return(z)
}

#create a vector of z samples
z = rzsample(10000,10000)

#plot the distribution of x using a histogram
h = hist(z,xlim=c(-3,3),breaks=100)

#change the scale to density instead of frequency
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE)


	
