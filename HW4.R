#Ndeye Khadidiatou Mbodj
#HW4
#1
#i The lambda used is 5.
#ii The p used id 0.8.
 x=rbinom(1000,15,0.3)
 table(x)
#there is a difference between the number of 0 observed, but if we do not take that into account, the rest of the plots are really similar.

#2
#i we use k=4 because we can observe 4 very clear delimitations.
#ii 
em.fit=normalmixEM(quadwts, k=4)
number of iterations= 124 
> em.fit$lambda
#[1] 0.3794767 0.1443920 0.1256899 0.3504414
> em.fit$mu
#[1] 306.9796 321.9005 330.8311 347.0181
#iii our EM output agree with the theorical values, because the values obtained with em.fit$mu are nearly the same as the theorical values.

#3
#hamming Distance=2
#Jaccard distance= 3/5

#4
ncars = nrow(mtcars) 
dist.cars = matrix(0, nrow=ncars, ncol=ncars) 
for (i in 1:ncars)
	for (j in 1:ncars)
		dist.cars[i, j] = sqrt(sum((mtcars[i, ] - mtcars[j, ])^2))
