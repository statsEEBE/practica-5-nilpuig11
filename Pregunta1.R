#exs q2-> 1 identificar modelo- binomial, binomial negativa (resistencia), hipergeometrica, poisson, uniforme, exponencial,normal
#binomial (encuesta) (repetición de ensayo de bernoulli) P(X=13)=dbinom(13,n,p)
#identificar parámetros
#problema 1
#distribución normal

mu<-95.3
sigma<-5.7
#N->N(mu,sigma^2)
curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120),col="red")
#a formula directa E(Y)=N*u
Y<- function(i){sum(rnorm(4,mean=mu, sd=sigma))} #creem la nova variable aleatoria de la suma
Y100000<-sapply(1:100000,Y)
mean(Y100000)#mitjana de 1000 repeticions
hist(Y100000,freq=FALSE)
#teorema suma muestral (foto 14/11)
curve(dnorm(x,4*mu, 2*sigma),col="red",add=TRUE)
#resposta=4*mu
4*mu
#b formula directa V(Y)=n*sigma^2
Y<- function(i){sum(rnorm(100,mean=mu, sd=sigma))} #creem la nova variable aleatoria de la suma
Y100000<-sapply(1:100000,Y)
var(Y100000)
100*sigma^2

#c
curve(dnorm(x,mean=mu,sd=sigma),xlim=c(80,120),col="red")
#P(X>103)
1-pnorm(103,mu,sigma)

#d
Xbar<- function(i){mean(rnorm(4,mean=mu, sd=sigma))} 
Xbar100000<-sapply(1:100000,Xbar)
hist(Xbar100000)
mean(Xbar100000<98)
hist(Xbar100000,freq=FALSE)
curve(dnorm(x,mu,sigma/sqrt(4)),add=TRUE,col="red")
pnorm(98,mu,sigma/sqrt(4)) #foto

#e (foto)
Ssq<- function(i){var(rnorm(100,mean=mu, sd=sigma))} 
Ssq100000<-sapply(1:100000,Ssq)
hist(Ssq100000,freq=FALSE)
mean(Ssq100000>32)
hist(Ssq100000*(100-1)/sigma^2,freq=FALSE)
curve(dchisq(x,100-1),add=TRUE,col="red")     
w<-32*(100-1)/sigma^2
w
1-pchisq(w,100-1)
mean(Ssq100000>32)
