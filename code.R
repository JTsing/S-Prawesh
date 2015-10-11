library(fda)
final1 <- read.csv("C:/Users/Jitendra/Desktop/data/time/final1.csv")
names=names(final1)
namedate=final1$Date[1:135]

##Dataframe to matrix
final=data.matrix(final1[1:135,2:11])
final2=matrix(final,nrow=135,ncol=10,dimnames=list(namedate,names[2:11]))

##Normalize The Data by mean and Standard deviation
final3=apply(final2, MARGIN = 2, FUN = function(X) (X - mean(X))/sd(X)) 




#### Functional PCA
dayrange  = c(0,135)
daybasis  = create.fourier.basis(dayrange, 135)

Lcoef        = c(0,(2*pi/diff(dayrange))^2,0)
harmaccelLfd = vec2Lfd(Lcoef, dayrange)

lambda      = 1e6
fdParobj    = fdPar(daybasis, harmaccelLfd, lambda)
final.fit = smooth.basis(day.5[1:135], final3, fdParobj)
final.fd  = final.fit$fd
nharm = 2
final.pcalist = pca.fd(final.fd, nharm)

print(final.pcalist$values[1:4])

plot.pca.fd(final.pcalist, type = 'l', main = "(a)")
plot(final.pcalist, expand=.5, type = 'l', main = "(b)")
matplot(day.5[1:135], final3, type = "l",ylim = c(-1.5,10) , xlab =" time", ylab = "score")
#






final.rotpcalist = varmx.pca.fd(final.pcalist)

plot.pca.fd(final.rotpcalist, expand=.5)

# Figure 7.3

rotpcascores = final.rotpcalist$scores

plot(rotpcascores[,1], rotpcascores[,2], type="p", pch="o",
     xlab="Rotated Harmonic I", ylab="Rotated Harmonic II")

