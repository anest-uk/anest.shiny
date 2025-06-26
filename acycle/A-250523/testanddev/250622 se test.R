x0 <- as.matrix(summary(x12)$cov.unscaled)
x1 <- eigen(x0)
barplot(x1$values)
x1$vectors%*%diag(x1$values)%*%t(x1$vectors)-x0
kmax <- 35
x3 <- x1$vectors[,1:kmax]%*%diag(x1$values[1:kmax])%*%t(x1$vectors[,1:kmax])
x4 <- x0-x3
image(x3)
plot(x3[,10],x0[,10])


image(x1$vectors[,1:kmax]%*%diag(x1$values[1:kmax])%*%t(x1$vectors[,1:kmax]))
?summary.lm
diag(sqrt(x0*summary(x12)$sigma^2))-summary(x12)$coeff[,2]

x2 <- x0*summary(x12)$sigma^2
image(x2)
image(cov2cor(x2))
barplot(eigen(cov2cor(x2))$values)

x12

x5 <- (x0*summary(x12)$sigma^2)
sqrt(sum(x5))/1e-5
sqrt(sum(x5[40:46,40:46]))/1e-5
sqrt(sum(x5[35:46,35:46]))/1e-5

x7 <- res$rsi[nx==max(nx),as.numeric(diff(c(res$da0,date)))]
imax <- 46
x6 <- 1:imax
for(i in 1:imax){
  x6[i] <- sqrt(sum(x5[i:imax,i:imax]))*x7[i]
}
#this is s.e. x[i:ibar] 3% +/-1% but lower for shorter
#store this and use in the graphic
#a bit compute heavy - ask how to do it fast
summary(x6)

barplot(x6)
print(x6)
plot(x6)



summary(x12)

