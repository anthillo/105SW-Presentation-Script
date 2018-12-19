Month <- c("January","February","March","April","May","June",
           "July","August","September","October","November","December")
o15 <- c(14489821,14310456,15069465,14436777,14574324,13853701,
         14194579,13401918,15261916,14857724,13722286,12261158)
o16 <- c(13854393,14578786,13868580,14545539,14106406,14786688,
         14104114,15212744,15305727,14811890,14135918,13732632)
e16 <- c(13416131,14746508,13839207,14559989,13880713,14973023,
         14156210,15244339,15328005,14525576,14353717,14006863)
e17 <- c(13412145,16127110,15167461,15076455,15203234,14608610,
         15496872,14014742,14902690,16813581,14794402,14430747)
e18 <- c(14732409,16610465,15617830,15519966,15646749,15030010,
         15941619,14412470,15323592,17284574,15203621,14824555)

chart <- data.frame(Month,o15,o16,e16,e17,e18)
print(chart)

summary(o15)
sd(o15)
summary(o16)
sd(o16)
summary(e16)
sd(e16)
summary(e17)
sd(e17)
summary(e18)
sd(e18)

boxplot(o15,o16,e16,e17,e18, main="Registration Comparison by Year", 
        names=c("o15","o16","e16","e17","e18"), horizontal=TRUE, notch=TRUE)

plot(x=c(1:12),o15,type="o",col="blue",pch=3,lty=1,
     ylim=c(10000000,17500000),main="Registration Comparison by Year",
     xlab="Month",ylab="Registration",xaxt='n')
axis(side=1, at=c(1:12), labels=c("Jan","Feb","Mar",
                                  "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
points(x=c(1:12),o16,col="red",pch=15)
lines(x=c(1:12),o16,col="red",lty=1)
points(x=c(1:12),e16,col="green",pch=16)
lines(x=c(1:12),e16,col="green",lty=1)
points(x=c(1:12),e17,col="orange",pch=17)
lines(x=c(1:12),e17,col="orange",lty=1)
points(x=c(1:12),e18,col="purple",pch=18)
lines(x=c(1:12),e18,col="purple",lty=1)
legend("bottomleft",legend=c("o15","o16","e16","e17","e18"), 
       col=c("blue","red","green","orange","purple"), pch=c(3,15,16,17,18), lty=1)

linmod <- lm(e16~o16)
resid(linmod)
plot(resid(linmod), xlab="Month", ylab="Residuals",main="Residual Plot", pch=3, xaxt='n')
axis(side=1, at=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
abline(0,0)

plot(linmod)

summary(linmod)
confint(linmod)

