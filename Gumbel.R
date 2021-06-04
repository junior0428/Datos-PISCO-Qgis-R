#....x = ln[ ln[f(x)]] · + µ
#... =Sx/Sn
#...µ = x(media) (Y n )
# x(gumbel)

Enero199<-read_excel('Excel/Enereo1999.xlsx')
View(Enero199)
#values 
Yn<-0.5410
Sn<-1.1313

#Media and standard deviation
xmedia<-apply(Enero199[2:19], 2, mean)
Sx<-apply(Enero199[2:19], 2, sd) 
α<-Sx/Sn
µ<-xmedia-(Yn*α)


#order the Enero199 dataframe "Weibull"
Weibul<-apply(Enero199[2:19], 2, sort, decreasing=T)
View(Weibul)

# generate data to f(x)
f<-c(1:31)
fde<-NULL
for (u in 1:31) {
  ff<-f[u]/32
  fde<-c(fde, ff)
}
fdex<-sort(fde, decreasing = T)

#...generate gumbel to each subbasin
Gumbe<-data.frame() 
for (i in 1:18) {
  Gumbel_tem<-NULL
  for (j in 1:31) {
    gum<- -log(-log(fdex[j]))*α[i]+µ[i]
    Gumbel_tem<-c(Gumbel_tem, gum)
  }
  Gumbe[1:31, i]<-Gumbel_tem
}
View(Gumbe)

#plot of Weibull and Gumbel
nombre<-c("Weibull","Gumbel")
rm(Gumbel)
length(fdex)
length(Weibul)
plot(fdex, Weibul[,18], type="o", col="green",
     xlab="f(x)",
     ylab="Precipitaciones diarias (mm)",
     main="Distribución Weibull y Gumbel para la Subcuenca 18",lwd = 2)
lines(fdex, Gumbe[,18], col="blue", type="o",lwd = 2)
legend("topleft", Weibull, nombre, cex = 0.6,
       col = c("green", "blue"), pch = 21:23, lty=1:3, lwd = 2)