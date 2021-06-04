trs<-c(10, 30, 50, 100)
TiemRetorno<-data.frame()
for (jj in 1:4) {
  fx<-(1-(1/trs[jj]))
  tr_tem<-NULL
  for (ii in 1:18) {
    tr<- -log(-log(fx))*α[ii]+µ[ii]
    tr_tem<-c(tr_tem, tr)
  }
  TiemRetorno[1:18, jj]<-tr_tem
}

names(TiemRetorno)<-c("TR=10", "TR=30", "TR=50", "TR=100")
View(TiemRetorno)

write.csv(TiemRetorno, "Excel/Tiempo_Retorno.csv")


#export in graph 
Subcuenca<-c(colnames(Enero199[2:19]))
plot_colors<-c("green","#FFCC00", "red","blue")
max_y<-max(TiemRetorno)
plot(TiemRetorno$`TR=10`,type = "o",  ylim=c(1,max_y), axes=FALSE, ann=FALSE, col=plot_colors[1])
lines(TiemRetorno$`TR=30`, type = "o", col=plot_colors[2])
lines(TiemRetorno$`TR=50`, type = "o", col=plot_colors[3])
lines(TiemRetorno$`TR=100`, type = "o", col=plot_colors[4])
axis(1, at=1:18, lab=Subcuenca)
axis(2, las=1, at=4*0:max_y)

# Create a title with a red, bold/italic font
title(main="Tiempo de Retorno para las Subcuencas de la cuenca de rio Piura", font.main=4)

# Label the x and y axes with dark green text
title(xlab= "Centroide de las Subcuencas")
title(ylab= "Precipitaciones Maximas (mm)")

#Leyenda
legend("topleft", max_y, names(TiemRetorno), cex=0.6, col=plot_colors,
       pch=21:23, lwd=1:3)
