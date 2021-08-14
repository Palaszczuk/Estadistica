#install.packages(c("readxl","dplyr")) 
library(readxl)
library(dplyr)
excel_sheets("grafica1.xlsx")

#grafico 1 experiencia 3
hoja1<-read_excel("grafica1.xlsx",sheet="grafica1")
reg<-lm(Brix~Tiempo,data=hoja1)
ordenada<-round(reg$coefficients[1],3)
pendiente<-round(reg$coefficients[2],3)
summary(reg)
dev.new()
plot(hoja1$Tiempo,
     hoja1$Brix,
     main="Solidos",
     xlab="Tiempo (min)",
     ylab="°Brix",
     xlim=c(min(hoja1$Tiempo),max(hoja1$Tiempo)),
     ylim=c(min(hoja1$Brix),max(hoja1$Brix)),
     pch=20,
     col="black",
     panel.first = grid())
recta<-pendiente*(min(hoja1$Tiempo):max(hoja1$Tiempo))+ordenada
lines(min(hoja1$Tiempo):max(hoja1$Tiempo),recta,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("°Brix =",
              pendiente ,
              "t + ",
              ordenada,
              "  R^2 ajust= ",
              round(summary(reg)$adj.r.squared,3)),
       col="blue",
       lwd=2)

#Grafico 2 experiencia 3

reg2<-lm(ART~Tiempo,data=hoja1)
ordenada2<-round(reg2$coefficients[1],3)
pendiente2<-round(reg2$coefficients[2],3)
summary(reg2)
dev.new()
plot(hoja1$Tiempo,
     hoja1$ART,
     main="Solidos",
     xlab="Tiempo (min)",
     ylab="ART (g/L)",
     xlim=c(min(hoja1$Tiempo),max(hoja1$Tiempo)),
     ylim=c(min(hoja1$ART),max(hoja1$ART)),
     pch=20,
     col="black",
     panel.first = grid())
recta2<-pendiente2*(min(hoja1$Tiempo):max(hoja1$Tiempo))+ordenada2
lines(min(hoja1$Tiempo):max(hoja1$Tiempo),recta2,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("ART (g/L) =",pendiente2 ,"t + ",ordenada2,"  R^2 ajust= ",
              round(summary(reg2)$adj.r.squared,3)),
       col="blue",
       lwd=2)
#experiencia 4
#grafico 3 experiencia 4
hoja2<-read_excel("grafica1.xlsx",sheet="grafica2")

reg3<-lm(Brix~Tiempo,data=hoja2)
ordenada3<-round(reg3$coefficients[1],3)
pendiente3<-round(reg3$coefficients[2],3)
summary(reg3)
dev.new()
plot(hoja2$Tiempo,
     hoja2$Brix,
     main="Solidos",
     xlab="Tiempo (min)",
     ylab="°Brix",
     xlim=c(min(hoja2$Tiempo),max(hoja2$Tiempo)),
     ylim=c(min(hoja2$Brix),max(hoja2$Brix)),
     pch=20,
     col="black",
     panel.first = grid())
recta3<-pendiente3*(min(hoja2$Tiempo):max(hoja2$Tiempo))+ordenada3
lines(min(hoja2$Tiempo):max(hoja2$Tiempo),recta3,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("°Brix =",
              pendiente3 ,
              "t + ",
              ordenada3,
              "  R^2 ajust= ",
              round(summary(reg)$adj.r.squared,3)),
       col="blue",
       lwd=2)

#Grafica 4: Experienca 4
reg4<-lm(ART~Tiempo,data=hoja2)
ordenada4<-round(reg4$coefficients[1],3)
pendiente4<-round(reg4$coefficients[2],3)
summary(reg4)
dev.new()
plot(hoja2$Tiempo,
     hoja2$ART,
     main="Solidos",
     xlab="Tiempo (min)",
     ylab="ART (g/L)",
     xlim=c(min(hoja2$Tiempo),max(hoja2$Tiempo)),
     ylim=c(min(hoja2$ART),max(hoja2$ART)),
     pch=20,
     col="black",
     panel.first = grid())
recta2<-pendiente4*(min(hoja2$Tiempo):max(hoja2$Tiempo))+ordenada4
lines(min(hoja2$Tiempo):max(hoja2$Tiempo),recta2,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("ART (g/L) =",pendiente4 ,"t + ",ordenada4,"  R^2 ajust= ",
              round(summary(reg4)$adj.r.squared,3)),
       col="blue",
       lwd=2)
#grafico 5 exp 3 hoja1
reg5<-lm(ART~Brix,data=hoja1)
ordenada5<-round(reg5$coefficients[1],3)
pendiente5<-round(reg5$coefficients[2],3)
summary(reg5)
dev.new()
plot(hoja1$Brix,
     hoja1$ART,
     main="Solidos",
     xlab="°Brix",
     ylab="ART (g/L)",
     xlim=c(hoja1$Brix[1],hoja1$Brix[length(hoja1$Brix)]),
     ylim=c(min(hoja1$ART),max(hoja1$ART)),
     pch=20,
     col="black",
     panel.first = grid())
recta5<-pendiente5*(hoja1$Brix[1]:hoja1$Brix[length(hoja1$Brix)])+ordenada5
lines(hoja1$Brix[1]:hoja1$Brix[length(hoja1$Brix)],recta5,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("ART (g/L) =",pendiente5 ,"°Brix + ",ordenada5,"  R^2 ajust= ",
              round(summary(reg5)$adj.r.squared,3)),
       col="blue",
       lwd=2)
#grafico 6 exp 4 hoja2
reg6<-lm(ART~Brix,data=hoja2)
ordenada6<-round(reg6$coefficients[1],3)
pendiente6<-round(reg6$coefficients[2],3)
summary(reg6)
dev.new()
plot(hoja2$Brix,
     hoja2$ART,
     main="Solidos",
     xlab="°Brix",
     ylab="ART (g/L)",
     xlim=c(min(hoja2$Brix),max(hoja2$Brix)),
     ylim=c(min(hoja2$ART),max(hoja2$ART)),
     pch=20,
     col="black",
     panel.first = grid())
recta6<-pendiente6*(min(hoja2$Brix):max(hoja2$Brix))+ordenada6
lines(min(hoja2$Brix):max(hoja2$Brix),recta6,
      type="l",
      col="blue",
      lwd=2)
legend("topleft",
       paste0("ART (g/L) =",pendiente6 ,"°Brix + ",ordenada6,"  R^2 ajust= ",
              round(summary(reg6)$adj.r.squared,3)),
       col="blue",
       lwd=2)
       #ffmfmfmfmd