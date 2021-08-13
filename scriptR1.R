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