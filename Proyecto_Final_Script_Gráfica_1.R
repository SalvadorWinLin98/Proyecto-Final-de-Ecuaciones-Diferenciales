###############################################################
###                                                         ###
### UNIVERSIDAD POLIT?CNICA DEL ESTADO DE MORELOS           ###
### Ingenier?a financiera                                   ###
### Asignatura: Ecuaciones diferenciales                    ###                                   ###
### Docente: Dra. Jessica Brise?o                           ###
### jbriseno@upemor.edu.mx                                  ###
###                                                         ###
###############################################################
##################################################################
##################################################################
##Script 3D.Calcula y grafica la sol. de una EDO
##
#PROBLEMA: Una cuenta bancaria de un pensionado tiene 200,000.00 pesos ganando 
#un 5% de interpés compuesto continuamente. El pensionista utiliza la cuenta para
#pagarse así mismo una anualidad de 20,000.00 pesos.  

 
### 3. Seleccionamos directorio de trabajo 
#setwd("C:/JB_/3_UPEMOR_2023/3_Ecuaciones diferenciales/3_Actividades")

#### 0. Instala y lee las paqueter?as necesarias para la soluci?n de EDO

#install.packages("deSolve")
library(deSolve)


#### 1. Genero el modelo(dq/dt)=-k q con la funci?n #function # de R

EcModeloq <- function (tiempoq, yq, parametrosq)
                      {with(as.list(c(yq, parametrosq)),
                      {dq <- k*q-20000
                      list(dq)
                      })
                      }

#### 2. Determino valores de los par?metros del modelo

yq <- c(q=200000)              #valor inicial de q
parametrosq <- c(k = 0.05)
taños<-seq(0,15,1)
tiempoq <- taños


#### 3. Calculo el modelo de la ED con la funci?n #ode# de R

Modelotq <- ode(yq, tiempoq, EcModeloq, parametrosq)
Modelotq
Modelotq[,1] #datos calculados con el modelo de la columna t
Modelotq[,2] #datos calculados con el modelo de la columna T

par(mfrow=c(1,2)) 


#### 4. Grafico los resultados del modelo de la ED con la funci?n #ode# de R
plot(Modelotq[,1],Modelotq[,2],
xlab="Tiempo (años)", ylab="Monto (MXN)",
xlim=c(0, 14), ylim=c(0, 210000), pch=2, col="blue",
tcl=0.1,
main="Gráfica del monto en pesos", las=1, cex=1.1, axes = TRUE)

####4.1 Coloca una l?nea a los puntos
lines(Modelotq[,1],Modelotq[,2],col="BLUE",lwd=0.5)

#abline(h=0,col="black",lwd=3)
#abline(v=0,col="black",lwd=3)

###  M?todo de Euler para resolver la ecucaci?n diferencial utilizando
#    un m?todo n?merico, es decir realizar aproximaciones para calcular
#    le soluci?n de la ecuaci?n diferencial en un valor determinado

euler1 = function(f, t0, y0, h, n) {

     #Datos igualmente espaciados iniciando en x0 = a, paso h. "n" datos
     t = seq(t0, t0 + (n-1)*h, by = h) # n datos
     y = rep(NA, times=n) # n datos
     y[1]=y0
     for(i in 2:n ) y[i]= y[i-1]+h*f(t[i-1], y[i-1])
     print(cbind(t,y)) # print
     plot(t,y, pch=19, col="red",main="Solución por Euler") # gráfica
     }

# --- Pruebas
f = function(t,y) 0.05*y-20000

# t0 = 20; y0 = 180, paso h=10, n = 10 puntos (ti,yi)
euler1(f, 0, 200000, 1, 14)

#abline(v = 13.86, col = "purple", lwd = 2, lty = 2)
#abline(h = 0, col = "purple", lwd = 2, lty = 2)
