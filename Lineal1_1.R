Datos <- read.table("bd.txt")
library(MASS)

#Asignaremos los nombres a las columnas
z <- list("ID", "LofStay", "Age", "InfRisk", "RatioOfCultures", 
          "RatioOfXrays", "Beds", "Afiliation", "Region", "#Patients", "#Nurses", "%Facilites" ) 
names(Datos) <- z 

Datos$ID <- NULL
Datos$Afiliation <- NULL
Datos$Region <- NULL


alfa <- 0.05

X <- Datos[, 1] #X
Y <- Datos[, 3] #Y
Xq <- X^2 #para modelo cuadratico correr esta con Y

X_bar <- mean(X)
Y_bar <- mean(Y)

Des_x <- X-X_bar #reparte la resta
Des_y <- Y-Y_bar #tambien aqui

b1n <- sum(Des_x*Des_y) #multiplicacion celda a celda
b1d <- sum(Des_x^2)

b1 <- b1n/b1d

b0 <- Y_bar-b1*X_bar

Y_hat <- b0+b1*X #evaluando X en la recta de regresion obtenida

Errores <- Y-Y_hat

SSE <- sum(Errores^2)

MSE <- SSE/58  #Estimador de varianza

#Prueba para B1#
#H0: B1 = 0
#H1: B1=/=0

sb1 <- MSE/b1d #var muestral de b1

t1 <- b1/sqrt(sb1) #estadistico de prueba para b1

r1 <- qt(1-alfa/2,58) #region critica de para b1

#Como el valor absoluto de t1 excede a r1, se rechaza la hipotesis nula

#Obtener intervalo de confianza para b1

LI <- b1- sqrt(sb1)*qt(1-alfa/2, 58) #por default toma cola izquierda
LS <- b1+ sqrt(sb1)*qt(1-alfa/2, 58)

# b1 puede valer -1.3? SI, ya que es elemento del intervalo de confianza

#Prueba para b0
#H0: B0  = 100
#H1: B0 =/= 100
B0 <- 100

sb0 <- MSE*(1/60 + X_bar^2/b1d)

t0 <- (b0-B0)/sqrt(sb0)

r0 <- qt(alfa/2, 58, lower.tail = FALSE) #asume cola derecha

# Como t0 excede a r0, se rechaza H0 

#Intervalo de confianza

LI_0 <- b0- sqrt(sb0)*qt(1-alfa/2, 58) #por default toma cola izquierda
LS_0 <- b0+ sqrt(sb0)*qt(1-alfa/2, 58)

#Predecir valores

xh <- 58 #Valor no existente en los datos iniciales

yh_gor <- b0+b1*xh

#intervalo de confianza

sbh <- MSE*(1/60 +(xh-X_bar)^2/b1d)

LI_h <- yh_gor-sqrt(sbh)*qt(alfa/2,58,lower.tail = FALSE) #cola derecha
LS_h <- yh_gor+sqrt(sbh)*qt(alfa/2,58,lower.tail = FALSE)

#Banda de confianza

LI_b <- yh_gor-sqrt(sbh)*sqrt(2*qf(1-alfa, 2, 68))#cola izquierda
LS_b <- yh_gor+sqrt(sbh)*sqrt(2*qf(1-alfa, 2, 68))

#Correlacion no parametrica

Rx <- rank(X)
Ry <- rank(Y)
mRx <- mean(Rx)
mRy <- mean(Ry)

desv_Rx <- Rx-mRx
desv_Ry <- Ry-mRy

r <- sum(desv_Rx*desv_Ry)/(sum(desv_Rx^2)*sum(desv_Ry^2))
rn <- sum(Des_x*Des_y)/(sum(Des_x^2)*sum(Des_y^2))

t_c <- rn*sqrt(58)/sqrt(1-rn^2)

#como es menor a 0.005 puedo rechazar H0 con 99.5% de confianza 

