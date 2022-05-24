#Ajustar el modelo lineal multiple para todas las variables
library(MASS)
bd <- read.table("bd.txt")
#Asignaremos los nombres a las columnas
z <- list("ID", "LofStay", "Age", "InfRisk", "RatioOfCultures", 
          "RatioOfXrays", "Beds", "Afiliation", "Region", "#Patients", "#Nurses", "%Facilites" ) 
names(bd) <- z 

bd$ID <- NULL
bd$Afiliation <- NULL
bd$Region <- NULL

Y <- bd[,3]
Y <- as.matrix(Y)
bd <- as.matrix(bd)
bdT <- t(bd)
bdT <- as.matrix(bdT)

X2 <- bdT%*%bd
X2I <- solve(X2)
XTY <- bdT%*%Y
b <- X2I%*%XTY
H <- bd%*%X2I%*%bdT #Matriz H
J <- matrix(1, nrow = 113, ncol = 113)
SST <- t(Y)%*%(diag(113)-J/113)%*%Y #Formula Matricial de SST
SSE <- t(Y)%*%(diag(113)-H)%*%Y
SSR <- SST-SSE
MSR <- SSR/2
MSE <- SSE/3
F.est <- MSR/MSE
Rc <- qf(.95, 2, 3)
xh <- c(10, 50, 5, 30, 130, 200, 180, 150, 60)
yhg <- t(xh)%*%b
#Por lo tanto, vemos que el modelo lineal multiple queda como:
#Riesgo de infección = (2.889e-14)(x1) + (-2.775e-15)(x2) +(2.414e-15)(x3) + 
# (-3.885e-16)(x4) + (2.775e-16)(x5) + (-4.5796e-16)(x6) + (1.5265e-16)(x7) 