
G <- 6.6743e-11 ## gravitational constant: m^3 kg^-1 s^-2 
delta_t <- 3600 ## time step: s
m1 <- 5.972e24 ## mass of earth: kg
m2 <- 7.347e22 ## mass of moon: kg

r1_init <- matrix(c(0,0), ncol = 1)
r2_init <- matrix(c(3.844e8,0), ncol = 1)

v1_init <- matrix(c(-100, 50), ncol = 1)
v2_init <- matrix(c(0, 1.1e3), ncol = 1)


K <- 24*48
x1 <- numeric(K)
y1 <- numeric(K)
x2 <- numeric(K)
y2 <- numeric(K)

v1 <- v1_init
v2 <- v2_init
r1 <- r1_init
r2 <- r2_init
for (i in 1:K){
  a1 <- (-1*G / m1)* m1 * (m2 / (norm(r1 - r2, "2"))^3) * (r1 - r2)
  a2 <- (-1*G / m2)* m1 * (m2 / (norm(r2 - r1, "2"))^3) * (r2 - r1)
  
  v1 <- v1 + (delta_t * a1)
  v2 <- v2 + (delta_t * a2)
  
  r1 <- r1 + (delta_t * v1)
  r2 <- r2 + (delta_t * v2)
  
  x1[i] <- r1[1]
  y1[i] <- r1[2]
  x2[i] <- r2[1]
  y2[i] <- r2[2]
}


plot(c(x1, x2), c(y1, y2), col = c( rep("red", K) , rep("blue", K) ), xlim = c(-1e9, 1e9), 
     ylim = c(-1e9, 1e9), xlab = "X coordinate", ylab = "Y coordinate", 
     main = paste("Earth & Moon Orbit after ", K/24, "Days"))
grid(col = "gray", lty = "dotted", lwd = 1)














