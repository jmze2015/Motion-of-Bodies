
library(scatterplot3d)
library(rgl)
library(rgl)


G <- 6.6743e-11 ## gravitational constant: m^3 kg^-1 s^-2 
delta_t <- 3600 ## time step: s
m1 <- 5.972e24 ## mass of earth: kg
m2 <- 7.347e22 ## mass of moon: kg

r1_init <- matrix(c(0, 0, 0), ncol = 1)
r2_init <- matrix(c(3.844e8, 0, 1e8), ncol = 1)

v1_init <- matrix(c(0, 0, 5e1), ncol = 1)
v2_init <- matrix(c(0, 1.1e3, 30), ncol = 1)


K <- 24*365
x1 <- numeric(K)
y1 <- numeric(K)
z1 <- numeric(K)
x2 <- numeric(K)
y2 <- numeric(K)
z2 <- numeric(K)

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
  z1[i] <- r1[3]
  x2[i] <- r2[1]
  y2[i] <- r2[2]
  z2[i] <- r2[3]
}

## plotting using 3d scatterplot
# s3d <- scatterplot3d(c(x1, x2), c(y1, y2), c(z1, z2), xlim = c(-1e9, 1e9), color = c(rep("red", K), rep("blue", K)),
#      ylim = c(-1e9, 1e9), zlim =c(-1e9, 1e9), xlab = "X coordinate", ylab = "Y coordinate", zlab = "Z coordinate",
#      main = paste("Earth & Moon Orbit after ", K/24, "Days"))
# #grid(col = "gray", lty = "dotted", lwd = 1)
# 
# s3d
# s3d$plane3d(0, 0, 0, col = "lightgray")  # xy-plane grid


earth <- cbind(x1, y1, z1)
moon <- cbind(x2, y2, z2)

## interactive 3dplot using rgl
plot3d(earth, xlab = "X coordinate", ylab = "Y coordinate", zlab = "Z coordinate",
       xlim =c(-1e9, 1e9), ylim = c(-1e9, 1e9), zlim = c(-1e9, 1e9), col = "gray",
       type = "l", lwd = 3, main = paste("Earth & Moon Orbit after ", K/24, "Days"))
lines3d(moon, col = "blue", lwd = 2)

# optional: mark current positions
points3d(tail(earth, 1), col = "blue", size = 8)
points3d(tail(moon, 1), col = "gray", size = 6)


## animation but bad
# for (i in 1:nrow(earth)) {
#   plot3d(earth[1:i,], col = "blue", type = "l", lwd = 3,
#          xlim = c(-1e9, 1e9), ylim = c(-1e9, 1e9), zlim = c(-1e9, 1e9))
#   
#   lines3d(moon[1:i,], col = "gray", lwd = 2)
#   
#   Sys.sleep(0.01)
# }



#install.packages("plotly")
library(plotly)
## interactive plot very cool
plot_ly() %>%
  add_trace(x = x1, y = y1, z = z1,
            type = "scatter3d", mode = "lines",
            line = list(color = "blue")) %>%
  add_trace(x = x2, y = y2, z = z2,
            type = "scatter3d", mode = "lines",
            line = list(color = "gray")) %>% 
  layout(
    scene = list(
      xaxis = list(range = c(-1e9, 1e9)),
      yaxis = list(range = c(-1e9, 1e9)),
      zaxis = list(range = c(-1e9, 1e9))
    ))




