## all potential 3d plotting packages 
library(scatterplot3d)
library(rgl)
library(plotly)

G <- 6.6743e-11 ## gravitational constant: m^3 kg^-1 s^-2 
delta_t <- 3600 ## time step: s
m1 <- 6e24 ## mass of body 1: kg
m2 <- 6e23 ## mass of body 2: kg
m3 <- 6e24 ## mass of body 3: kg

## initial positions
r1_init <- matrix(c(0, 4e8, 0), ncol = 1)
r2_init <- matrix(c(4e8, 0, 0), ncol = 1)
r3_init <- matrix(c(0, 0, 4e8), ncol = 1)

## initial velocities
V <- 100
v1_init <- matrix(V*rnorm(3,0,1)
, ncol = 1)
v2_init <- matrix(V*rnorm(3,0,1)
, ncol = 1)
v3_init <- matrix(V*rnorm(3,0,1)
, ncol = 1)

K <- 24*25
x1 <- numeric(K)
y1 <- numeric(K)
z1 <- numeric(K)
x2 <- numeric(K)
y2 <- numeric(K)
z2 <- numeric(K)
x3 <- numeric(K)
y3 <- numeric(K)
z3 <- numeric(K)


v1 <- v1_init
v2 <- v2_init
v3 <- v3_init
r1 <- r1_init
r2 <- r2_init
r3 <- r3_init

for (i in 1:K){
  a1 <- G*(m2/(norm(r2 - r1, "2"))^3)*(r2 - r1) + G*(m3 / (norm(r3-r1, "2")^3))*(r3-r1) 
  a2 <- G*(m1/(norm(r1 - r2, "2"))^3)*(r1 - r2) + G*(m3 / (norm(r3-r2, "2")^3))*(r3-r2)
  a3 <- G*(m1/(norm(r1 - r3, "2"))^3)*(r1 - r3) + G*(m2 / (norm(r2-r3, "2")^3))*(r2-r3)
  
  v1 <- v1 + (delta_t * a1)
  v2 <- v2 + (delta_t * a2)
  v3 <- v3 + (delta_t * a3)
  
  r1 <- r1 + (delta_t * v1)
  r2 <- r2 + (delta_t * v2)
  r3 <- r3 + (delta_t * v3)
  
  x1[i] <- r1[1]
  y1[i] <- r1[2]
  z1[i] <- r1[3]
  x2[i] <- r2[1]
  y2[i] <- r2[2]
  z2[i] <- r2[3]
  x3[i] <- r3[1]
  y3[i] <- r3[2]
  z3[i] <- r3[3]
}


## interactive plot very cool | Keeping this as a standard for 3d plots
plot_ly() %>%
  add_trace(x = x1, y = y1, z = z1,
            type = "scatter3d", mode = "lines",
            line = list(color = "blue")) %>%
  add_trace(x = x2, y = y2, z = z2,
            type = "scatter3d", mode = "lines",
            line = list(color = "red")) %>%
  add_trace(x = x3, y = y3, z = z3,
            type = "scatter3d", mode = "lines",
            line = list(color = "gray")) %>%
  layout(
    scene = list(
      xaxis = list(range = c(-5e9, 5e9)),
      yaxis = list(range = c(-5e9, 5e9)),
      zaxis = list(range = c(-5e9, 5e9))
    ))




