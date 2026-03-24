## three bodies in 2d

three_body_orbit_simulation <- function(mass_ratios, init_dist = 5e8, delta_t = 3600, 
                                        init_vel = 100, N_days){
  G <- 6.6743e-11 ## gravitational constant: m^3 kg^-1 s^-2 
  delta_t <- delta_t ## time step: s
  M <- 6e24 
  
  if (sum(mass_ratios) != 1){
    stop("mass ratio vector must sum to 1.0")
  }
  
  m1 <- M*mass_ratios[1] ## mass of body 1: kg
  m2 <- M*mass_ratios[2] ## mass of body 2: kg
  m3 <- M*mass_ratios[3] ## mass of body 3: kg
  
  
  R <- init_dist
  
  ## initial configuration
  r1_init <- matrix(c(R,0), ncol = 1)
  r2_init <- matrix(c(R*cos(2*pi/3), R*sin(2*pi/3)), ncol = 1)
  r3_init <- matrix(c(R*cos(4*pi/3), R*sin(4*pi/3)), ncol = 1)
  
  ## random initial velocities
  V <- init_vel
  #set.seed(123)
  v1_init <- matrix(V*rnorm(2, 0, 1), ncol = 1)
  v2_init <- matrix(V*rnorm(2, 0, 1), ncol = 1)
  v3_init <- matrix(V*rnorm(2, 0, 1), ncol = 1)
  
  K <- 24*N_days
  x1 <- numeric(K)
  y1 <- numeric(K)
  x2 <- numeric(K)
  y2 <- numeric(K)
  x3 <- numeric(K)
  y3 <- numeric(K)
  
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
    x2[i] <- r2[1]
    y2[i] <- r2[2]
    x3[i] <- r3[1]
    y3[i] <- r3[2]
  }
  
  plot(c(x1, x2, x3), c(y1, y2, y3), col = c( rep("red", K) , rep("blue", K), rep("orange", K)), xlim = c(-2e9, 2e9), 
       ylim = c(-2e9, 2e9), xlab = "X coordinate", ylab = "Y coordinate", 
       main = paste("3 Body Orbits After ", K/24, "Days"))
  grid(col = "gray", lty = "dotted", lwd = 1)
  legend("bottomright", legend = c("p1(t)", "p2(t)", "p3(t)"), col = c("red", "blue", "orange"), pch = 19)
}


three_body_orbit_simulation(mass_ratios = c(1/6, 2/6, 3/6), init_vel = 400, N_days = 50)






























