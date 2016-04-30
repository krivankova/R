#Simulace stochastickych procesu
# ==============================================================================
# ==============================================================================
# wienerův proces 1 trajektorie
dt <- 0.001
W0 <- 0
t <- seq (0, 1, by=dt)

N <- length (t)
dW <- rnorm (N - 1) * sqrt (dt)
W <- cumsum (c (W0, dW))

plot (t, W, type="l", col="red", xlab="t", ylab="W")
#abline (h = W0, lty = 2) #drift
# ==============================================================================
# wienerův proces funkce
generuj.Wp <- function (t, dt, W0) {
  dW <- rnorm (length (t) - 1) * sqrt (dt)
  W <- cumsum (c (W0, dW))
}

W <- generuj.Wp (t, dt, W0)

plot (t, W, type = "l", col = "red", xlab = "t", ylab = "W")
#abline (h = W0, lty = 2)
# ==============================================================================
# wienerův proces více trajektorií
M <- sapply (1:5 , function (k) {
  generuj.Wp (t, dt, W0)
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "W(t)")#, main = "trajektorie W")
#abline (h = W0 , lty = 2)
# =============================================================================
# =============================================================================
# dvojrozměrný wienerův proces
dt <- 0.0005
W0 <- 0
t <- seq (0, 1, by=dt)

W1 <- generuj.Wp (t, dt, W0)
W2 <- generuj.Wp (t, dt, W0)
W3 <- generuj.Wp (t, dt, W0)

plot (W1, W2, type = "l", col = "black", cex.lab=0.75, cex.axis=0.75, xlab = expression(W[1](t)), ylab = expression(W[2](t)))

points3D (W1[1:100], W2[1:100], W3[1:100], col = "red", phi = 20, theta = 20, xlab = expression(W[1](t)), ylab = expression(W[2](t)), zlab = expression(W[3](t)))


# =============================================================================
# =============================================================================
# geometricky Brownuv pohyb -- verze bez pouziti cyklu

generuj.gBp <- function (t, dt, X0, r, sigma) {
  dW <- rnorm (length (t) - 1) * sqrt (dt)
  dX <- 1 + r * dt + sigma * dW   #dX/X=r*dt+sigma*dW => X_{i+1}=X_i+dX=X_i+r*dt*X_i+sigma*dW*X_i=X_i*(1+r*dt+sigma*dW)
  X <- cumprod (c (X0 , dX))
}

# =============================================================================

X0 <- 10
dt <- 0.001
r <- 0.5
sigma <- 0.2
t <- seq (0, 1, by = dt)

X <- generuj.gBp (t, dt, X0, r, sigma)

plot (t, X, type="l", col = "red", xlab = "t", ylab = "S(t)"#"geometricky Brownuv pohyb")
      
abline (h = X0 , lty = 2)

# =============================================================================
# jedna trajektorie

X0 <- 0#10
dt <- 0.001
r <- 1#0.5
sigma <- 0.1#0.2
t <- seq (0, 1, by = dt)

M <- sapply (1:100 , function (k) {
  generuj.gBp (t, dt, X0, r, sigma)
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "S(t)")#"geometricky Brownuv pohyb", col = "green")
#abline (h = X0 , lty = 2)

# =============================================================================
# vice trajektorii

X0 <- 1#10
dt <- 0.001
r <- 1#0
sigma <- 0.1#1
t <- seq (0, 1, by = dt)

M <- sapply (1:10 , function (k) {
  generuj.gBp (t, dt, X0, r, sigma)
})

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "S(t)")#"geometricky Brownuv pohyb")
#abline (h = X0 , lty = 2)

# =============================================================================
# vice trajektorii - více parametrů

X0 <- 1
dt <- 0.001
r <- c(0,0,1,1)
sigma <- c(0.1,0.5,0.1,0.5)
t <- seq (0, 1, by = dt)
M<-generuj.gBp (t, dt, X0, r[1], sigma[1])
  
for (i in 2:length (r)){        #cyklus 
  Mi<-generuj.gBp (t, dt, X0, r[i], sigma[i])
  M<-cbind(M,Mi)
}
matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "S(t)")
legend(0,max(c(M[,])), c(expression(mu==0~a~sigma==0.1),expression(mu==0~a~sigma==0.5),expression(mu==1~a~sigma==0.1),expression(mu==1~a~sigma==0.5)), col=1:length (r), lty=1,  box.lty=0)

# =============================================================================
# GWP + bezrizikov� aktivum

X0 <- 1#10
dt <- 0.001
r <- 1#0
sigma <- 0.1#1
t <- seq (0, 1, by = dt)

M <- generuj.gBp (t, dt, X0, r, sigma)
M<-cbind(exp(r*t),M)
matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "S(t)")#"geometricky Brownuv pohyb")
#abline (h = X0 , lty = 2)
legend(0,max(c(M[,])),c(expression(e^{r[f]*t}),expression(S(t))), col=1:length (M), lty=1,  box.lty=0)
# =============================================================================
# =============================================================================
# Ornstein-Uhlenbeckuv proces 
#dr=kappa*(theta-ri)*dt+sigma*dW => r_{i+1}=r_i+dr=r_i+kappa*(theta-r_i)*dt+sigma*dW=r_i*(1-kappa*dt)+kappa*theta*dt+sigma*dW

generuj.OU <- function (t, dt, r0, theta, sigma, kappa) {
  ri<-r0
  X<-r0
  dw_r<-rnorm((length (t) - 1),0,sqrt(dt))    
  for (i in 1:(length (t) - 1)){  
    dr<-kappa*(theta-ri)*dt+sigma*dw_r[i] 
    ri<-ri+dr
    X<-cbind(X,ri)
  }
}
# =============================================================================

r0<- 2
dt <- 0.001
theta <- 2
sigma <- 0.1
kappa <- 1
t <- seq (0, 1, by = dt)

X <- generuj.OU (t, dt, r0, theta, sigma, kappa)

plot (t, X, type="l", col = "red", xlab = "t", ylab = "X(t)")#"geometricky Brownuv pohyb")
 
# =============================================================================

r0<- 2
dt <- 0.001
theta <- 2
sigma <- 0.1
kappa <- 1
t <- seq (0, 1, by = dt)

ri<-r0
X<-r0
dw_r<-rnorm((length (t) - 1),0,sqrt(dt))    
for (i in 1:(length (t) - 1)){  
  dr<-kappa*(theta-ri)*dt+sigma*dw_r[i] 
  ri<-ri+dr
  X<-cbind(X,ri)
}

plot (t, X, type="l", col = "red", xlab = "t", ylab = "X(t)")#"geometricky Brownuv pohyb")
abline (h = theta , lty = 2)  

# =============================================================================
# vice trajektorii

r0<- 2
dt <- 0.001
theta <- 2
sigma <- 0.1
kappa <- 1
t <- seq (0, 1, by = dt)
M<-t(X)
for (k in 1:4){
  ri<-r0
  X<-r0
  dw_r<-rnorm((length (t) - 1),0,sqrt(dt))    
  for (i in 1:(length (t) - 1)){  
    dr<-kappa*(theta-ri)*dt+sigma*dw_r[i] 
    ri<-ri+dr
    X<-cbind(X,ri)}
M<-cbind(M,t(X))
}

matplot (t, M, type = "l", lty = 1, xlab = "t", ylab = "X(t)")#"geometricky Brownuv pohyb")
abline (h = theta , lty = 2)  
