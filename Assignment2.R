#Question 1
# Set parameters
s <- 0.3  # Savings rate
alpha <- 1 # Capital share of output
delta <- 0.1   # Capital depreciation rate
g <- 0.1   # productivity growth
n <- 0.01   # population growth

k <- seq(0, 1000, by = 1)
Investment <- s*k^alpha
Depreciation <- (delta+g+n)*k

plot(k, Investment, type = "l", col = "blue", 
     xlab = "Capital per worker (k_t)", ylab = "Output per worker (y_t)",
     main = "Solow Swan Model")
lines(k, Depreciation, col = "red", lty = 2)

legend("topleft", legend = c("effective investment sf(k_t)", "effective depreciation (d+g+n)(k_t)"),
       col = c("blue", "red"), lty = 1, cex = 0.8)





s <- 0.3  # Savings rate
alpha <- 1 # Capital share of output
delta <- 0.1   # Capital depreciation rate
g <- 0.1   # productivity growth
n <- 0.1   # population growth

k <- seq(0, 1000, by = 1)
Investment <- s*k^alpha
Depreciation <- (delta+g+n)*k

plot(k, Investment, type = "l", col = "blue", 
     xlab = "Capital per worker (k_t)", ylab = "Output per worker (y_t)",
     main = "Solow Swan Model")
lines(k, Depreciation, col = "red", lty = 2)

legend("topleft", legend = c("effective investment sf(k_t)", "effective depreciation (d+g+n)(k_t)"),
       col = c("blue", "red"), lty = 1, cex = 0.8)










s <- 0.3  # Savings rate
alpha <- 1 # Capital share of output
delta <- 0.1   # Capital depreciation rate
g <- 0.1   # productivity growth
n <- 0.2   # population growth

k <- seq(0, 1000, by = 1)
Investment <- s*k^alpha
Depreciation <- (delta+g+n)*k

plot(k, Investment, type = "l", col = "blue", 
     xlab = "Capital per worker (k_t)", ylab = "Output per worker (y_t)",
     main = "Solow Swan Model")
lines(k, Depreciation, col = "red", lty = 2)

legend("topleft", legend = c("effective investment sf(k_t)", " effectivedepreciation (d+g+n)(k_t)"),
       col = c("blue", "red"), lty = 1, cex = 0.8)


s <- 0.3  # Savings rate
alpha <- 0.35 # Capital share of output
delta <- 0.001   # Capital depreciation rate
g <- 0.001   # productivity growth
n <- 0.003   # population growth

k <- seq(0, 1000, by = 1)
Investment <- s*k^alpha
Depreciation <- (delta+g+n)*k

plot(k, Investment, type = "l",  ylim = c(0, 5), col = "blue", 
     xlab = "Capital per worker (k_t)", ylab = "Output per worker (y_t)",
     main = "Solow Swan Model")
lines(k, Depreciation, col = "red", lty = 2)

legend("topleft", legend = c("effective investment sf(k_t)", "effective depreciation (d+g+n)(k_t)"),
       col = c("blue", "red"), lty = 1, cex = 0.8)

# Add dotted lines to indicate intersection
intersect_k <- ((s/(delta+g+n))^(1/(1-alpha)))
abline(h = Investment[which.min(abs(k-intersect_k))], col = "black", lty = 3)
abline(v = intersect_k, col = "black", lty = 3)

# Add labels near the axes
text(intersect_k + 10, 0.1, expression(bold(k^"*")), col = "black", cex = 1.2, font = 2)
text(0, Investment[which.min(abs(k-intersect_k))] + 0.1, expression(bold(y^"*")),
     col = "black", cex = 1.2, font = 2)

# Add arrow to indicate direction of increasing capital
arrows(0, 4, 520, 4, col = "black", length = 0.1, angle = 20)
text(x = 200, y = 3.8, labels = "investment > depreciation", col = "blue", font = 1, cex = 0.8)

arrows(1000, 1.2, 580, 1.2, col = "black", length = 0.1, angle = 20)
text(x = 740, y = 1, labels = "investment < depreciation", col = "blue", font = 1, cex = 0.8)

points(intersect_k, Investment[which.min(abs(k-intersect_k))], col = "black", pch = 19)
text(intersect_k + 20, Investment[which.min(abs(k-intersect_k))] - 0.1,
     "Steady state", pos = 4, col = "purple")





#Question 2
# Load necessary packages
library(pwt10)
library(ggplot2)

# Load the dataset
data("pwt10.01")

# Filter data for Australia
rgdpo <- pwt10.01[pwt10.01$isocode == "AUS", c("rgdpo")]
pop <- pwt10.01[pwt10.01$isocode == "AUS", c("pop")]
emp <- pwt10.01[pwt10.01$isocode == "AUS", c("emp")]
avh <- pwt10.01[pwt10.01$isocode == "AUS", c("avh")]
cn <- pwt10.01[pwt10.01$isocode == "AUS", c("cn")]
rgdpph <- rgdpo/(emp*avh)
total <- data.frame()
#convert cn to billions instead of millions

cn <- cn/1000
rgdpo <- rgdpo/1000

rGDPpp <- rgdpo/pop
rgdppw <- rgdpo/emp
kpw <- cn/emp

# Set log scale on y-axis
par(pty="s")
options(scipen=999)

# Plot 1: real GDP per person
rGDPppt <- ts(rGDPpp, start=c(1950,1), end=c(2019,1), frequency = 1)
plot(rGDPppt, main = "real GDP per person (log scale)", ylab = "$'000s output per person", log="y")

# Plot 2: real GDP per worker
rgdppwt <- ts(rgdppw, start=c(1950,1), end=c(2019,1), frequency = 1)
plot(rgdppwt, main = "real GDP per worker (log scale)", ylab = "$'000s output per worker", log="y")

# Plot 3: real GDP per hour
rgdppht <- ts(rgdpph, start=c(1950,1), end=c(2019,1), frequency = 1)
plot(rgdppht, main = "real GDP per hour (log scale)", ylab = "$ output per hour worked", log="y")

# Plot 4: capital per worker
kpwt <- ts(kpw, start=c(1950,1), end=c(2019,1), frequency = 1)
plot(kpwt, main = "Capital per worker (log scale)", ylab = "$ value of capital in '000s", log="y")

#returning to original values
cn <- cn*1000
rgdpo <- rgdpo*1000

rGDPpp <- rgdpo/pop
rgdppw <- rgdpo/emp
kpw <- cn/emp


#Creating growth datasets
rgdppwg <- diff(rgdppw)/ rgdppw[-length(rgdppw)] * 100
rgdpppg <- diff(rGDPpp)/ rGDPpp[-length(rGDPpp)] * 100
rgdpphg <- diff(rgdpph)/ rgdpph[-length(rgdpph)] * 100
kpwg <- diff(kpw)/ kpw[-length(kpw)] * 100
growth_table <- data.frame(
  timeframe = c("Total", "50s","60s", "70s","80s","90s","00s","10s"),
  GDPpw = c(mean(rgdppwg), mean(rgdppwg[1:9]), mean(rgdppwg[10:19]), 
            mean(rgdppwg[20:29]), mean(rgdppwg[30:39]), mean(rgdppwg[40:49]),
            mean(rgdppwg[50:59]), mean(rgdppwg[60:69])),
  
  GDPpp = c(mean(rgdpppg), mean(rgdpppg[1:9]), mean(rgdpppg[10:19]), 
          mean(rgdpppg[20:29]), mean(rgdpppg[30:39]), mean(rgdpppg[40:49]),
          mean(rgdpppg[50:59]), mean(rgdpppg[60:69])),
  
  GDPph = c(mean(rgdpphg), mean(rgdpphg[1:9]), mean(rgdpphg[10:19]), 
           mean(rgdpphg[20:29]), mean(rgdpphg[30:39]), mean(rgdpphg[40:49]),
           mean(rgdpphg[50:59]), mean(rgdpphg[60:69])),
  
  kpw = c(mean(kpwg), mean(kpwg[1:9]), mean(kpwg[10:19]), 
          mean(kpwg[20:29]), mean(kpwg[30:39]), mean(kpwg[40:49]),
          mean(kpwg[50:59]), mean(kpwg[60:69]))
)

print(growth_table)






