
#Question 1
# Set parameters
c0 <- 100  # Autonomous consumption
c1 <- 0.5  # Marginal propensity to consume (MPC)
I <- 130   # Investment
G <- 100   # Government spending
T <- 100   # Taxes

# Create data for the AE and PAE lines
output <- seq(0, 1000, by = 10)
PAE <- c0 + c1 * (output - T) + I + G
PAE2 <- 50 + c1 * (output - T) + 50 + G
AE <- output

# Create the base R plot
plot(output, PAE, type = "l", col = "blue", ylim = c(0, 1000), xlab = "Output (Y)", ylab = "Expenditure",
     main = "Keynesian Cross")
lines(output, AE, col = "red", lty = 2)
lines(output, PAE2, col = "green", lty = 1)

# Find the equilibrium point
eq_output <- (c0 + I + G - c1 * T) / (1 - c1)
eq_expenditure <- c0 + c1 * (eq_output - T) + I + G

# Add equilibrium lines
abline(v = eq_output, lty = "dotted")
abline(h = eq_expenditure, lty = "dotted")

legend("topleft", legend = c("PAE", "AE"),
       col = c("blue", "red"), lty = 1, cex = 0.8)



# Set parameters
c0 <- 100  # Autonomous consumption
c1 <- 0.5  # Marginal propensity to consume (MPC)
I <- 150   # Investment (constant)
G <- 200   # Government spending
T <- 100   # Taxes

# Create data for the planned savings and planned investment lines
output <- seq(0, 1000, by = 10)
planned_savings <- (1-c1) * (output - T) - c0
#planned_savings1 <- (1-0.3) * (output - T) - c0
#planned_savings2 <- (1-c1) * (output - T) - 0
planned_investment <- rep(I, length(output))
planned_investment2 <- rep(250, length(output))

# Create the base R plot
plot(output, planned_savings, type = "l", col = "blue", ylim = c(0, max(planned_savings)),
     xlab = "Aggregate Output (Y)", ylab = "Savings and Investment",
     main = "Planned Savings and Planned Investment")
lines(output, planned_investment, col = "red", lty = 1)
#lines(output, planned_savings1, col = "green", lty = 1)
lines(output, planned_savings2, col = "green", lty = 1)
lines(output, planned_investment2, col = "orange", lty = 1)
abline(v = 400, lty = "dotted")
abline(v = 600, lty = "dotted")

# Add legend
legend("topleft", legend = c("Planned Savings", "Planned Investment", "Pandemic Savings", "G+I expenditure"),
       col = c("blue", "red","green", "orange"), lty = 1, cex = 0.8)


library(readr)

#Question 2

Macro <- read_csv("Macro Assignment 1 - Sheet1.csv")
Macro1 <- read_csv("Macro Assignment 1 - Sheet2.csv")

RGDP2019 <- (Macro$RealGDP/518390)*100
NGDP2019 <- (Macro$NominalGDP/502743)*100

nominal <- ts(NGDP2019, start=c(2011,1), end=c(2022,4), frequency = 4)
real <- ts(RGDP2019, start=c(2011,1), end=c(2022,4), frequency = 4)


CPIA <- ts(Macro$CPIInflationA, start=c(2011,1), end=c(2022,4), frequency = 4)
CPIQ <- ts(Macro$CPIInflationQ, start=c(2011,1), end=c(2022,4), frequency = 4)

EPR <- ts(Macro1$employmentpopulation_ratio, start=c(2011,1), end=c(2022,12), frequency = 12)

plot(nominal, main = "Nominal GDP (seasonally adjusted, indexed Dec 2019)", ylab = "index points")
abline(h = 100, col = "red")
plot(real, main = "Real GDP (seasonally adjusted, indexed Dec 2019)", ylab = "index points")
abline(h = 100, col = "red")
plot(CPIA, main = "Annual Inflation Rate", ylab = "%")
plot(CPIQ, main = "Quarterly Inflation Rate", ylab = "%")



plot(EPR, main = "Employment/Population Ratio (seasonally adjusted)", ylab = "%")
