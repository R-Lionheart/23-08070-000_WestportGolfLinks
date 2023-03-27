## Westport Links erosion model
## Regina Lionheart
## Feb 2023


# cubic equation x^3 - 6x^2 + 11x - 6 = 0
b <- c(1,-6, 11, -6)

## Values for cubic equation
R0 <- 1500
beta <- seq(15, 30, 0.5)
theta <- c(15, 90, 0.5)
C0 <- 0.071
C1 <- 1.083
C2 <- -0.355
C3 <- 0.2 # constant

## Cubic equation for shoreline equilibrium
ShorelineEq <- function()
R/R0 = C0 + C1(beta/theta) + C2(beta/theta)^2 + C3(beta/theta)^3

## Logarithmic spiral with continual similarity
ln(R) = (k * theta) + ln(R0)
