require("PoEdata")
install.packages("stargazer")
require(stargazer)
?njmin3
data("njmin3", package="PoEdata")
mod1 <- lm(fte~nj*d, data=njmin3)
mod2 <- lm(fte~nj*d+
             kfc+roys+wendys+co_owned, data=njmin3)
mod3 <- lm(fte~nj*d+
             kfc+roys+wendys+co_owned+
             southj+centralj+pa1, data=njmin3)

summary(mod1)
summary(mod2)
summary(mod3)