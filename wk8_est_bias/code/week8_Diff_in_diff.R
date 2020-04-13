' Description:  
'

library(devtools)
require(stargazer)

mod1 <- lm(fte~nj*d, data=njmin3)
mod2 <- lm(fte~nj*d+
             kfc+roys+wendys+co_owned, data=njmin3)
mod3 <- lm(fte~nj*d+
             kfc+roys+wendys+co_owned+
             southj+centralj+pa1, data=njmin3)

summary(mod1)
summary(mod2)
summary(mod3)