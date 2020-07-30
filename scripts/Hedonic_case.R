# Based on the hedonic housing prices and the demand of clean air
require(Ecdat)
data(Hedonic)

# standard linear model via OLS
l1 <- lm(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
           rad+tax+ptratio+blacks+lstat+townid, data = Hedonic)
summary(l1)
plot(l1)
qqnorm(resid(l1))
qqline(resid(l1))

require(lme4)
lmm1 <- lmer(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
       rad+tax+ptratio+blacks+lstat+ (1|townid), data = Hedonic)
summary(lmm1)
plot(lmm1)
qqnorm(resid(lmm1))
qqline(resid(lmm1))
