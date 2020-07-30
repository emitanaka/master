# Based on the hedonic housing prices and the demand of clean air
require(Ecdat)
require(ggplot2)
require(lme4)
require(nullabor)

data(Hedonic)
data <- Hedonic %>% mutate(chas = ifelse(chas == 'yes', 1, 0))
str(data)
data$townid <- as.factor(data$townid)

data$tax <- data$tax/10000
summary(data)
# checking correlation
cor(data)
# standard linear model via OLS
l1 <- lm(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
           rad+tax+ptratio+blacks+lstat+townid, data = Hedonic)
summary(l1)
plot(l1)
qqnorm(resid(l1))
qqline(resid(l1))

ggplot(data, aes(crim, mv)) + geom_point() + stat_smooth(method = "lm")+ facet_wrap(~townid)

lmm1 <- lmer(mv ~ crim+zn+indus+chas+nox+rm+age+dis+
       rad+tax+ptratio+blacks+lstat+ (1|townid), data = data)
summary(lmm1)
fixef(lmm1) # fixed effect
ranef(lmm1) # random effect


##
data("sleepstudy")
str(sleepstudy)
sleepstudy$Days <- as.integer(sleepstudy$Days)
ggplot(sleepstudy, aes(x = Days, y = Reaction)) + 
  geom_point() + stat_smooth(method = "lm") + facet_wrap(~Subject)
# slope and intercept are different
fm1 <- lmer(Reaction ~ 1+Days + (1+Days|Subject), sleepstudy)
summary(fm1)

##### NULLABOR PACKAGE #####
d <- lineup(null_permute("mpg"), mtcars)
head(d)
attr(d, "pos") # position of actual data plot
ggplot(data = d, aes(x = mpg, y = wt)) + geom_point() + facet_wrap(~ .sample)

## null_dist: returns a function that given the data generates a null data set.
head(null_dist('mpg', dist = 'normal')(mtcars))
head(null_permute('mpg')(mtcars))
## The three built in methods are ‘rotate’, ‘pboot’ and ‘boot’ 
# defined by "resid_rotate", "resid_pboot" and "resid_boot" respectively. 
head(null_lm(wt~mpg, method = 'rotate')(mtcars))

require(MASS)
data(wasps)

wasps.lda <- lda(Group~., data = wasps[,-1])
wasps.la <- predict(wasps.lda,dimen = 2)$x
true <- data.frame(wasps.la, Group = wasps$Group)

wasp.sim <- data.frame(LD1 = NULL, LD2 = NULL, Group = NULL, .n = NULL)
for (i in 1:19) {
  x <- wasps
  x$Group <- sample(x$Group)
  x.lda <- lda(Group~., data = x[,-1])
  x.ld <- predict(x.lda, dimen = 2)$x
  sim <- data.frame(x.ld, Group = x$Group, .n = i)
  wasp.sim <- rbind(wasp.sim, sim)
}

pos <- sample(1:20, 1)
d <- lineup(true = true, samples = wasp.sim, pos = pos)

ggplot(d, aes(x = LD1, y = LD2, color = Group)) + 
  facet_wrap(~.sample, ncol = 5) + 
  geom_point() + theme(aspect.ratio = 1)
attr(d, "pos")


