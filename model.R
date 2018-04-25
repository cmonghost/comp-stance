source("setup.R")
# multivariate analysis


model <- glmer(dep.var ~
			   affect +
			   hierarchy +
			   matrix.verb +
			   alignment +
			   event.type.generic +
			   intervening.verbal +
			   intervening.elsewhere +
			   investment *
			   matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(model)

# Bidirectional effect of investment:
# When Subj = I, investment means MORE overt (see ctree.R)
# When Subj = X, investment means LESS overt


levels(df$dep.var)

stance <- glmer(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy +
			   event.type.generic +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(stance)

