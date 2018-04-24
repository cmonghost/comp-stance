source("setup.R")
# multivariate analysis
model <- glmer(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy +
			   event.type.generic +
			   intervening.verbal +
			   intervening.elsewhere +
			   matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(model)
