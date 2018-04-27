source("setup.R")
# multivariate analysis



levels(df$speaker)

# Bidirectional effect of investment:
# When Subj = I, investment means MORE overt (see ctree.R)
# When Subj = X, investment means LESS overt


levels(df$dep.var)


### Round 2 ###

m2 <- glmer(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy +
			   matrix.subj.simp +
			   matrix.verb +
			   investment:matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m2)

m1 <- glmer(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy +
			   matrix.subj.simp +
			   matrix.verb +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m1)

df$investment <- relevel(df$investment,"mid")
df$matrix.subj.simp <- relevel(df$matrix.subj.simp,"other")

m3 <- glmer(dep.var ~
			   affect +
			   alignment +
			   hierarchy +
			   investment +
			   event.type.generic +
			   matrix.tense +
			   intervening.verbal +
			   intervening.elsewhere +
			   matrix.verb *
			   matrix.subj.simp +
			   investment:matrix.verb +
			   investment:matrix.subj.simp +
			   investment:matrix.verb:matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m3)

m4 <- glmer(dep.var ~
			affect +
			alignment +
			hierarchy +
			investment +
			(1|speaker)
		,
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m4)

m5 <- glmer(dep.var ~
			   matrix.tense +
			   event.type.generic *
			   intervening.verbal +
			   event.type.generic *
			   intervening.elsewhere +
			   matrix.verb +
			   matrix.subj.simp +
			   investment:matrix.verb:matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m5)

m6 <- glmer(dep.var ~
			   affect +
			   alignment +
			   hierarchy +
			   investment +
			   event.type.generic +
			   matrix.tense +
			   intervening.verbal +
			   intervening.elsewhere +
			   matrix.verb +
			   matrix.subj.simp +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))
summary(m6)

probs1 = 1/(1+exp(-fitted(m1)))
somers2(probs1,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing 
#   0.8078968   0.6157937 734.0000000   0.0000000 

probs2 = 1/(1+exp(-fitted(m2)))
somers2(probs2,as.numeric(df$dep.var)-1)
#          C        Dxy          n    Missing 
#   0.813972   0.627944 734.000000   0.000000 

probs3 = 1/(1+exp(-fitted(m3)))
somers2(probs3,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing 
#   0.8577786   0.7155571 734.0000000   0.0000000 

probs4 = 1/(1+exp(-fitted(m4)))
somers2(probs4,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing 
#   0.6836456   0.3672912 734.0000000   0.0000000 

probs5 = 1/(1+exp(-fitted(m5)))
somers2(probs5,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing 
#   0.8545836   0.7091672 734.0000000   0.0000000 

probs6 = 1/(1+exp(-fitted(m6)))
somers2(probs6,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing 
#   0.8577786   0.7155571 734.0000000   0.0000000 

prop.table(xtabs(~event.type.generic + intervening.verbal,df),2)
prop.table(xtabs(~event.type.generic + intervening.elsewhere,df),2)

summary(df[df$event.type.generic=="medium",]$dep.var)
summary(df[df$event.type.generic=="formal",]$dep.var)

summary(glmer(intervening.verbal ~ event.type.generic + (1|speaker), df, family = binomial))
summary(glmer(intervening.elsewhere ~ event.type.generic + (1|speaker), df, family = binomial))
levels(df$intervening.elsewhere)

prop.table(xtabs(~matrix.verb + event.type.generic,df),2)
