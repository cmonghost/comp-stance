source("setup.R")
library(tidyverse)
library(ggmosaic)

tense <- glmer(dep.var ~
			   matrix.verb *
			   matrix.tense +
			   (1|speaker),
			   data = df,
			   family = binomial,
			   control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000000)))

summary(tense)


m3 <- glmer(dep.var ~
			   affect +
			   alignment +
			   hierarchy +
			   investment +
			   #event.type.generic +
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

probs3 = 1/(1+exp(-fitted(m3)))
somers2(probs3,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing
#   0.8525628   0.7051256 734.0000000   0.0000000

m4 <- glmer(dep.var ~
			   affect +
			   alignment +
			   #hierarchy +
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
summary(m4)

probs4 = 1/(1+exp(-fitted(m4)))
somers2(probs4,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing
#   0.8544262   0.7088524 734.0000000   0.0000000

m5 <- glmer(dep.var ~
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
summary(m5)

probs5 = 1/(1+exp(-fitted(m5)))
somers2(probs5,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing
#   0.8577786   0.7155571 734.0000000   0.0000000

m6 <- glmer(dep.var ~
			   affect +
			   alignment +
			   investment +
			   hierarchy *
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
summary(m6)

probs6 = 1/(1+exp(-fitted(m6)))
somers2(probs6,as.numeric(df$dep.var)-1)
#           C         Dxy           n     Missing
#   0.8571574   0.7143149 734.0000000   0.0000000

xtabs(~hierarchy + event.type.generic, df)
#          event.type.generic
# hierarchy casual formal medium
#    novice     25     27     34
#    same      103     42    103
#    expert    115    158    127

round((prop.table(xtabs(~hierarchy + event.type.generic, df),2)*100),digits = 2)
#          event.type.generic
# hierarchy casual formal medium
#    novice  10.29  11.89  12.88
#    same    42.39  18.50  39.02
#    expert  47.33  69.60  48.11

df <- as.tbl(df)

hier.tbl <- df %>% count(dep.var,event.type.generic,hierarchy) %>%
	spread(dep.var, n) %>%
	mutate(total = zero + overt, prop = zero / total) %>%
	select(-overt)

hier.tbl

ggplot(hier.tbl, aes(x = hierarchy, y = prop)) +
	geom_col() +
	facet_wrap(~event.type.generic) +
	scale_colour_few() +
	theme_few() +
	scale_y_continuous(limits = c(0,1))
