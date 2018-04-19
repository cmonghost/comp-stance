library(lme4)
library(party)
library(lattice)

df <- read.csv("18-04-2018.csv")
summary(df)
colnames(df)

levels(df$dep.var)
df <- droplevels(df[df$dep.var != ""&df$dep.var != "/",]) # removing "" and "/"
df <- droplevels(df[df$dep.var != ""&df$dep.var != "/",]) # removing "" and "/"

# clean up stance
df <- droplevels(df[df$affect!="",])
levels(df$affect)
levels(df$affect) <- c("neg","neg","neut","neut","pos")

df <- droplevels(df[df$investment!="",])
levels(df$investment)
levels(df$investment) <- c("high","high","low","low","mid","mid","mid","mid","mid","high","low")


df <- droplevels(df[df$alignment!="",])
levels(df$alignment)
levels(df$alignment) <- c("align","align","align","neutral","disalign","disalign","align","neutral")

df <- droplevels(df[df$hierarchy!="",])
levels(df$hierarchy)
levels(df$hierarchy) <- c("same","expert","expert","expert","novice","novice","novice","same","same","same","same")

# clean up matrix verb
xtabs(~matrix.verb,df)
levels(df$matrix.verb) <- c("other","assume","be","be+adj","believe", # above 7
							"be+np","other","other","other","other",
							"other","feel","other","other","other",
							"other","other","other","guess","hear",
							"other","other","other","other","other",
							"know","other","other","make-sure","other",
							"mean","other","other","other","other",
							"other","other","other","remember","say",
							"say","other","other","other","other",
							"other","other","other","other","tell",
							"think","other","other","other","other",
							"other")

levels(df$matrix.verb) <- c("other","other","be","be+adj","other", # above 10
							"be+np","other","other","other","other",
							"other","feel","other","other","other",
							"other","other","other","guess","hear",
							"other","other","other","other","other",
							"know","other","other","make-sure","other",
							"mean","other","other","other","other",
							"other","other","other","other","say",
							"say","other","other","other","other",
							"other","other","other","other","tell",
							"think","other","other","other","other",
							"other")

df <- droplevels(df[df$matrix.verb!="feel"&
					df$matrix.verb!="guess",]
				)


# xtabs
prop.table(xtabs(~dep.var + investment,df),2)
xtabs(~matrix.verb + investment,df)

# multivariate analysis
model <- glmer(dep.var ~ affect + investment + alignment + hierarchy + event.type.generic + (1|speaker) + (1|matrix.verb), data = df, family = binomial, control = glmerControl(optCtrl=list(maxfun=10000000)))
summary(model)

# cforest - need to run this on lab computer
forest <- cforest(dep.var1 ~ event.type.generic + matrix.verb + matrix.tense + matrix.subj.simp + speaker + like + age + gender + language + region + investment + affect + alignment + hierarchy,df)
forest.varimp <- varimp(forest,conditional=TRUE)
dotplot(sort(forest.varimp),mar=c(15,4,4,2),xlab="",ylab="predictor",panel=function(x,y){
			panel.dotplot(x,y,col="darkblue",pch=16,cex=1.1)
			panel.abline(v=abs(min(forest.varimp)),col="red",lty=2,lwd=2)
			panel.abline(v=0,col="blue")
}
)
