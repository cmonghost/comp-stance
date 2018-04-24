library(lme4)
library(party)
library(lattice)

df <- read.csv("22-04-2018.csv")
df <- df[complete.cases(df[7]),]
df <- droplevels(df[df$dep.var != ""&
				 df$dep.var != "/"&
				 df$dep.var != "NA"&
				 df$speaker != "tomo"&
				 df$speaker != "kiranpreet",])
df$dep.var <- relevel(df$dep.var,"zero")

df <- droplevels(df[df$matrix.verb != "feel"&
				 df$matrix.verb != "guess",])

# clean up matrix subj
levels(df$matrix.subj.simp) <- c("I","I","other")

# clean up stance
df <- droplevels(df[df$affect!="",])
levels(df$affect)
#levels(df$affect) <- c("neg","neg","neut","neut","pos")

df <- droplevels(df[df$investment!="",])
levels(df$investment)
levels(df$investment) <- c("high","high","low","low","mid","mid","mid","mid","mid","high","low")
df$investment <- relevel(df$investment,"low")


df <- droplevels(df[df$alignment!="",])
levels(df$alignment)
levels(df$alignment) <- c("align","align","neutral","disalign","disalign","align","neutral")
df$alignment <- relevel(df$alignment,"disalign")

df <- droplevels(df[df$hierarchy!="",])
levels(df$hierarchy)
levels(df$hierarchy) <- c("same","expert","expert","novice","novice","novice","same","same","same")
df$hierarchy <- relevel(df$hierarchy,"novice")

# clean up matrix verb
# xtabs(~matrix.verb,df)
# levels(df$matrix.verb) <- c("other","assume","be","be+adj","believe", # above 7
#                             "be+np","other","other","other","other",
#                             "other","feel","other","other","other",
#                             "other","other","other","guess","hear",
#                             "other","other","other","other","other",
#                             "know","other","other","make-sure","other",
#                             "mean","other","other","other","other",
#                             "other","other","other","remember","say",
#                             "say","other","other","other","other",
#                             "other","other","other","other","tell",
#                             "think","other","other","other","other",
#                             "other")

# levels(df$matrix.verb) <- c("other","other","be","be+adj","other", # above 10
#                             "be+np","other","other","other","other",
#                             "other","feel","other","other","other",
#                             "other","other","other","guess","hear",
#                             "other","other","other","other","other",
#                             "know","other","other","make-sure","other",
#                             "mean","other","other","other","other",
#                             "other","other","other","other","say",
#                             "say","other","other","other","other",
#                             "other","other","other","other","tell",
#                             "think","other","other","other","other",
#                             "other")