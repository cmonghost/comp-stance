source("setup.R")
# ctree

stance <- ctree(dep.var~
				affect +
				investment +
				alignment +
				hierarchy,
			df)
plot(stance)


ctree <- ctree(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy +
			   event.type.generic +
			   intervening.verbal +
			   intervening.elsewhere +
			   matrix.subj.simp +
			   matrix.verb,
		   #speaker,
		   df)
ctree
plot(ctree)

tree <- ctree(dep.var ~
			  affect +
			  hierarchy +
			  #matrix.verb +
			  alignment +
			  event.type.generic +
			  intervening.verbal +
			  intervening.elsewhere +
			  investment +
			  matrix.subj.simp,
		  #speaker,
		  df)
tree
plot(tree)

stree <- ctree(dep.var ~
			   affect +
			   investment +
			   alignment +
			   hierarchy
		   ,df)
plot(stree)

# show big model
# show random forest
# explanatory power taken up by big grammatical factors
# but look at these interactions, so let's put the interactions in
# we actually get more predictive power

# 'that' = objective, you're scientist, you're confident
# in some contexts---no comp, more generically
# something could come up and want to show investment in particular situation

# out of all the stance variables
# investment is one of the things that complementizers do
# (completely new insight!)
# grammatical forms mark things that grammarians would never have expected them to do!
