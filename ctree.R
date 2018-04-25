# ctree
ctree <- ctree(dep.var ~
			   investment +
			   matrix.subj.simp,
		   df)
plot(ctree)


ctree <- ctree(dep.var ~
			   hierarchy +
			   matrix.subj.simp,
		   df)
plot(ctree)

ctree <- ctree(dep.var~
			   alignment + 
			   matrix.verb,
		   df)
plot(ctree)


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
