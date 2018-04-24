# ctree
v.ctree <- ctree(dep.var ~ matrix.verb + affect + investment + alignment + hierarchy + event.type.generic, df) # with verb
plot(v.ctree)

ctree <- ctree(dep.var ~ affect + investment + alignment + hierarchy + event.type.generic, df) # without verb
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
