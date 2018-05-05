source("setup.R")

interaction.df <- table(df$investment,df$dep.var,df$matrix.subj.simp,df$matrix.verb)
interaction.df <- as.data.frame(interaction.df)
interaction.df <- ddply(interaction.df,.(Var1,Var3,Var4),transform,prop=(Freq/sum(Freq)*100))
interaction.cols <- ggplot(
						  aes(x=Var1,y=prop,fill = Var2),
						  data=interaction.df) +
	geom_bar(stat="identity",position="stack") +
	theme_few() +
	theme(text=element_text(family="Linux Libertine O",size=24)) +
	labs(fill="variant",x="investment",y="") +
	#geom_text(aes(x = Var1, y = prop, ymax = prop, hjust = 0.5, label = (Freq)), family = "Linux Libertine O") +
	facet_wrap(Var3~Var4) +
	#scale_fill_few()
	scale_fill_manual(values=wes_palette(n=3,name="Chevalier1"))

png("interactioncollapsed.png",height=800,width=1000)
interaction.cols
dev.off()

xtabs(~dep.var + matrix.subj.simp + matrix.verb,df)

interaction.df <- table(df$investment,df$dep.var,df$matrix.verb)
interaction.df <- as.data.frame(interaction.df)
interaction.df <- ddply(interaction.df,.(Var1,Var3),transform,prop=(Freq/sum(Freq)*100))
interaction.cols <- ggplot(
						  aes(x=Var1,y=prop,fill = Var2),
						  data=interaction.df) +
	geom_bar(stat="identity",position="stack") +
	theme_few() +
	theme(text=element_text(family="Linux Libertine O",size=18)) +
	labs(fill="variant",x="investment",y="") +
	geom_text(aes(x = Var1, y = prop, ymax = prop, hjust = 0.5, label = (Freq)), family = "Linux Libertine O") +
	facet_wrap(~Var3) +
	#scale_fill_few()
	scale_fill_manual(values=wes_palette(n=3,name="Royal1"))
interaction.cols

interaction.df <- table(df$investment,df$dep.var,df$matrix.subj.simp)
interaction.df <- as.data.frame(interaction.df)
interaction.df <- ddply(interaction.df,.(Var1,Var3),transform,prop=(Freq/sum(Freq)*100))
interaction.cols <- ggplot(
						  aes(x=Var1,y=prop,fill = Var2),
						  data=interaction.df) +
	geom_bar(stat="identity",position="stack") +
	theme_few() +
	theme(text=element_text(family="Linux Libertine O",size=18)) +
	labs(fill="variant",x="interaction",y="") +
	geom_text(aes(x = Var1, y = prop, ymax = prop, hjust = 0.5, label = (Freq)), family = "Linux Libertine O") +
	facet_wrap(~Var3) +
	#scale_fill_few()
	scale_fill_manual(values=wes_palette(n=3,name="Royal1"))
interaction.cols
