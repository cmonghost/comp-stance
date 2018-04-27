source("setup.R")

### STANCE ###
df$investment <- factor(df$investment, levels = c("low","mid","high"))
df$affect <- factor(df$affect, levels = c("negative","neutral","positive"))
df$hierarchy <- factor(df$hierarchy, levels = c("novice","same","expert"))
df$alignment <- factor(df$alignment, levels = c("disalign","neutral","align"))


investment.df <- table(df$investment,df$dep.var)
investment.df <- as.data.frame(investment.df)
investment.df <- ddply(investment.df,.(Var1),transform,prop=(Freq/sum(Freq)*100))

investment.df
investment.cols <- ggplot(
						  aes(x=Var1,y=prop,fill = Var2),
						  data=investment.df) +
	geom_bar(stat="identity",position="stack") +
	theme_tufte() +
	theme(text=element_text(family="Fira Sans",size=18)) +
	labs(fill="variant",x="investment",y="") +
	#geom_text(aes(x = Var1, y = prop, label = (Freq)), nudge_y = -10, family = "Fira Sans") +
	scale_fill_manual(values=wes_palette(n=3,name="Chevalier1"))
	investment.cols

affect.df <- table(df$affect,df$dep.var)
affect.df <- as.data.frame(affect.df)
affect.df <- ddply(affect.df,.(Var1),transform,prop=(Freq/sum(Freq)*100))

affect.cols <- ggplot(
					  aes(x=Var1,y=prop,fill = Var2),
					  data=affect.df) +
	geom_bar(stat="identity",position="stack") +
	theme_tufte() +
	theme(text=element_text(family="Fira Sans",size=18)) +
	labs(fill="variant",x="affect",y="") +
	#geom_text(aes(x = Var1, y = prop, label = (Freq)), nudge_y = -10, family = "Fira Sans") +
	scale_fill_manual(values=wes_palette(n=3,name="Chevalier1"))
	affect.cols

hierarchy.df <- table(df$hierarchy,df$dep.var)
hierarchy.df <- as.data.frame(hierarchy.df)
hierarchy.df <- ddply(hierarchy.df,.(Var1),transform,prop=(Freq/sum(Freq)*100))

hierarchy.cols <- ggplot(
						 aes(x=Var1,y=prop,fill = Var2),
						 data=hierarchy.df) +
	geom_bar(stat="identity",position="stack") +
	theme_tufte() +
	theme(text=element_text(family="Fira Sans",size=18)) +
	labs(fill="variant",x="hierarchy",y="") +
	#geom_text(aes(x = Var1, y = prop, label = (Freq)), nudge_y = -10, family = "Fira Sans") +
	scale_fill_manual(values=wes_palette(n=3,name="Chevalier1"))
	hierarchy.cols



alignment.df <- table(df$alignment,df$dep.var)
alignment.df <- as.data.frame(alignment.df)
alignment.df <- ddply(alignment.df,.(Var1),transform,prop=(Freq/sum(Freq)*100))

alignment.cols <- ggplot(
						 aes(x=Var1,y=prop,fill = Var2),
						 data=alignment.df) +
	geom_bar(stat="identity",position="stack") +
	theme_tufte() +
	theme(text=element_text(family="Fira Sans",size=18)) +
	labs(fill="variant",x="alignment",y="") +
	#geom_text(aes(x = Var1, y = prop, label = (Freq)), nudge_y = -10, family = "Fira Sans") +
	scale_fill_manual(values=wes_palette(n=3,name="Chevalier1"))
	alignment.cols

png("output/investment-cols.png",width=500,height=500)
investment.cols
dev.off()

png("output/affect-cols.png",width=500,height=500)
affect.cols
dev.off()

png("output/hierarchy-cols.png",width=500,height=500)
hierarchy.cols
dev.off()

png("output/alignment-cols.png",width=500,height=500)
alignment.cols
dev.off()
