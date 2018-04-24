# cforest - need to run this on lab computer
forest <- cforest(dep.var1 ~ event.type.generic + matrix.verb + matrix.tense + matrix.subj.simp + speaker + like + age + gender + language + region + investment + affect + alignment + hierarchy + verbal_arguments + other_intervening_elements,df)
forest.varimp <- varimp(forest,conditional=FALSE)
dotplot(sort(forest.varimp),mar=c(15,4,4,2),xlab="",ylab="predictor",panel=function(x,y){
			panel.dotplot(x,y,col="darkblue",pch=16,cex=1.1)
			panel.abline(v=abs(min(forest.varimp)),col="red",lty=2,lwd=2)
			panel.abline(v=0,col="blue")
}
)
