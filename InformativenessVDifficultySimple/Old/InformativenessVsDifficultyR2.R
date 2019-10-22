######
#
#Select the right data!
#Change graph title x2 to reflect round
focusdata <- read.csv("InformativenessVsDifficultyR2 - FOCUS.csv", row.names=1)
controldata <- read.csv("InformativenessVsDifficultyR2 - Control.csv", row.names=1)
#NOTE the start-column of each problem
p_starts = c(1,5,9,15)
#get sqrt (optional)
do_sqrt = T
#####




if (do_sqrt){
  focusdata = sqrt(focusdata)
  controldata = sqrt(controldata)
}

#get average of each column
focus_difficulty= apply(focusdata[,],2,mean)
control_difficulty= apply(controldata[,],2,mean)

#get averages of all other questions by forecaster/team
focus_difficulty_Qx = focusdata
for (i in 1:length(focusdata)){
  focus_difficulty_Qx[,i] = apply(focusdata[,-i],1,mean)
}
control_difficulty_Qx = controldata
for (i in 1:length(controldata)){
  control_difficulty_Qx[,i] = apply(controldata[,-i],1,mean)
}

#correlate question scores to means of all other questions
focus_informativeness_Qx = focus_difficulty
for (i in 1:length(focusdata)){
  focus_informativeness_Qx[i] = cor(focusdata[,i],focus_difficulty_Qx[,i])
}  
control_informativeness_Qx = control_difficulty
for (i in 1:length(controldata)){
  control_informativeness_Qx[i] = cor(controldata[,i],control_difficulty_Qx[,i])
}  

#produce plot of informativeness vs difficulty by question

if (do_sqrt){
  png("Round 2 Informativeness by Question vs. Difficulty (All WRPS square rooted)",width = 1000,height = 700)
  plot(focus_difficulty,focus_informativeness_Qx,main="Round 2 Informativeness by Question vs. Difficulty (All WRPS square rooted)",
     col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSx,Avg(WRPS~x))',
     xlim=c(0,1.25),ylim=c(-.5,1),pch=16)
} else {
  png("Round 2 Informativeness by Question vs. Difficulty",width = 1000,height = 700)
  plot(focus_difficulty,focus_informativeness_Qx,main="Round 2 Informativeness by Question vs. Difficulty",
     col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSx,Avg(WRPS~x))',
     xlim=c(0,1.25),ylim=c(-.5,1),pch=16)
}
mtext("Informativeness by Question (Round 2, Problem y, Question x)",side=3,line=0.4,at=.5,adj=0.5,cex=.7)
grid (NULL,NULL, lty = 6) 
text(focus_difficulty,focus_informativeness_Qx+.06,labels = colnames(focusdata),col=2,cex=1.4)
points(control_difficulty,control_informativeness_Qx,col=4,pch=16)
text(control_difficulty,control_informativeness_Qx+.06,labels = colnames(focusdata),col=4,cex=1.4)
abline(lm(focus_informativeness_Qx~focus_difficulty),col=2)
abline(lm(control_informativeness_Qx~control_difficulty),col=4)
legend(x=1,y=.9,legend=c("FOCUS","Control"),col=c(2,4),pch=16,bty='n')
dev.off()


#do the same thing but by problem instead of question
#get averages of all other problems (not questions) by forecaster/team
#Cannot get for loops to work here... grrrr
focus_difficulty_Px = focusdata
focus_difficulty_Px[,p_starts[1]:(p_starts[2]-1)] = apply(focusdata[,-(p_starts[1]:(p_starts[2]-1))],1,mean)
focus_difficulty_Px[,p_starts[2]:(p_starts[3]-1)] = apply(focusdata[,-(p_starts[2]:(p_starts[3]-1))],1,mean)
focus_difficulty_Px[,p_starts[3]:(p_starts[4]-1)] = apply(focusdata[,-(p_starts[3]:(p_starts[4]-1))],1,mean)
focus_difficulty_Px[,p_starts[4]:length(focusdata)] = apply(focusdata[,-(p_starts[4]:length(focusdata))],1,mean)

control_difficulty_Px = controldata
control_difficulty_Px[,p_starts[1]:(p_starts[2]-1)] = apply(controldata[,-(p_starts[1]:(p_starts[2]-1))],1,mean)
control_difficulty_Px[,p_starts[2]:(p_starts[3]-1)] = apply(controldata[,-(p_starts[2]:(p_starts[3]-1))],1,mean)
control_difficulty_Px[,p_starts[3]:(p_starts[4]-1)] = apply(controldata[,-(p_starts[3]:(p_starts[4]-1))],1,mean)
control_difficulty_Px[,p_starts[4]:length(controldata)] = apply(controldata[,-(p_starts[4]:length(controldata))],1,mean)


#correlate problem scores to means of all other questions
focus_informativeness_Px = focus_difficulty
for (i in 1:length(focusdata)){
  focus_informativeness_Px[i] = cor(focusdata[,i],focus_difficulty_Px[,i])
}  
control_informativeness_Px = control_difficulty
for (i in 1:length(controldata)){
  control_informativeness_Px[i] = cor(controldata[,i],control_difficulty_Px[,i])
}  

#produce plot of informativeness vs difficulty by prpblem
if (do_sqrt){
  png("Round 2 Informativeness by Problem vs. Difficulty (All WRPS square rooted)",width = 1000,height = 700)
  plot(focus_difficulty,focus_informativeness_Px,main="Round 2 Informativeness by Problem vs. Difficulty (All WRPS square rooted)",
     col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSy,Avg(WRPS~y))',
     xlim=c(0,1.25),ylim=c(-.5,1),pch=16)
} else {
  png("Round 2 Informativeness by Problem vs. Difficulty",width = 1000,height = 700)
  plot(focus_difficulty,focus_informativeness_Px,main="Round 2 Informativeness by Problem vs. Difficulty",
     col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSy,Avg(WRPS~y))',
     xlim=c(0,1.25),ylim=c(-.5,1),pch=16)
}
mtext("Informativeness by Problem (Round 2, Problem y, Question x)",side=3,line=0.4,at=.5,adj=0.5,cex=.7)
grid (NULL,NULL, lty = 6) 
text(focus_difficulty,focus_informativeness_Px+.06,labels = colnames(focusdata),col=2,cex=1.4)
points(control_difficulty,control_informativeness_Px,col=4,pch=16)
text(control_difficulty,control_informativeness_Px+.06,labels = colnames(focusdata),col=4,cex=1.4)
abline(lm(focus_informativeness_Px~focus_difficulty),col=2)
abline(lm(control_informativeness_Px~control_difficulty),col=4)
legend(x=1,y=.9,legend=c("FOCUS","Control"),col=c(2,4),pch=16,bty='n')
dev.off()