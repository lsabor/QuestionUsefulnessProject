###############################################
#
#
#get data by rounds
#make sure data is in format:
#col 1: forecaster/team
#col 2-n labels: question numbers as you want them to appear on graph
#data = WRPS scores
#p_starts are the columns where each successive problem starts in data

gjudgedataR1 <- read.csv("InformativenessVsDifficultyR1 - GJudge.csv", row.names=1)
controldataR1 <- read.csv("InformativenessVsDifficultyR1 - Control.csv", row.names=1)
p_startsR1 = c(1,5,9,13)

gjudgedataR2 <- read.csv("InformativenessVsDifficultyR2 - GJudge.csv", row.names=1)
controldataR2 <- read.csv("InformativenessVsDifficultyR2 - Control.csv", row.names=1)
p_startsR2 = c(1,5,9,15)

gjudgedataR1_initial <- read.csv("InformVDiff_data - Round1_GJudge_Initial.csv", row.names=1)
controldataR1_initial <- read.csv("InformativenessVsDifficultyR1 - Control.csv", row.names=1)
p_startsR1 = c(1,5,9,13)

gjudgedataR2_initial <- read.csv("InformVDiff_data - Round2_GJudge_Initial.csv", row.names=1)
controldataR2_initial <- read.csv("InformativenessVsDifficultyR2 - Control.csv", row.names=1)
p_startsR2 = c(1,5,9,15)

round1 = list("data" = list("gjudge" = gjudgedataR1,"control" = controldataR1,
    "p_starts" = c(p_startsR1,length(gjudgedataR1)+1),"Round" = "Round 1 Final",'sqrt'=''))
round2 = list("data" = list("gjudge" = gjudgedataR2,"control" = controldataR2,
    "p_starts" = c(p_startsR2,length(gjudgedataR2)+1),"Round" = "Round 2 Final",'sqrt'=''))
round1_initial = list("data" = list("gjudge" = gjudgedataR1_initial,"control" = controldataR1_initial,
    "p_starts" = c(p_startsR1,length(gjudgedataR1_initial)+1),"Round" = "Round 1 Initial",'sqrt'=''))
round2_initial = list("data" = list("gjudge" = gjudgedataR2_initial,"control" = controldataR2_initial,
    "p_starts" = c(p_startsR2,length(gjudgedataR2_initial)+1),"Round" = "Round 2 Initial",'sqrt'=''))

#
#
###############################################

#find means of each question score
get_difficulties = function(round_data){
  for (i in 1:2){
    round_data[[i]]$gjudge_difficulty = apply(round_data[[i]]$gjudge[,],2,mean)
    round_data[[i]]$control_difficulty = apply(round_data[[i]]$control[,],2,mean)
  }
  return(round_data)
}

#get means of all other questions by forecaster/team by question
get_means_by_question = function(round_data){
  for (i in 1:2){
    round_data[[i]]$gjudge_difficulty_Qx = round_data[[i]]$gjudge
    for (j in 1:length(round_data[[i]]$gjudge)){
      round_data[[i]]$gjudge_difficulty_Qx[,j] = apply(round_data[[i]]$gjudge[,-j],1,mean)
    }
    round_data[[i]]$control_difficulty_Qx = round_data[[i]]$control
    for (j in 1:length(round_data[[i]]$control)){
      round_data[[i]]$control_difficulty_Qx[,j] = apply(round_data[[i]]$control[,-j],1,mean)
    }
  }
  return(round_data)
}

#get means of all other questions by forecaster/team by problem
get_means_by_problem = function(round_data){
  for (i in 1:2){
    round_data[[i]]$p_lengths = round_data[[i]]$p_starts[-1]-round_data[[i]]$p_starts[-length(round_data[[i]]$p_starts)]
    
    round_data[[i]]$gjudge_difficulty_Px = round_data[[i]]$gjudge
    for (j in 1:length(round_data[[i]]$p_lengths)){
      round_data[[i]]$gjudge_difficulty_Px[,round_data[[i]]$p_starts[j]:(round_data[[i]]$p_starts[j]+round_data[[i]]$p_lengths[j]-1)] =
        apply(round_data[[i]]$gjudge[,-c(round_data[[i]]$p_starts[j]:(round_data[[i]]$p_starts[j]+round_data[[i]]$p_lengths[j]-1))],1,mean)
    }
    
    round_data[[i]]$control_difficulty_Px = round_data[[i]]$control
    for (j in 1:length(round_data[[i]]$p_lengths)){
      round_data[[i]]$control_difficulty_Px[,round_data[[i]]$p_starts[j]:(round_data[[i]]$p_starts[j]+round_data[[i]]$p_lengths[j]-1)] =
        apply(round_data[[i]]$control[,-c(round_data[[i]]$p_starts[j]:(round_data[[i]]$p_starts[j]+round_data[[i]]$p_lengths[j]-1))],1,mean)
    }
  }
  return(round_data)
}

#correlate scores to means of all other questions/problems
#Could be simplified more to remove repetition
get_informativeness = function(round_data){
  for (i in 1:2){
    round_data[[i]]$gjudge_informativeness_Qx = round_data[[i]]$gjudge_difficulty
    for (j in 1:length(round_data[[i]]$gjudge)){
      round_data[[i]]$gjudge_informativeness_Qx[j] = cor(round_data[[i]]$gjudge[,j],round_data[[i]]$gjudge_difficulty_Qx[,j])
    } 
    round_data[[i]]$gjudge_informativeness_Px = round_data[[i]]$gjudge_difficulty
    for (j in 1:length(round_data[[i]]$gjudge)){
      round_data[[i]]$gjudge_informativeness_Px[j] = cor(round_data[[i]]$gjudge[,j],round_data[[i]]$gjudge_difficulty_Px[,j])
    } 
    round_data[[i]]$control_informativeness_Qx = round_data[[i]]$control_difficulty
    for (j in 1:length(round_data[[i]]$control)){
      round_data[[i]]$control_informativeness_Qx[j] = cor(round_data[[i]]$control[,j],round_data[[i]]$control_difficulty_Qx[,j])
    } 
    round_data[[i]]$control_informativeness_Px = round_data[[i]]$control_difficulty
    for (j in 1:length(round_data[[i]]$control)){
      round_data[[i]]$control_informativeness_Px[j] = cor(round_data[[i]]$control[,j],round_data[[i]]$control_difficulty_Px[,j])
    }
  }
  return(round_data)
}


#does the above in order
prepare_data = function(round_data){
  #grab sqrts first here
  round_data$data_sqrt = round_data$data
  round_data$data_sqrt$sqrt = '(All WRPS square rooted)'
  round_data$data_sqrt$gjudge = sqrt(round_data$data_sqrt$gjudge)
  round_data$data_sqrt$control = sqrt(round_data$data_sqrt$control)

  round_data = get_difficulties(round_data)
  round_data = get_means_by_question(round_data)
  round_data = get_means_by_problem(round_data)
  round_data = get_informativeness(round_data)
  
  return(round_data)  
}

#creates and saves 4 graphs per run of informativeness vs difficulty
generate_graph = function(round_data){
  #repetition for Q vs P balancing
  for (i in 1:2){
    title = paste(round_data[[i]]$Round,"Informativeness by Question vs. Difficulty",round_data[[i]]$sqrt)
    png(paste(title,".png"),width = 1000,height = 700)
    plot(round_data[[i]]$gjudge_difficulty,round_data[[i]]$gjudge_informativeness_Qx,main=title,
         col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSx,Avg(WRPS~x))',
         xlim=c(0,1.25),ylim=c(-.5,1),pch=16,cex=2)
    mtext("Informativeness by Question (Round 2, Problem y, Question x)",side=3,line=0.4,at=.5,adj=0.5,cex=1)
    grid (NULL,NULL, lty = 6)
    text(round_data[[i]]$gjudge_difficulty,round_data[[i]]$gjudge_informativeness_Qx+.06,labels = colnames(round_data[[i]]$gjudge),col=2,cex=1.4)
    points(round_data[[i]]$control_difficulty,round_data[[i]]$control_informativeness_Qx,col=4,pch=16,cex=2)
    text(round_data[[i]]$control_difficulty,round_data[[i]]$control_informativeness_Qx+.06,labels = colnames(round_data[[i]]$control),col=4,cex=1.4)
    
    trendline_gjudge = lm(round_data[[i]]$gjudge_informativeness_Qx~round_data[[i]]$gjudge_difficulty)
    text(x=1.07,y=.95,labels = paste('p-value :',round(summary(trendline_gjudge)[[4]][[8]],3)),col=2)
    abline(trendline_gjudge,col=2)
    trendline_control = lm(round_data[[i]]$control_informativeness_Qx~round_data[[i]]$control_difficulty)
    text(x=1.07,y=.905,labels = paste('p-value :',round(summary(trendline_control)[[4]][[8]],3)),col=4)
    abline(trendline_control,col=4)
    
    legend(x=1,y=.9,legend=c("Good Judgement","Control"),col=c(2,4),pch=16,bty='n')
    dev.off()
    
    title = paste(round_data[[i]]$Round,"Informativeness by Problem vs. Difficulty",round_data[[i]]$sqrt)
    png(paste(title,".png"),width = 1000,height = 700)
    plot(round_data[[i]]$gjudge_difficulty,round_data[[i]]$gjudge_informativeness_Px,main=title,
         col = 2,xlab = 'Difficulty: Average(WRPS)',ylab = 'Informativeness: Cor(WRPSy,Avg(WRPS~y))',
         xlim=c(0,1.25),ylim=c(-.5,1),pch=16,cex=2)
    mtext("Informativeness by Problem (Round 2, Problem y, Question x)",side=3,line=0.4,at=.5,adj=0.5,cex=1)
    grid (NULL,NULL, lty = 6)
    text(round_data[[i]]$gjudge_difficulty,round_data[[i]]$gjudge_informativeness_Px+.06,labels = colnames(round_data[[i]]$gjudge),col=2,cex=1.4)
    points(round_data[[i]]$control_difficulty,round_data[[i]]$control_informativeness_Px,col=4,pch=16,cex=2)
    text(round_data[[i]]$control_difficulty,round_data[[i]]$control_informativeness_Px+.06,labels = colnames(round_data[[i]]$control),col=4,cex=1.4)
    
    trendline_gjudge = lm(round_data[[i]]$gjudge_informativeness_Px~round_data[[i]]$gjudge_difficulty)
    text(x=1.07,y=.95,labels = paste('p-value :',round(summary(trendline_gjudge)[[4]][[8]],3)),col=2)
    abline(trendline_gjudge,col=2)
    trendline_control = lm(round_data[[i]]$control_informativeness_Px~round_data[[i]]$control_difficulty)
    text(x=1.07,y=.905,labels = paste('p-value :',round(summary(trendline_control)[[4]][[8]],3)),col=4)
    abline(trendline_control,col=4)
    
    legend(x=1,y=.9,legend=c("Good Judgement","Control"),col=c(2,4),pch=16,bty='n')
    dev.off()
    
  }
}

create_graphs = function(round_data){
  round_data = prepare_data(round_data)
  generate_graph(round_data)
  return(round_data)
}



round1 = create_graphs(round1)
round2 = create_graphs(round2)
round1_initial = create_graphs(round1_initial)
round2_initial = create_graphs(round2_initial)



