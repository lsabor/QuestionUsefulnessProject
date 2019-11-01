#factor analysis number of factors:
num.factors = 4


datasets = list(read.csv("InformativenessVsDifficultyR1 - Control.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR1 - GJudge.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR2 - Control.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR2 - GJudge.csv", row.names=1))

cor.matrices = list(cor(datasets[[1]],datasets[[1]]),
                 cor(datasets[[2]],datasets[[2]]),
                 cor(datasets[[3]],datasets[[3]]),
                 cor(datasets[[4]],datasets[[4]]))

#check for independent forecasting (should be no 1's except on diag)
fc.matrices = list(cor(t(datasets[[1]]),t(datasets[[1]])),
                   cor(t(datasets[[2]]),t(datasets[[2]])),
                   cor(t(datasets[[3]]),t(datasets[[3]])),
                   cor(t(datasets[[4]]),t(datasets[[4]])))

#THIS IS THE ONLY DATASET THAT WORKS, to try others, replace [[4]] with other numbers
fan.result = factanal(covmat = cor.matrices[[4]],factors = num.factors, 
                      n.obs = nrow(cor.matrices[[4]]), rotation = "varimax")
fan.result

# fan.results = list(factanal(covmat = cor.matrices[[1]],factors = num.factors, 
#                             n.obs = nrow(cor.matrices[[1]]), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[2]],factors = num.factors, 
#                             n.obs = nrow(cor.matrices[[2]]), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[3]],factors = num.factors, 
#                             n.obs = nrow(cor.matrices[[3]]et), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[4]],factors = num.factors, 
#                             n.obs = nrow(cor.matrices[[4]]), rotation = "varimax")
# )

compiled.ctl.gjp = list(rbind(datasets[[1]],datasets[[2]]),
                        rbind(datasets[[3]],datasets[[4]]))
cor.compiled.mat = list(cor(compiled.ctl.gjp[[1]],compiled.ctl.gjp[[1]]),
                        cor(compiled.ctl.gjp[[2]],compiled.ctl.gjp[[2]]))

fan.compiled.results = list(factanal(covmat = cor.compiled.mat[[1]],factors = num.factors,
                            n.obs = nrow(cor.compiled.mat[[1]]), rotation = "varimax"),
                   factanal(covmat = cor.compiled.mat[[2]],factors = num.factors,
                            n.obs = nrow(cor.compiled.mat[[2]]), rotation = "varimax"))
fan.compiled.results[[1]]
fan.compiled.results[[2]]


p1 = apply(datasets[[1]][,1:4],1,mean)
p2 = apply(datasets[[1]][,5:8],1,mean)
p3 = apply(datasets[[1]][,9:12],1,mean)
p4 = apply(datasets[[1]][,13:16],1,mean)
r1c = data.frame(cbind(p1,p2,p3,p4))

p1 = apply(datasets[[2]][,1:4],1,mean)
p2 = apply(datasets[[2]][,5:8],1,mean)
p3 = apply(datasets[[2]][,9:12],1,mean)
p4 = apply(datasets[[2]][,13:16],1,mean)
r1g = data.frame(cbind(p1,p2,p3,p4))

p1 = apply(datasets[[3]][,1:4],1,mean)
p2 = apply(datasets[[3]][,5:8],1,mean)
p3 = apply(datasets[[3]][,9:14],1,mean)
p4 = apply(datasets[[3]][,15:20],1,mean)
r2c = data.frame(cbind(p1,p2,p3,p4))

p1 = apply(datasets[[4]][,1:4],1,mean)
p2 = apply(datasets[[4]][,5:8],1,mean)
p3 = apply(datasets[[4]][,9:14],1,mean)
p4 = apply(datasets[[4]][,15:20],1,mean)
r2g = data.frame(cbind(p1,p2,p3,p4))

  
datasets.avg.prob = list(r1c,
                         r1g,
                         r2c,
                         r2g)


