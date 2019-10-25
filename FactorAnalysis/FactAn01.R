datasets = list(read.csv("InformativenessVsDifficultyR1 - Control.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR1 - GJudge.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR2 - Control.csv", row.names=1),
             read.csv("InformativenessVsDifficultyR2 - GJudge.csv", row.names=1))

cor.matrices = list(cor(datasets[[1]],datasets[[1]]),
                 cor(datasets[[2]],datasets[[2]]),
                 cor(datasets[[3]],datasets[[3]]),
                 cor(datasets[[4]],datasets[[4]]))

#THIS IS THE ONLY DATASET THAT WORKS, to try others, replace [[4]] with other numbers
fan.result = factanal(covmat = cor.matrices[[4]],factors = 3, 
                      n.obs = nrow(cor.matrices[[4]]), rotation = "varimax")
fan.result

# fan.results = list(factanal(covmat = cor.matrices[[1]],factors = 3, 
#                             n.obs = nrow(cor.matrices[[1]]), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[2]],factors = 3, 
#                             n.obs = nrow(cor.matrices[[2]]), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[3]],factors = 3, 
#                             n.obs = nrow(cor.matrices[[3]]et), rotation = "varimax"),
#                    factanal(covmat = cor.matrices[[4]],factors = 3, 
#                             n.obs = nrow(cor.matrices[[4]]), rotation = "varimax")
# )



