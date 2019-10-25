dataset = read.csv("InformativenessVsDifficultyR1 - Control.csv", row.names=1)
cor.matrix = cor(dataset,dataset)

fa1 = factanal(covmat = cor.matrix, factors = 3, n.obs = nrow(dataset), rotation = "varimax")
fa1