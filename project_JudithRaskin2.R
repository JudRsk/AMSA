library(dplyr)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(lattice)
library(MASS)
library(rpart)
library(rpart.plot)

canada <- read.csv("C:/Users/raski/Downloads/Canada.csv",header=T)
attach(canada)
dimnames(canada)
str(canada)
canada <- na.omit(canada)

canada_c <- canada[,c(-1,-2,-3,-4,-5)]
round(cor(canada_c), digits = 2)
res <- rcorr(as.matrix(canada_c)) 
round(res$P, 2)
corrplot(res$r, method = "color", type = "upper", addCoef.col = "black",  tl.col = "black",  number.cex = 0.5, pch.cex = 0.8, tl.cex = 0.8,
         p.mat = res$P, sig.level = 0.05, diag = F)

##########YEAR###############
canada_y <- canada[,c(-1,-3,-4,-5)]
summary(canada_y)

canada_year <- canada_y %>% group_by(Year) %>% summarise(across(everything(), sum)) %>% as.data.frame()

#scaling to std=1 and create a new dataset
X.n<-scale(canada_year[,-1],center=F,scale=T)
canada_year.c <- cbind(canada_year[1], X.n)
attach(canada_year.c)

data_ggp <- data.frame(x = canada_year.c$Year,                            
                       y = c(canada_year.c$TPM, canada_year.c$PM10, canada_year.c$PM2.5, canada_year.c$SOX, canada_year.c$NOX, canada_year.c$VOC, canada_year.c$CO,
                             canada_year.c$NH3, canada_year.c$Pb, canada_year.c$Cd,canada_year.c$Hg, canada_year.c$D.F, canada_year.c$HCB, canada_year.c$PAH),
                       group = c(rep("TPM", nrow(canada_year.c)),
                                 rep("PM10", nrow(canada_year.c)),
                                 rep("PM2.5", nrow(canada_year.c)),
                                 rep("SOX", nrow(canada_year.c)),
                                 rep("NOX", nrow(canada_year.c)),
                                 rep("VOC", nrow(canada_year.c)),
                                 rep("CO", nrow(canada_year.c)),
                                 rep("NH3", nrow(canada_year.c)),
                                 rep("Pb", nrow(canada_year.c)),
                                 rep("Cd", nrow(canada_year.c)),
                                 rep("HG", nrow(canada_year.c)),
                                 rep("D.F", nrow(canada_year.c)),
                                 rep("HB", nrow(canada_year.c)),
                                 rep("PAH", nrow(canada_year.c))))

ggp <- ggplot(data_ggp, aes(x, y, col = group)) +             
  geom_line()
ggp

canada_D <- canada[,c(-1,-2,-4,-5)]
canada_D$Source<- as.factor(canada_D$Source)
summary(canada_D)
#scaling to std=1 and centering
X.c<-scale(canada_D[,-1],center=T,scale=T)

canada_DA <- cbind(canada_D[1], X.c)
attach(canada_DA)
canada.lda<-lda(Source~TPM+ PM10+ PM2.5+ SOX+ NOX+ VOC+ CO+ NH3+ Pb + Cd+ Hg+ D.F+HCB + PAH)
canada.lda
canada.pred<-predict(canada.lda)
table(canada_DA$Source,canada.pred$class,dnn=c("From","Classified into"))

canada.ld<-predict(canada.lda,dim=2)$x
eqscplot(canada.ld,type="n")
text(canada.ld,col=as.numeric(canada.pred$class),labels=as.numeric(canada_DA$Source))
title("LDA classification in space of canonical functions")

canada_T <- canada[,c(-1,-2,-4,-5)]
canada_T$Source<- as.factor(canada_T$Source)
canada_T$Source<- as.numeric(canada_T$Source)
attach(canada_T)

# Fit the model
control <- rpart.control(minsplit=10,maxcompete=5,maxsurrogate=5,usesurrogate=2,xval=20,maxdepth=30)
canada_tree <- rpart(Source~ + TPM+ PM10+ PM2.5+ SOX+ NOX+ VOC+ CO+ NH3+ Pb + Cd+ Hg+ D.F+HCB + PAH ,method="class", control = control )
printcp(canada_tree)
plotcp(canada_tree)
#Plot model
rpart.plot(canada_tree, type = 5, main = "TBM for Source")

# Pruning the tree at cost-complexity parameter cp = 0.01
canada_tree_p<-prune(canada_tree,cp=0.01) 
summary(canada_tree_p)
canada_tree_p
rpart.plot(canada_tree_p, type = 5, main = "TBM pruned at cp = 0.01")
