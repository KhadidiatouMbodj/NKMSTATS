#Ndeye Khadidiatou Mbodj 

#I
statChf = function(table){ sum((table[, 1] - table[, 2])^2 + (table[, 3] - table[, 4])^2) }
chfstat = statChf(ChargaffTable)
n.rep = 10000
permstat = numeric(n.rep)
for (i in 1:n.rep){
permuted = t(apply(ChargaffTable, 1, sample))
permstat[i] = statChf(permuted)} 
pChf = sum(permstat <= chfstat)/n.rep

#length of the output
length((table[, 1] - table[, 2])^2)

#it allows to perform the test on a sample.
apply(ChargaffTable, 1, sample)

#if the integer is 1 the theory is true, if the integer is 0 the theory is false. 
sum(permstat <= chfstat)

#pChf is 0.
pChf = sum(permstat <= chfstat)/n.rep


#II
install.packages('HardyWeinberg')
library("HardyWeinberg")
data("Mourant")
pop = 192
Mourant[pop, ]
nMM = Mourant$MM[192]
nMN = Mourant$MN[192]
nNN = Mourant$NN[192]
#phat
af(c(nMM, nMN, nNN))
phat=af(c(nMM, nMN, nNN))

#expected number of each genotype
qhat=1-phat
pHW=c(MM=phat^2, MN=2*phat*qhat, NN=qhat^2)
sum(c(nMM,nNN,nMN))*pHW

#Finetti plot
HWTernaryPlot(Mourant[pop, 4:6], markerlab = Mourant$Country[pop], alpha = 0.01, curvecols = c("red", rep("purple", 4)), vbounds = FALSE)
#USA doesn't fit

#The sample values are really different to the expected counts. 



#III
#the false positive rate of the test is 0.025.
#the false negative rate of the test is 0.125.


#IV
data("PlantGrowth")
ctrl = PlantGrowth$weight[PlantGrowth$group =="ctrl"]
trt2 = PlantGrowth$weight[PlantGrowth$group =="trt2"]
t.test(ctrl, trt2)

