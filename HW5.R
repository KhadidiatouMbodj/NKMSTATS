load('~/Downloads/MorderGE(1).RData')
 morder.kmeans <- kmeans(Morder, centers=3)
 
 morder.kmeans
K-means clustering with 3 clusters of sizes 3, 6, 21

 table(morder.kmeans$cluster)

 1  2  3 
 3  6 21 

 table(type)
type
effector   memory    naive 
      10        9       11 
 data.frame(morder.kmeans$cluster, type)
            morder.kmeans.cluster     type
HEA31_NAI                       1    naive
HEA26_NAI                       1    naive
MEL36_NAI                       1    naive
MEL51_NAI                       2    naive
HEA59_NAI                       2    naive
MEL67_NAI                       2    naive
HEA55_NAI                       2    naive
MEL53_NAI                       2    naive
HEA25_NAI                       2    naive
MEL39_NAI                       3    naive
MEL51_MEM                       3   memory
HEA25_MEM                       3   memory
MEL39_MEM                       3   memory
HEA59_MEM                       3   memory
HEA26_MEM                       3   memory
HEA31_MEM                       3   memory
HEA55_MEM                       3   memory
MEL36_MEM                       3   memory
MEL53_NAI.1                     3    naive
MEL67_MEM                       3   memory
MEL39_EFF                       3 effector
HEA25_EFF                       3 effector
HEA31_EFF                       3 effector
MEL67_EFF                       3 effector
HEA59_EFF                       3 effector
HEA55_EFF                       3 effector
MEL53_EFF                       3 effector
MEL51_EFF                       3 effector
MEL36_EFF                       3 effector
HEA26_EFF                       3 effector
 sum(type == "naive" & morder.kmeans$cluster != 2)
[1] 5
 sum(type == "memory" & morder.kmeans$cluster != 1)
[1] 9
 sum(type == "effector" & morder.kmeans$cluster != 3)
[1] 0


 table(morder.kmeans$cluster, type)
   type
    effector memory naive
  1        0      0     3
  2        0      0     6
  3       10      9     2

 my.color = c("red", "blue", "green")
 plot(Morder$X3968, Morder$X14831, col = my.color[morder.kmeans$cluster], pch=19)

 #2
 
 load('~/Downloads/DNASeqs.RData')
 hamming<- function(x,y)
+ {sum(x != y)}
 load(DNASeqs.RData)
Error: object 'DNASeqs.RData' not found
 n <- nrow(dna.seq)
 D <- matrix(0, nrow=n, ncol=n)
 row.names(D) = row.names(dna.seq)
 for (i in 2:n){
+ for (j in 1:(i-1)){
+ D[i, j] <- hamming(dna.seq[i,], dna.seq[j,])
+ }
+ }
 dna.dist = as.dist(D)
 #because we do not have numeric data, it would not make sense to use eucleudian distance.
 dna.dist
      Pre1 Pme2 Pma3 Pfa4 Pbe5 Plo6 Pfr7 Pkn8 Pcy9 Pvi10
Pme2   113                                              
Pma3   116  109                                         
Pfa4   107  110  114                                    
Pbe5   101   97  111  102                               
Plo6   107  124  126  131  102                          
Pfr7    95  136  130  126   99  111                     
Pkn8   111  131  145  132  106   89   94                
Pcy9   105  141  140  113  110  111  107   92           
Pvi10   92  113  129  120  109  124  110   98   79      
Pga11  116  114  131  119  115  119  123  105   94    87
 plot(hclust(dna.dist), method='Average')
 plot(hclust(dna.dist), method='Complete')
 install.packages("ape")

 library(ape)
 plot(nj(dna.dist))
 axis(1)
 