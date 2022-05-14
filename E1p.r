# E1.r

rm(list=ls())       # cleans workspace
getwd()             # [1] "C:/Users/Cesar/Documents"
setwd("C:/Users/Cesar/Desktop")
d0 = read.csv("E1.csv",header=T)
d1=d0[,-1]

aux = d1$AAPL
change = diff(aux)     # succesive differences
row1 = aux[-252]       # prices ignoring last row
Aret = change/row1     # returns

aux = d1$XOM
change = diff(aux)     # succesive differences
row1 = aux[-252]       # prices ignoring last row
Xret = change/row1     # returns

aux = d1$TGT
change = diff(aux)     # succesive differences
row1 = aux[-252]       # prices ignoring last row
Tret = change/row1     # returns

aux = d1$MCD
change = diff(aux)     # succesive differences
row1 = aux[-252]       # prices ignoring last row
Mret = change/row1     # returns

d2 = data.frame(Aret,Xret,Tret,Mret)

means = colMeans(d2)    # daily mean returns
#        Aret         Xret         Tret         Mret 
#0.0003471533 0.0006647325 0.0004364417 0.0004501119 

cova = cov(d2)          # cov matrix daily returns
#              Aret         Xret          Tret         Mret
#Aret  3.209768e-04 5.110389e-06 -1.747252e-06 5.563128e-06
#Xret  5.110389e-06 6.540093e-05  2.500389e-05 2.118557e-05
#Tret -1.747252e-06 2.500389e-05  1.044785e-04 1.769191e-05
#Mret  5.563128e-06 2.118557e-05  1.769191e-05 5.522153e-05 

vars = diag(cova)
sqrt(vars)
#        Aret        Xret        Tret        Mret 
# 0.017915825 0.008087084 0.010221471 0.007431119

# min variance portfolio
a = cova
b = c(1,1,1,1)
weight = solve(a,b)
weight = weight/sum(weight)
#       Aret       Xret       Tret       Mret 
# 0.09357789 0.30002696 0.17935131 0.42704384 

# portfolio's  mean and std deviation
meanfound=t(weight)%*%means    # 0.0005024175
minvar=t(weight)%*%a%*%weight
sqrt(minvar)                   # 0.005799303
#stddev is smaller than individual ones.

# tangent portfolio T
#===================================================================
rfr = 0.09/365
b = means - rfr
weight = solve(a,b)
weight = weight/sum(weight)
weight
muM=t(weight)%*%means
varM=t(weight)%*%a%*%weight
sigmaM=sqrt(varM)       # 0.006163203

# portfolio P with risk free rate
#===================================================================
# weights 
mu = 0.2/365                    # 0.0005479452
alpha = (mu - muM)/(rfr - muM)     
w1 = (1-alpha)*weight[1]            
w2 = (1-alpha)*weight[2]            
w3 = (1-alpha)*weight[3]            
w4 = (1-alpha)*weight[4]            

weight = c(alpha,w1,w2,w3,w4)    # weights portfolio P
# 0.17677298 0.02136848 0.63502513 0.02021713 0.14661627

means = c(rfr,means)             # means of assets in P
# 0.0002465753 0.0003471533 0.0006647325 0.0004364417 0.0004501119 
# first mean is daily rfr = 0.09/365 = 0.0002465753

mufound=t(weight)%*%means          # same as mu
zeros = c(0,0,0,0)
aux = cbind(zeros,cova)
zeros = c(0,0,0,0,0)
a = rbind(zeros,aux)

varP=t(weight)%*%a%*%weight
sigmaP=sqrt(varP)                  # 0.005710833    

# portfolio P
# borrow at rf rate  $ 17.67
# buy asset 1        $  2.13
# buy asset 2        $ 63.50
# buy asset 3        $  2.02
# buy asset 4        $ 14.66

# losses
#================================================
Arch = 1 + Aret             # daily relative changes
Aval = 10000000*Arch        # new portfolio value
Aloss = 10000000 - Aval     # daily losses
Aloss = Aloss/1000

Xrch = 1 + Xret             # daily relative changes
Xval = 10000000*Xrch        # new portfolio value
Xloss = 10000000 - Xval     # daily losses
Xloss = Xloss/1000

Trch = 1 + Tret             # daily relative changes
Tval = 10000000*Trch        # new portfolio value
Tloss = 10000000 - Tval     # daily losses
Tloss = Tloss/1000

Mrch = 1 + Mret             # daily relative changes
Mval = 10000000*Mrch        # new portfolio value
Mloss = 10000000 - Mval     # daily losses
Mloss = Mloss/1000

aux = 0.05*251                # [1] 12.55
aux2 = floor(aux)         

Aloss = Aloss[order(-Aloss)]
Xloss = Xloss[order(-Xloss)]
Tloss = Tloss[order(-Tloss)]
Mloss = Mloss[order(-Mloss)]

# individual VaR, CVaR

Aloss[aux2]            # [1] 264.8545
mean(Aloss[1:12])      # [1] 422.1105
Xloss[aux2]            # [1] 116.7178
mean(Xloss[1:12])      # [1] 163.7176
Tloss[aux2]            # [1] 161.7368
mean(Tloss[1:12])      # [1] 233.3834
Mloss[aux2]            # [1] 121.6362
mean(Mloss[1:12])      # [1] 166.2264

# portfolio losses
#=======================================================
Prch = 0.09357789*Aret + 0.30002696*Xret +0.17935131*Tret +0.42704384*Mret
Prch = Prch + 1
Pval = 10000000*Prch
ploss = 10000000 - Pval  # daily losses
ploss = ploss/1000
ploss = ploss[order(-ploss)]

#VaR
ploss[aux2]          # [1] 89.4039
#CVaR
mean(ploss[1:12])    #[1] 121.254


