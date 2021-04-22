
load("2015-2019/data/input.RData")
load("2015-2019/data/input_mod_1.RData")

x1 = M1_lambda[,"x"]
y1 = M1_lambda[,"y"]
x2 = M2_lambda[,"x"]
y2 = M2_lambda[,"y"]

par1 = matrix(0, nrow = nrow(M1_lambda), ncol = 6)
colnames(par1) = c("10", "01", "11", "22", "21", "12")
for(i in 1:nrow(M1_lambda)) {
  if(x1[i] == 0 & y1[i] == 0) {
    # do nothing
  } else if(x1[i] == 1 & y1[i] == 0) {
    par1[i, 1] = 1
  } else if(x1[i] == 0 & y1[i] == 1) {
    par1[i, 2] = 1
  } else if(x1[i] == 1 & y1[i] == 1) {
    par1[i, 3] = 1
  } else if((x1[i] - y1[i] == 0) & x1[i] >= 2 & y1[i] >= 2) {
    par1[i, 4] = 1
  } else if((x1[i] - y1[i] >= 1) & x1[i] >= 2) {
    par1[i, 5] = 1
  } else if((x1[i] - y1[i] <= -1) & y1[i] >= 2) {
    par1[i, 6] = 1
  } else {
    print(paste0("Error in i = ", i))
  }
}

par2 = matrix(0, nrow = nrow(M2_lambda), ncol = 6)
colnames(par2) = c("10", "01", "11", "22", "21", "12")
for(i in 1:nrow(M2_lambda)) {
  if(x2[i] == 0 & y2[i] == 0) {
    # do nothing
  } else if(x2[i] == 1 & y2[i] == 0) {
    par2[i, 1] = 1
  } else if(x2[i] == 0 & y2[i] == 1) {
    par2[i, 2] = 1
  } else if(x2[i] == 1 & y2[i] == 1) {
    par2[i, 3] = 1
  } else if((x2[i] - y2[i] == 0) & x2[i] >= 2 & y2[i] >= 2) {
    par2[i, 4] = 1
  } else if((x2[i] - y2[i] >= 1) & x2[i] >= 2) {
    par2[i, 5] = 1
  } else if((x2[i] - y2[i] <= -1) & y2[i] >= 2) {
    par2[i, 6] = 1
  } else {
    print(paste0("Error in i = ", i))
  }
}

load("2015-2019/data/input_mod_2.RData")

M1_lambda = cbind(M1_lambda[,-c((ncol(M1_lambda)-3):ncol(M1_lambda))],
                  par1,
                  0, 0, 0, 0, 0, 0,
                  M1_lambda[,c((ncol(M1_lambda)-1):ncol(M1_lambda))])

M2_lambda = cbind(M2_lambda[,-c((ncol(M2_lambda)-3):ncol(M2_lambda))],
                  par2,
                  0, 0, 0, 0, 0, 0,
                  M2_lambda[,c((ncol(M2_lambda)-1):ncol(M2_lambda))])

M1_mu = cbind(M1_mu[,-c((ncol(M1_mu)-3):ncol(M1_mu))],
              0, 0, 0, 0, 0, 0,
              par1,
              M1_mu[,c((ncol(M1_mu)-1):ncol(M1_mu))])

M2_mu = cbind(M2_mu[,-c((ncol(M2_mu)-3):ncol(M2_mu))],
              0, 0, 0, 0, 0, 0,
              par2,
              M2_mu[,c((ncol(M2_mu)-1):ncol(M2_mu))])

colnames(M1_lambda)[(ncol(M1_lambda)-7):(ncol(M1_lambda)-2)] = "0"
colnames(M2_lambda)[(ncol(M2_lambda)-7):(ncol(M2_lambda)-2)] = "0"

colnames(M1_mu)[(ncol(M1_mu)-13):(ncol(M1_mu)-8)] = "0"
colnames(M2_mu)[(ncol(M2_mu)-13):(ncol(M2_mu)-8)] = "0"

save.image("2015-2019/data/input_mod_5.RData")
