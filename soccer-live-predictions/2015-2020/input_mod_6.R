
load("2015-2020/data/input.RData")
load("2015-2020/data/input_mod_1.RData")

x1 = M1_lambda[,"x"]
y1 = M1_lambda[,"y"]
x2 = M2_lambda[,"x"]
y2 = M2_lambda[,"y"]

par1 = matrix(0, nrow = nrow(M1_lambda), ncol = 4)
colnames(par1) = c("10", "20", "01", "02")
for(i in 1:nrow(M1_lambda)) {
  if(x1[i] == y1[i]) {
    # do nothing
  } else if(x1[i] - y1[i] == 1) {
    par1[i, 1] = 1
  } else if(x1[i] - y1[i] >= 2) {
    par1[i, 2] = 1
  } else if(x1[i] - y1[i] == -1) {
    par1[i, 3] = 1
  } else if(x1[i] - y1[i] <= -2) {
    par1[i, 4] = 1
  } else {
    print(paste0("Error in i = ", i))
  }
}

par2 = matrix(0, nrow = nrow(M2_lambda), ncol = 4)
colnames(par2) = c("10", "20", "01", "02")
for(i in 1:nrow(M1_lambda)) {
  if(x2[i] == y2[i]) {
    # do nothing
  } else if(x2[i] - y2[i] == 1) {
    par2[i, 1] = 1
  } else if(x2[i] - y2[i] >= 2) {
    par2[i, 2] = 1
  } else if(x2[i] - y2[i] == -1) {
    par2[i, 3] = 1
  } else if(x2[i] - y2[i] <= -2) {
    par2[i, 4] = 1
  } else {
    print(paste0("Error in i = ", i))
  }
}

load("2015-2020/data/input_mod_2.RData")

M1_lambda = cbind(M1_lambda[,-c((ncol(M1_lambda)-3):ncol(M1_lambda))],
                  par1,
                  0, 0, 0, 0,
                  M1_lambda[,c((ncol(M1_lambda)-1):ncol(M1_lambda))])

M2_lambda = cbind(M2_lambda[,-c((ncol(M2_lambda)-3):ncol(M2_lambda))],
                  par2,
                  0, 0, 0, 0,
                  M2_lambda[,c((ncol(M2_lambda)-1):ncol(M2_lambda))])

M1_mu = cbind(M1_mu[,-c((ncol(M1_mu)-3):ncol(M1_mu))],
              0, 0, 0, 0,
              par1,
              M1_mu[,c((ncol(M1_mu)-1):ncol(M1_mu))])

M2_mu = cbind(M2_mu[,-c((ncol(M2_mu)-3):ncol(M2_mu))],
              0, 0, 0, 0,
              par2,
              M2_mu[,c((ncol(M2_mu)-1):ncol(M2_mu))])

colnames(M1_lambda)[(ncol(M1_lambda)-5):(ncol(M1_lambda)-2)] = "0"
colnames(M2_lambda)[(ncol(M2_lambda)-5):(ncol(M2_lambda)-2)] = "0"

colnames(M1_mu)[(ncol(M1_mu)-9):(ncol(M1_mu)-6)] = "0"
colnames(M2_mu)[(ncol(M2_mu)-9):(ncol(M2_mu)-6)] = "0"

save.image("2015-2020/data/input_mod_6.RData")
