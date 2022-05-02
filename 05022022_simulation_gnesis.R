#simulation of laboratory variables using Multivariate Gausian approach (Jiang Li on 05/02/2022)

#outline:

#1. load the 'real world' data and calculate mean and sd
#2. calculate correlation matrix for all selected laboratory variables
#3. create covariance matrix and simulate data based on mean, sd, and covariance matrix
#4. put the mask from the realworld dataframe on simulated dataframe
#5. comparing the distribution and correlation of real world and simulated data with the same mask

setwd("C:/Users/jli/Desktop/simulation")
sessionInfo()
options("install.lock"=FALSE)

save.image(file = "GOTOPATH/03162022_simulationfigures.RData")


#load the 'real world' data as an object named "labData_gs_transformed_clean"
#Outliers have been removed based on 3*IQR for each laboratory variable
dim(labData_gs_transformed_clean) #18310*49

#calculate mean for before the event
mu_ori <- data.frame(apply(labData_gs_transformed_clean_PC[labData_gs_transformed_clean_PC$TIME == 2, 2:48], 2, function(x) mean(x, na.rm = TRUE)))

mu_ori$lab <- rownames(mu_ori)
colnames(mu_ori) <- c("Mean", "lab")
head(mu_ori)

#remove some laboratory variables before simulation (optional)
mu_ori_select <- mu_ori[!(mu_ori$lab %in% c("X2965.2", "X49541.6")), ]$Mean
length(mu_ori_select) #45 laboratory variables for final simulation

mu_ori[!(mu_ori$lab %in% c("X2965.2", "X49541.6")), ]$lab
# [1] "X704.7"   "X706.2"   "X711.2"   "X713.8"   "X10466.1" "X13457.7" "X1743.4"  "X17856.6" "X17861.6" "X1975.2"  "X2028.9" 
# [12] "X2075.0"  "X2085.9"  "X2093.3"  "X2160.0"  "X2345.7"  "X2571.8"  "X2823.3"  "X2885.2"  "X2951.2"  "X3016.3"  "X30239.8"
# [23] "X3094.0"  "X32623.1" "X4544.3"  "X50560.2" "X5902.2"  "X5905.5"  "X61151.7" "X6301.6"  "X6690.2"  "X6768.6"  "X718.7"  
# [34] "X731.0"   "X736.9"   "X742.7"   "X751.8"   "X770.8"   "X777.3"   "X785.6"   "X786.4"   "X787.2"   "X788.0"   "X789.8"  
# [45] "X9830.1"
mu_ori_select  #before
# [1]  -1.4517883  -7.6785702  -0.8282933   0.3245331  10.5148238  99.8533547  21.6830126   7.0246843   9.2724550   0.5308886
# [11]  26.8344935 101.9746835  48.0499880 178.5793367   1.0086507 119.7272727 142.8963128   4.2563687   6.8735559 139.2344022
# [21]   2.2488746  24.5190415  19.2394788   9.8544984  39.0478237   6.0092272  14.2333333   8.4858110   3.9407030   1.1076563
# [31]   8.0791477  81.7555556  13.0808162   1.7255278  22.2196963   0.6808278   5.4511994  65.6869359 232.9835668  30.2943962
# [41]  33.4248756  90.5456108  13.9439517   4.3289212   3.9583574

#caculate sd for before
sd_ori <- data.frame(apply(labData_gs_transformed_clean_PC[labData_gs_transformed_clean_PC$TIME == 2, 2:48], 2, function(x) sd(x, na.rm = TRUE)))

sd_ori$lab <- rownames(sd_ori)
colnames(sd_ori) <- c("Sd", "lab")
head(sd_ori)

#remove some laboratory variables before simulation
sd_ori_select <- sd_ori[!(sd_ori$lab %in% c("X2965.2", "X49541.6")), ]$Sd
length(sd_ori_select) #45
sd_ori[!(sd_ori$lab %in% c("X2965.2", "X49541.6")), ]$lab
# [1] "X704.7"   "X706.2"   "X711.2"   "X713.8"   "X10466.1" "X13457.7" "X1743.4"  "X17856.6" "X17861.6" "X1975.2"  "X2028.9" 
# [12] "X2075.0"  "X2085.9"  "X2093.3"  "X2160.0"  "X2345.7"  "X2571.8"  "X2823.3"  "X2885.2"  "X2951.2"  "X3016.3"  "X30239.8"
# [23] "X3094.0"  "X32623.1" "X4544.3"  "X50560.2" "X5902.2"  "X5905.5"  "X61151.7" "X6301.6"  "X6690.2"  "X6768.6"  "X718.7"  
# [34] "X731.0"   "X736.9"   "X742.7"   "X751.8"   "X770.8"   "X777.3"   "X785.6"   "X786.4"   "X787.2"   "X788.0"   "X789.8"  
# [45] "X9830.1"

sd_ori_select #before
# [1]  0.2959709 10.0078910  0.3876089  0.3465580  3.2175726 38.6113792 10.8404517  1.6328577  0.5498441  0.2913735  3.0353263
# [12]  3.9253418 14.7855068 46.3558339  0.3253781 38.2392794 70.8421086  0.4813867  0.6468539  3.3184303  1.4845073  8.9207319
# [23]  8.0728707  1.3970843  5.3772271  0.7903559  1.8388183  2.8435160  0.5010587  0.1728929  2.6646662 27.6092566  1.9429289
# [34]  0.7774734  9.5652664  0.2761332  2.4238172 11.4470783 75.4180214  2.1130936  1.2385251  5.4390551  1.3071206  0.6370179
# [45]  1.3925555

#calculate correlation matrix for before
#remove c("X2965.2", "X49541.6")
Cor_before <- as.matrix(cor(labData_gs_transformed_clean_PC[labData_gs_transformed_clean_PC$TIME == 2, c(2:21, 23:27, 29:48)], use="pairwise.complete.obs"))

library(reshape2)
melted_cormat <- melt(Cor_before)
head(melted_cormat)
library(ggplot2)
tiff("labData_gs_transformed_clean_PC_before_new_cor.tiff", units="in", width=10, height=8, res=600)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()
dev.off()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

#Reordered correlation data visualization :
# Reorder the correlation matrix
Cor_before <- reorder_cormat(Cor_before)
upper_tri <- get_upper_tri(Cor_before)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
tiff("labData_gs_transformed_clean_PC_before_new_cor_half.tiff", units="in", width=10, height=8, res=600)
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
dev.off()

#create covariance matrix 

dim(Cor_before)  #45*45
V <- Cor_before
mu <- mu_ori_select
sigma <- sd_ori_select

Cor_after <- as.matrix(cor(labData_gs_transformed_clean_PC[labData_gs_transformed_clean_PC$TIME == 1, c(2:21, 23:27, 29:48)], use="pairwise.complete.obs"))

Cor_before_after

#Inputs: Create matrix of correlations and vectors of means and SD's all at once
library(JWileymisc)
library(MASS)
library(dplyr)
library(data.table)
set.seed(414)

#Create matrix of covariances (Sigma)
# a positive definite symmetric matrix
Sigma<-cor2cov(V,sigma)

Sigma
set.seed(414)
library(MASS)
dffinal_repeat_select_widformat_t0 <- data.frame(mvrnorm(n=9037,mu,Sigma,2,2))  #9037 cases
str(dffinal_repeat_select_widformat_t0)
summary(dffinal_repeat_select_widformat_t0)

# X704.7            X706.2             X711.2            X713.8            X10466.1          X13457.7         X1743.4      
# Min.   :-2.6747   Min.   :-44.7605   Min.   :-2.2922   Min.   :-0.93959   Min.   :-0.7945   Min.   :-38.55   Min.   :-23.81  
# 1st Qu.:-1.6526   1st Qu.:-14.4615   1st Qu.:-1.0864   1st Qu.: 0.09347   1st Qu.: 8.3639   1st Qu.: 73.82   1st Qu.: 14.45  
# Median :-1.4496   Median : -7.7222   Median :-0.8286   Median : 0.32190   Median :10.5307   Median : 99.73   Median : 21.52  
# Mean   :-1.4518   Mean   : -7.6786   Mean   :-0.8283   Mean   : 0.32453   Mean   :10.5148   Mean   : 99.85   Mean   : 21.68  
# 3rd Qu.:-1.2503   3rd Qu.: -0.7725   3rd Qu.:-0.5650   3rd Qu.: 0.55835   3rd Qu.:12.6422   3rd Qu.:125.66   3rd Qu.: 29.06  
# Max.   :-0.4079   Max.   : 30.9327   Max.   : 0.7968   Max.   : 1.96092   Max.   :22.7190   Max.   :252.59   Max.   : 63.34  
# X17856.6          X17861.6         X1975.2           X2028.9         X2075.0          X2085.9           X2093.3      
# Min.   : 0.8788   Min.   : 7.288   Min.   :-0.5113   Min.   :14.51   Min.   : 85.20   Min.   : -7.097   Min.   : 13.98  
# 1st Qu.: 5.9347   1st Qu.: 8.897   1st Qu.: 0.3342   1st Qu.:24.73   1st Qu.: 99.35   1st Qu.: 38.318   1st Qu.:146.87  
# Median : 7.0107   Median : 9.277   Median : 0.5337   Median :26.81   Median :102.00   Median : 48.136   Median :178.69  
# Mean   : 7.0247   Mean   : 9.272   Mean   : 0.5309   Mean   :26.83   Mean   :101.97   Mean   : 48.050   Mean   :178.58  
# 3rd Qu.: 8.1001   3rd Qu.: 9.632   3rd Qu.: 0.7287   3rd Qu.:28.88   3rd Qu.:104.66   3rd Qu.: 57.824   3rd Qu.:209.35  
# Max.   :13.5936   Max.   :11.276   Max.   : 1.6758   Max.   :37.70   Max.   :116.44   Max.   :104.815   Max.   :348.16  
# X2160.0           X2345.7          X2571.8           X2823.3         X2885.2         X2951.2         X3016.3      
# Min.   :-0.2717   Min.   :-25.99   Min.   :-131.80   Min.   :2.283   Min.   :4.544   Min.   :127.3   Min.   :-2.769  
# 1st Qu.: 0.7895   1st Qu.: 94.66   1st Qu.:  94.52   1st Qu.:3.932   1st Qu.:6.448   1st Qu.:137.0   1st Qu.: 1.251  
# Median : 1.0094   Median :119.64   Median : 141.97   Median :4.253   Median :6.879   Median :139.2   Median : 2.250  
# Mean   : 1.0087   Mean   :119.73   Mean   : 142.90   Mean   :4.256   Mean   :6.874   Mean   :139.2   Mean   : 2.249  
# 3rd Qu.: 1.2323   3rd Qu.:145.31   3rd Qu.: 191.51   3rd Qu.:4.577   3rd Qu.:7.315   3rd Qu.:141.5   3rd Qu.: 3.248  
# Max.   : 2.2400   Max.   :263.41   Max.   : 460.55   Max.   :5.993   Max.   :9.416   Max.   :152.9   Max.   : 7.711  
# X30239.8         X3094.0          X32623.1         X4544.3         X50560.2        X5902.2          X5905.5      
# Min.   :-8.719   Min.   :-9.946   Min.   : 4.009   Min.   :20.76   Min.   :3.236   Min.   : 7.213   Min.   :-3.734  
# 1st Qu.:18.448   1st Qu.:13.906   1st Qu.: 8.920   1st Qu.:35.39   1st Qu.:5.459   1st Qu.:12.983   1st Qu.: 6.575  
# Median :24.567   Median :19.270   Median : 9.848   Median :38.98   Median :6.003   Median :14.219   Median : 8.469  
# Mean   :24.519   Mean   :19.239   Mean   : 9.854   Mean   :39.05   Mean   :6.009   Mean   :14.233   Mean   : 8.486  
# 3rd Qu.:30.544   3rd Qu.:24.692   3rd Qu.:10.795   3rd Qu.:42.68   3rd Qu.:6.551   3rd Qu.:15.467   3rd Qu.:10.397  
# Max.   :57.496   Max.   :49.911   Max.   :15.022   Max.   :57.64   Max.   :9.812   Max.   :21.436   Max.   :19.241  
# X61151.7        X6301.6          X6690.2          X6768.6           X718.7           X731.0            X736.9      
# Min.   :2.023   Min.   :0.4749   Min.   :-2.065   Min.   :-30.76   Min.   : 5.307   Min.   :-0.9802   Min.   :-10.81  
# 1st Qu.:3.607   1st Qu.:0.9912   1st Qu.: 6.301   1st Qu.: 63.14   1st Qu.:11.750   1st Qu.: 1.1912   1st Qu.: 15.76  
# Median :3.944   Median :1.1080   Median : 8.091   Median : 81.97   Median :13.080   Median : 1.7178   Median : 22.24  
# Mean   :3.941   Mean   :1.1077   Mean   : 8.079   Mean   : 81.76   Mean   :13.081   Mean   : 1.7255   Mean   : 22.22  
# 3rd Qu.:4.277   3rd Qu.:1.2223   3rd Qu.: 9.861   3rd Qu.:100.36   3rd Qu.:14.409   3rd Qu.: 2.2464   3rd Qu.: 28.71  
# Max.   :5.807   Max.   :1.7993   Max.   :17.643   Max.   :185.67   Max.   :20.131   Max.   : 4.6778   Max.   : 56.19  
# X742.7            X751.8           X770.8           X777.3           X785.6          X786.4          X787.2      
# Min.   :-0.4756   Min.   :-3.414   Min.   : 17.31   Min.   :-71.62   Min.   :21.58   Min.   :28.76   Min.   : 67.80  
# 1st Qu.: 0.4966   1st Qu.: 3.830   1st Qu.: 57.81   1st Qu.:182.05   1st Qu.:28.86   1st Qu.:32.58   1st Qu.: 86.89  
# Median : 0.6823   Median : 5.458   Median : 65.65   Median :234.41   Median :30.27   Median :33.43   Median : 90.48  
# Mean   : 0.6808   Mean   : 5.451   Mean   : 65.69   Mean   :232.98   Mean   :30.29   Mean   :33.42   Mean   : 90.55  
# 3rd Qu.: 0.8664   3rd Qu.: 7.091   3rd Qu.: 73.45   3rd Qu.:283.84   3rd Qu.:31.72   3rd Qu.:34.26   3rd Qu.: 94.27  
# Max.   : 1.6584   Max.   :14.608   Max.   :106.55   Max.   :497.60   Max.   :38.72   Max.   :37.83   Max.   :113.28  
# X788.0           X789.8         X9830.1      
# Min.   : 9.271   Min.   :1.562   Min.   :-1.542  
# 1st Qu.:13.056   1st Qu.:3.900   1st Qu.: 3.028  
# Median :13.949   Median :4.328   Median : 3.959  
# Mean   :13.944   Mean   :4.329   Mean   : 3.958  
# 3rd Qu.:14.844   3rd Qu.:4.758   3rd Qu.: 4.893  
# Max.   :19.438   Max.   :6.928   Max.   : 9.132  


#put the mask from the realworld dataframe on t0 simulated dataframe
dffinal_repeat_select_widformat_t0_masked_test <- data.frame(mapply(function(x, y) ifelse(is.na(y), NA, x), dffinal_repeat_select_widformat_t0[, 2:46], labData_gs_transformed_clean_PC_before_sorted[, c(2:46)]))
dffinal_repeat_select_widformat_t0_masked_test$PT_ID <- dffinal_repeat_select_widformat_t0$PT_ID
summary(dffinal_repeat_select_widformat_t0_masked_test)
save(dffinal_repeat_select_widformat_t0_masked_test, file = "dffinal_repeat_select_widformat_t0_masked_test_1st_version2.RData", version = 2)
# X10466.1         X13457.7         X1743.4          X17856.6         X17861.6         X1975.2      
# Min.   :-1.595   Min.   :-43.51   Min.   :-13.05   Min.   : 0.077   Min.   : 7.436   Min.   :-0.687  
# 1st Qu.: 8.440   1st Qu.: 74.14   1st Qu.: 14.31   1st Qu.: 5.931   1st Qu.: 8.918   1st Qu.: 0.331  
# Median :10.526   Median : 99.70   Median : 21.44   Median : 7.023   Median : 9.290   Median : 0.524  
# Mean   :10.517   Mean   : 99.88   Mean   : 21.64   Mean   : 7.041   Mean   : 9.285   Mean   : 0.528  
# 3rd Qu.:12.600   3rd Qu.:125.76   3rd Qu.: 29.15   3rd Qu.: 8.194   3rd Qu.: 9.655   3rd Qu.: 0.722  
# Max.   :23.907   Max.   :220.84   Max.   : 60.62   Max.   :11.548   Max.   :11.363   Max.   : 1.763  
# NA's   :3674     NA's   :4983     NA's   :4204     NA's   :6424     NA's   :3261     NA's   :4423    
# X2028.9         X2075.0          X2085.9           X2093.3          X2160.0          X2345.7      
# Min.   :14.48   Min.   : 86.26   Min.   : -3.414   Min.   :-12.24   Min.   :-0.163   Min.   :-17.92  
# 1st Qu.:24.64   1st Qu.: 99.36   1st Qu.: 37.911   1st Qu.:148.34   1st Qu.: 0.795   1st Qu.: 93.45  
# Median :26.77   Median :101.99   Median : 47.911   Median :178.14   Median : 1.013   Median :119.21  
# Mean   :26.77   Mean   :102.05   Mean   : 48.020   Mean   :178.96   Mean   : 1.011   Mean   :119.30  
# 3rd Qu.:28.86   3rd Qu.:104.74   3rd Qu.: 58.248   3rd Qu.:210.47   3rd Qu.: 1.224   3rd Qu.:144.66  
# Max.   :38.24   Max.   :117.29   Max.   :103.882   Max.   :331.75   Max.   : 2.218   Max.   :279.33  
# NA's   :3262    NA's   :3270     NA's   :4880      NA's   :4846     NA's   :3471     NA's   :3504    
# X2571.8           X2823.3         X2885.2         X2951.2         X3016.3          X30239.8     
# Min.   :-110.81   Min.   :2.651   Min.   :4.164   Min.   :126.3   Min.   :-2.752   Min.   :-6.704  
# 1st Qu.:  94.84   1st Qu.:3.923   1st Qu.:6.447   1st Qu.:137.0   1st Qu.: 1.239   1st Qu.:18.477  
# Median : 141.11   Median :4.247   Median :6.880   Median :139.2   Median : 2.247   Median :24.538  
# Mean   : 143.19   Mean   :4.247   Mean   :6.879   Mean   :139.2   Mean   : 2.252   Mean   :24.550  
# 3rd Qu.: 190.41   3rd Qu.:4.558   3rd Qu.:7.315   3rd Qu.:141.5   3rd Qu.: 3.261   3rd Qu.:30.570  
# Max.   : 447.30   Max.   :6.045   Max.   :9.335   Max.   :151.6   Max.   : 7.725   Max.   :56.842  
# NA's   :4996      NA's   :3243    NA's   :4304    NA's   :3299    NA's   :5671     NA's   :4363    
# X3094.0          X32623.1         X4544.3         X50560.2        X5902.2          X5905.5      
# Min.   :-14.50   Min.   : 4.820   Min.   :18.39   Min.   :3.473   Min.   : 8.232   Min.   :-2.050  
# 1st Qu.: 13.69   1st Qu.: 8.962   1st Qu.:35.48   1st Qu.:5.467   1st Qu.:13.026   1st Qu.: 6.597  
# Median : 19.41   Median : 9.884   Median :39.14   Median :6.007   Median :14.299   Median : 8.513  
# Mean   : 19.35   Mean   : 9.878   Mean   :39.11   Mean   :6.009   Mean   :14.240   Mean   : 8.499  
# 3rd Qu.: 24.79   3rd Qu.:10.810   3rd Qu.:42.71   3rd Qu.:6.555   3rd Qu.:15.439   3rd Qu.:10.386  
# Max.   : 53.12   Max.   :14.995   Max.   :58.05   Max.   :9.089   Max.   :20.891   Max.   :18.340  
# NA's   :3358     NA's   :3624     NA's   :3592    NA's   :5569    NA's   :6130     NA's   :4308    
# X61151.7        X6301.6         X6690.2          X6768.6           X704.7           X706.2       
# Min.   :1.907   Min.   :0.516   Min.   :-1.402   Min.   :-21.21   Min.   :-2.587   Min.   :-40.386  
# 1st Qu.:3.598   1st Qu.:0.988   1st Qu.: 6.344   1st Qu.: 63.83   1st Qu.:-1.651   1st Qu.:-14.305  
# Median :3.940   Median :1.106   Median : 8.109   Median : 82.02   Median :-1.450   Median : -7.433  
# Mean   :3.939   Mean   :1.105   Mean   : 8.108   Mean   : 81.64   Mean   :-1.450   Mean   : -7.406  
# 3rd Qu.:4.275   3rd Qu.:1.221   3rd Qu.: 9.900   3rd Qu.: 99.63   3rd Qu.:-1.256   3rd Qu.: -0.544  
# Max.   :5.790   Max.   :1.765   Max.   :17.516   Max.   :175.35   Max.   :-0.320   Max.   : 29.139  
# NA's   :4229    NA's   :6174    NA's   :3640     NA's   :4402     NA's   :4798     NA's   :4381     
# X711.2           X713.8           X718.7           X731.0           X736.9           X742.7      
# Min.   :-2.249   Min.   :-0.882   Min.   : 6.582   Min.   :-0.946   Min.   :-9.237   Min.   :-0.408  
# 1st Qu.:-1.096   1st Qu.: 0.091   1st Qu.:11.768   1st Qu.: 1.219   1st Qu.:15.739   1st Qu.: 0.496  
# Median :-0.840   Median : 0.312   Median :13.089   Median : 1.737   Median :22.147   Median : 0.684  
# Mean   :-0.832   Mean   : 0.321   Mean   :13.096   Mean   : 1.743   Mean   :22.147   Mean   : 0.685  
# 3rd Qu.:-0.567   3rd Qu.: 0.556   3rd Qu.:14.436   3rd Qu.: 2.277   3rd Qu.:28.785   3rd Qu.: 0.873  
# Max.   : 0.554   Max.   : 1.533   Max.   :19.694   Max.   : 4.750   Max.   :55.629   Max.   : 1.566  
# NA's   :4636     NA's   :4640     NA's   :3548     NA's   :4348     NA's   :4295     NA's   :4338    
# X751.8           X770.8           X777.3           X785.6          X786.4          X787.2      
# Min.   :-3.748   Min.   : 26.74   Min.   :-29.14   Min.   :22.42   Min.   :28.94   Min.   : 68.31  
# 1st Qu.: 3.873   1st Qu.: 58.19   1st Qu.:180.47   1st Qu.:28.91   1st Qu.:32.58   1st Qu.: 86.78  
# Median : 5.524   Median : 65.63   Median :231.33   Median :30.35   Median :33.45   Median : 90.52  
# Mean   : 5.496   Mean   : 65.75   Mean   :231.60   Mean   :30.32   Mean   :33.43   Mean   : 90.48  
# 3rd Qu.: 7.105   3rd Qu.: 73.36   3rd Qu.:282.46   3rd Qu.:31.74   3rd Qu.:34.27   3rd Qu.: 94.21  
# Max.   :13.919   Max.   :103.48   Max.   :501.40   Max.   :37.55   Max.   :37.61   Max.   :109.29  
# NA's   :5085     NA's   :4406     NA's   :3682     NA's   :3630    NA's   :3610    NA's   :3626    
# X788.0           X789.8         X9830.1           PT_ID     
# Min.   : 9.524   Min.   :2.009   Min.   :-1.304   Min.   :   1  
# 1st Qu.:13.043   1st Qu.:3.896   1st Qu.: 3.076   1st Qu.:2260  
# Median :13.922   Median :4.329   Median : 4.015   Median :4519  
# Mean   :13.928   Mean   :4.321   Mean   : 3.998   Mean   :4519  
# 3rd Qu.:14.799   3rd Qu.:4.750   3rd Qu.: 4.930   3rd Qu.:6778  
# Max.   :18.627   Max.   :6.609   Max.   : 9.136   Max.   :9037  
# NA's   :3738     NA's   :3605    NA's   :5013  


#comparing the distribution of real world and simulated data with the same mask.
library(tidyr)
realworld <- gather(labData_gs_transformed_clean_PC_before_sorted, lab, value, X10466.1:X9830.1, factor_key=TRUE)
realworld$Category <- "realworld"
colnames(realworld)[which(names(realworld) == "ID")] <- "PT_ID"
simulated <- gather(dffinal_repeat_select_widformat_t0_masked_test, lab, value, X10466.1:X9830.1, factor_key=TRUE)
simulated$Category <- "simulated"
all <- rbind(realworld, simulated)

library(ggplot2)
library(hrbrthemes)
library(viridis)
tiff("densityplot.tiff", units="in", width=15, height=12, res=300)
ggplot(data=all[, 2:4], aes(x=value, y=..scaled.., color=Category, fill=Category)) +
  geom_density(alpha = 0.6, position = "stack") +
  #geom_density(aes(x=value, y=..scaled.., color=Category), position = "dodge", geom="line")
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  facet_wrap(~lab, scales = "free") +
  theme(
    #legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "grey20", size = 8, angle = 45, hjust = .5, vjust = .5, face = "plain"),
    axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
    axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
    axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain")
) + xlab("original laboratory value") + ylab("density(scaled)")

dev.off()


#create the correlation heat map for simulated data
Cor_before_sim <- as.matrix(cor(dffinal_repeat_select_widformat_t0_masked_test[-ncol(dffinal_repeat_select_widformat_t0_masked_test)], use="pairwise.complete.obs"))

Cor_before_sim <- Cor_before_sim[as.character(colnames(Cor_before)), as.character(colnames(Cor_before))]
colnames(Cor_before_sim)
rownames(Cor_before_sim)
upper_tri_sim <- get_upper_tri(Cor_before_sim)
# Melt the correlation matrix
library(reshape2)
melted_cormat_sim <- melt(upper_tri_sim, na.rm = TRUE)

# Create a ggheatmap
library(ggplot2)
tiff("dffinal_repeat_select_widformat_t0_masked_sorted_new_cor_half.tiff", units="in", width=10, height=8, res=600)
ggheatmap <- ggplot(melted_cormat_sim, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
dev.off()

