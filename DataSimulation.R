library(dplyr)
library(ggplot2)
library(tidyverse)
library(Hmisc)
library(lattice)
library(MASS)
library(xlsx)

#### create dataset of subjects #### 
#create 100 subjects, with 5 entries each
subj_id = rep(c(1:100),5)

#create subject characteristics
subj_characteristics = data.frame(
  Age = as.numeric(round(runif(100, min = 18, max = 45), digits = 0)),
  Height = as.numeric(round(runif(100, min = 1.5, max = 2 ), digits = 2)),
  BMI = as.numeric(round(runif(100, min = 18, max = 32 ), digits = 1))
)
subj_characteristics$Mass = round(subj_characteristics$BMI * (subj_characteristics$Height)^2, digits = 1)

#combine IDs with subject characteristics
Subj = cbind(subj_id, subj_characteristics)

#### create datset with shoes and their mechanical properties ####
#create 10 shoes brands with 5 shoes each
shoe_brand = c('A','B','C',
               'Q','R','S','T',
               'X', 'Y', 'Z')
#shoe_name = c('V1', 'V2', 'V3','V4', 'V5')


#function to combine shoe brands and shoe names, in addition to simulated mechanical properties
combineShoeName = function(brand_name) {
  
  tmpbrand = rep(brand_name, 5)
  shoe_name = c(paste0(brand_name, 1),
                paste0(brand_name, 2),
                paste0(brand_name, 3),
                paste0(brand_name, 4),
                paste0(brand_name, 5))
  HeelGmax = as.numeric(round(runif(5, min = 8, max = 13 ), digits = 1))
  ForefootGmax = as.numeric(round(runif(5, min = 10, max = 18 ), digits = 1))
  HeelDurometer = as.numeric(round(runif(5, min = 30, max = 70 ), digits = 1))
  ForefootDurometer = as.numeric(round(runif(5, min = 30, max = 70 ), digits = 1))
  HeelStack = as.numeric(round(runif(5, min = 25, max = 45 ), digits = 1))
  Drop = as.numeric(round(runif(5, min = 5, max = 15 ), digits = 1))
  ForefootStack = HeelStack - Drop
  HeelER = as.numeric(round(runif(5, min = 45, max = 75 ), digits = 1))
  ForefootER = as.numeric(round(runif(5, min = 45, max = 75 ), digits = 1))
  HeelLR = as.numeric(round(runif(5, min = 0.4, max = 0.9 ), digits = 2))
  ForefootLR = as.numeric(round(runif(5, min = 0.5, max = 1.5 ), digits = 2))
  HeelTTP = as.numeric(round(runif(5, min = 4, max = 22 ), digits = 1))
  ForefootTTP = as.numeric(round(runif(5, min = 12, max = 18 ), digits = 1))
  Flex = as.numeric(round((runif(5, min = 0.25, max = 1.25 )*0.0971), digits = 3))
  Weight = as.numeric(round(runif(5, min = 175, max = 350 ), digits = 0))
  tmpshoe = cbind(tmpbrand, shoe_name, HeelGmax, ForefootGmax, 
                  HeelDurometer, ForefootDurometer, HeelStack, ForefootStack,
                  Drop, HeelER, ForefootER, HeelLR, ForefootLR, 
                  HeelTTP, ForefootTTP, Flex, Weight)
}

#create temporary dataframe of shoes and mechanical properties
shoeDF = data.frame()

for (i in 1:length(shoe_brand)) {
  
  tempshoe = combineShoeName(shoe_brand[i])
  shoeDF = rbind(shoeDF, tempshoe)
  
}

#### combine shoe dataframe with subject IDs #### 
tmp_shoe_df =  rbind(shoeDF, shoeDF)

#need to shuffle shoe dataframe to randomly assign shoes to subjects
SimShoeDF = cbind(Subj,
                  rbind(tmp_shoe_df[sample(1:nrow(tmp_shoe_df)),],
                        tmp_shoe_df[sample(1:nrow(tmp_shoe_df)),],
                        tmp_shoe_df[sample(1:nrow(tmp_shoe_df)),],
                        tmp_shoe_df[sample(1:nrow(tmp_shoe_df)),],
                        tmp_shoe_df[sample(1:nrow(tmp_shoe_df)),]
                        ))
names(SimShoeDF)[c(1,6,7)] = c('ID', 'Brand', 'Shoe')

#check to make sure no subject has duplicates of a shoe
check_duplicates = data.frame('ID' = rep(0,100),
                              'NumTrue' = rep(0,100))

for (i in 1:100) {
  
  check_duplicates$ID = i
  check_duplicates$NumTrue = sum(duplicated(SimShoeDF[which(SimShoeDF$ID == i),]))
  
}

sum(check_duplicates$NumTrue) # needs to equal 0 - if not, run SimShoeDF code again

#### create biomechanics dataset ####
joints = c('Hip', 'Knee', 'Ankle_RMF')
dims = c('X', 'Y', 'Z')
vars = c('Peak Angle', 'Angle ROM', 'Angle @ Footstrike', 'Norm. Moment', 'Peak Power')
vars_other = c('Angle Max', 'Angle Min', 'Angle ROM')

BigCush = as.data.frame(read_excel('/Users/labwork/Downloads/FinalCombined.xlsx'))
newmean = function(x) {
  mean(as.numeric(x), na.rm = TRUE) }

newSD = function(x) {
  sd(as.numeric(x), na.rm = TRUE) }

new.max = function(x) {
  max(as.numeric(x), na.rm = TRUE) }

new.min = function(x) {
  min(as.numeric(x), na.rm = TRUE) }

#biomech means, SD, max, min retrieved from private dataset - not loaded here
biomech_means = as.data.frame(apply(BigCush[,c(27,28,35:54,91,92,93,96,98:109,116:123,124,126, 128:140,147:154, 159,161,162)], 
                                    2, newmean))
biomech_SD = as.data.frame(apply(BigCush[,c(27,28,35:54,91,92,93,96,98:109,116:123,124,126, 128:140,147:154,159,161,162)], 
                                 2, newSD))
biomech_max = as.data.frame(apply(BigCush[,c(27,28,35:54,91,92,93,96,98:109,116:123,124,126, 128:140,147:154,159,161,162)], 
                                  2, new.max))
biomech_min = as.data.frame(apply(BigCush[,c(27,28,35:54,91,92,93,96,98:109,116:123,124,126, 128:140,147:154,159,161,162)], 
                                  2, new.min))

biomech_vals = cbind(biomech_means, biomech_SD, biomech_max, biomech_min)
names(biomech_vals) = c('Means', 'SDs', 'Max', 'Min')

#create function to generate biomechanical data
getSimBiomech = function(ref_vals, joint_name, var_name, dim){
  
  row_key = paste0(joint_name, '_', gsub(' ', '_', var_name), '_', dim)
  df_loc = which(rownames(biomech_vals) == row_key)
  
  Joint_Dim = rep(dim,500)
  tempvar = as.numeric(round(runif(500, 
                                   min = biomech_vals$Mean[df_loc] - 10,
                                   max = biomech_vals$Mean[df_loc]) + 10, 
                             digits = 1))
  varDF = as.data.frame(cbind(Joint_Dim, tempvar))
  varDF$tempvar = as.numeric(varDF$tempvar)
  colnames(varDF) = c('Dim', paste0(joint_name, '_', gsub(' ', '_', var_name)))
  return(varDF)
  
  }

#loop through dimensions (X,Y,Z), variables, and joints to generate biomechanics data
# 3 dimensions per variable per shoe per subject -> 1500 total
biomech_sim = data.frame()
temp_biomech = data.frame(placeholder = rep(0,500))
for (d in 1:length(dims)) {
  count = 0
  for (v in 1:length(vars_other)) {
    
    for (j in 1:length(joints)){
      count = count + 1
      temp_val = getSimBiomech(biomech_vals, joints[j], vars_other[v], dims[d])
      temp_biomech[,count] = temp_val[,2]
      names(temp_biomech)[count] = c(paste0(joints[j], '_', gsub(' ', '_', vars_other[v])))
    } 
    
    temp_biomech$Dim = rep(dims[d], nrow(temp_biomech))

    }
      temp_biomech$Dim = rep(dims[d], nrow(temp_biomech))
      if (d == 1) {
        biomech_sim = temp_biomech} else {biomech_sim = rbind(biomech_sim, temp_biomech) }

}

#### create perception dataset ####
ratings_levels = c('Very Dissatisfied',
            'Dissatisfied',
            'Slightly Dissatisfied',
            'Neutral',
            'Slightly Satisfied',
            'Satisfied',
            'Very Satisfied'
            )

#generate perception data - one value for each perception variable per shoe per subject (500 total)
Perception = data.frame(
  HeelCushion = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  ForefootCushion = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  Flexibility = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  Transition = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  Stability = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  Overall = factor(sample(ratings_levels, 500, replace = TRUE), ordered = TRUE, levels = ratings_levels),
  row.names = NULL
)

#### create stride metrics ####
stride_metric_vars = c('Cadence_Strides_Per_Minute', 'Delta_COM', 'Duty_Factor', 'Ground_Contact_Time',
                       'Stride_Length', 'Swing_Time')

#create function to generate stride metric values
getStrideMetrics = function(biomech_vals, var_name) {
  
  df_loc = which(rownames(biomech_vals) == var_name)
  tempvar = as.numeric(round(runif(500, 
                                   min = biomech_vals$Min[df_loc],
                                   max = biomech_vals$Max[df_loc]), 
                             digits = 3))
}

#generate some stride metrics - one value for each stride metric per shoe per subject (500 total)
stride_metrics = data.frame(rep(0,500))

for (var in 1:length(stride_metric_vars)) {
  
  tempvar = getStrideMetrics(biomech_vals, stride_metric_vars[var])
  stride_metrics[,var] = tempvar
  names(stride_metrics)[var] = stride_metric_vars[var]
}


#### create final dataset ####
#combine previous subject/shoe data with perception and stride metrics
SimShoeDF_new = cbind(SimShoeDF, Perception, stride_metrics)

#combine all data into final dataframe
SimFootwearData = cbind(rbind(SimShoeDF_new, SimShoeDF_new, SimShoeDF_new), biomech_sim)

write.xlsx(SimFootwearData, 
           file = '/Users/labwork/R-shiny-BigCush/SimulatedFootwearData.xlsx', 
           col.names = TRUE, row.names = FALSE)
