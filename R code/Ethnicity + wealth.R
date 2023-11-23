library(mitools)
library(survey)
library(srvyr)
#srvyr is a dplyr version of survey, check out the "Vignettes":
#https://cran.r-project.org/web/packages/srvyr/index.html
library(mice)
library(tidyverse)
library(oaxaca)
library(devtools)
install_github('djalmapessoa/convey')
library(convey)
library(haven)

#change to own file path
dataset <- read_dta("/Users/raphaelboulat/Desktop/data finlit.dta")

################################## CREATING DUMMIES #########################################

#create a dummy for every question (1=true, 0= wrong, don't know or not answered).

dataset$question_1 <- ifelse(dataset$question_1 == "5", 1, 0)
dataset$question_2 <- ifelse(dataset$question_2 == "1", 1, 0)
dataset$question_3 <- ifelse(dataset$question_3 == "5", 1, 0)

#create the variable financial literacy (0,1,2,3).

dataset1 <- dataset %>% 
  mutate(financial_literacy = question_1+question_2+question_3)

summary(dataset1$financial_literacy) # really high median and results more generally

#create dummy for sex (1=female)
dataset1$sex_1 <- ifelse(dataset1$sex_1 == "2", 1, 0)

dataset1$sex_2 <- na_if(dataset1$sex_2, 0) #changed the 0's into NA's (because some people are single)
dataset1$sex_2 <- ifelse(dataset1$sex_2 == "2", 1, 0)

dataset1$marriage_status_1 <- ifelse(dataset1$marriage_status_1 == "1", 1, 0) #1=married

#use a dummy for university education
dataset1$education_1 <- ifelse(dataset1$education_1 %in% 9:14, 1,0) #university education=1
dataset1$education_2 <- ifelse(dataset1$education_2 %in% 9:14, 1,0) 
dataset1$mother_educ_1 <- ifelse(dataset1$mother_educ_1 == "12", 1,0)         
dataset1$father_educ_1 <- ifelse(dataset1$father_educ_1 == "12", 1,0)

#create age categories dummies:
dataset1$age_18_29 <- ifelse(dataset1$age<30, 1, 0)
dataset1$age_30_49 <- ifelse(dataset1$age %in% 30:49, 1, 0)
dataset1$age_50_69 <- ifelse(dataset1$age %in% 50:69, 1, 0)
dataset1$age_70 <- ifelse(dataset1$age >70, 1, 0)

#create a dummy for employment, self-employed and retired, other/not working
dataset1$employment_dummy <- ifelse(dataset1$work_status_1<16,1,0) # "Worker + unemployed/looking for work" coded as 0. 
dataset1$retired_dummy <-  ifelse(dataset1$work_status_1==50,1,0)
dataset1$self_employed_dummy <-  ifelse(dataset1$work_status_1==50,1,0)
dataset1$not_working_dummy <- ifelse(dataset1$work_status_1 %in% 20:30,1,0)

#dummy for single person household
dataset1$one_person_household <- ifelse(dataset1$household_size==1, 1,0)
dataset1$four_person_household <- ifelse(dataset1$household_size>=4,1,0)

#variable for ethnicity
dataset1$black <- ifelse(dataset1$ethnicity==2,1,0)
dataset1$white <- ifelse(dataset1$ethnicity==1,1,0)    
dataset1$hispanic <- ifelse(dataset1$ethnicity==3,1,0)

### inverse hyperbolic sine transformation on wealth
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

dataset1$networth <- ihs(dataset1$networth)

################################### IMPLICATES/WEIGHTING  #########################################


#We start by creating the implicate variable from the ID
dataset1$mi.idx <- substr(dataset1$y1, nchar(dataset1$y1-1), nchar(dataset1$y1))
#See:
table(dataset1$mi.idx)
#And compare to the index you had created (mi.idx<-dataset$yy1)
table(dataset1$yy1)

#Second thing to notice it that mi.idx (as well as yyi) is a character vector, not a numeric vector
class(dataset1$mi.idx)
#This implies that a for loop running over a numeric vector like 1:5 is doomed to fail
#because the variable you are looping over is a character vector, not a numeric one

#SO
for(i in 1:5){}
#would not work for a character

#Instead we need a loop over the character
imps <- c("1", "2", "3", "4", "5")

for(i in 1:length(imps)){
  
}

#But you can also record the mi.idx to a numeric variable
dataset1$imp_id <- as.numeric(dataset1$mi.idx)
dataset1$imp_id <- as.numeric(dataset1$imp_id)

#Create vector with filnames
datasetnames <- rep("data", 5)

for(i in 1:5){
  #Setp 1 Filter Data by imp_id: (imp_id == i)
  tmp <- dataset1 %>% dplyr::filter(imp_id == i)
  #Setp 2And assign Filnes (data_1, data_2,...)
  assign(paste0("data_", i), tmp)
}

#change to own file path
weights <- read_dta("/Users/raphaelboulat/Desktop/p19_rw1.dta")

weights <-  weights %>% replace(is.na(.), 0)

repw <-  inner_join(data_1, weights, by = "y1") 

us.mih<-split(dataset1,factor(dataset1$imp_id))


us.svymih<-svrepdesign(id=~yy1,
                       weights=~x42001,
                       #using 200 weights, first to columns are IDs
                       repweights=weights[3:202],
                       data=imputationList(us.mih),
                       scale = 4/200, #4/number or rep weights
                       rscale = rep(1, 200),
                       type="other",
                       combined.weights = TRUE,
                       mse = TRUE)

us.svymih <- convey_prep(us.svymih)

############################### OAXACA ###################################

#1. WE fit the model by group to get betas using Replicate Weights + Implicates
model_formula <- formula( financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                            mother_educ_1 + father_educ_1 + employment_dummy)

lm_whites <-MIcombine(with(subset(us.svymih, white==1),
                           svyglm(model_formula)))
summary(lm_whites)
betas_w <- lm_whites$coefficients
se.betas.w <- sqrt(diag(lm_whites$variance))

#saving the main elements only 

lm_whites_res <- data.frame(betas = lm_whites$coefficients,
                            se = sqrt(diag(lm_whites$variance)),
                            ci_lower = summary(lm_whites)[[3]],
                            ci_upper = summary(lm_whites)[[4]])

lm_whites_res$ethnicity <- "Whites" ## doubt

models_whites<- with(subset(us.svymih, white==1),
                     svyglm(financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                              mother_educ_1 + father_educ_1 + employment_dummy))

##Let's Write a Function for Combining all the Summary Statistics that we need from the models
get_sumstats <- function(with_regoutput=models_whites,
                         MIc_regoutput=lm_whites,
                         model_name="Whites"){
  
  out <- data.frame(betas = MIc_regoutput$coefficients,
                    se = sqrt(diag(MIc_regoutput$variance)),
                    ci_lower = summary(MIc_regoutput)[[3]],
                    ci_upper = summary(MIc_regoutput)[[4]])
  out.b <- c()
  for(i in 1:length(with_regoutput)){
    out.b[i] <- with_regoutput[[i]]$aic
  }
  
  out$aic <- mean(out.b)
  out$n_obs <- length(with_regoutput[[1]]$residuals)
  out$model_name <- model_name
  out$formula <- paste(as.character(with_regoutput[[1]]$formula), collapse = " ")
  out$var.name <- rownames(out)
  out <- out %>% dplyr::select(var.name, model_name, everything())
  rownames(out) <- NULL
  return(out)
}

lm_whites_res <- get_sumstats(with_regoutput=models_whites,
                              MIc_regoutput=lm_whites,
                              model_name="Whites")

## Now let's turn to blacks

lm_blacks <-MIcombine(with(subset(us.svymih, black==1),
                           svyglm(financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                                    mother_educ_1 + father_educ_1 + employment_dummy)
))
models_blacks<- with(subset(us.svymih, black==1),
                     svyglm(financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                              mother_educ_1 + father_educ_1 + employment_dummy))

betas_b <- lm_blacks$coefficients
se.betas.b <- sqrt(diag(lm_blacks$variance))

lm_blacks_res <- get_sumstats(with_regoutput=models_blacks,
                              MIc_regoutput=lm_blacks,
                              model_name="Blacks")

## we obtained beta coefficient by groups

##2. We also need estimates of the Means (X and Y)
means.w <- MIcombine(with(subset(us.svymih,
                                 white==1),
                          svymean(financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                                    mother_educ_1 + father_educ_1 + employment_dummy)))

#Get Coef and SE and Confint
est.means.w <- means.w$coefficients
#means.m$variance is a variance covariance matrix
se.means.w <- sqrt(diag(means.w$variance))
confint.means.w <- t(confint(means.w))

#Build A Data Frame to Save the Results and Merge them with Model Results
prep_oaxaca_data <- function(MIoutput = means.w,
                             res_model = lm_whites_res,
                             model_name = "Whites",
                             m.variables.names = c("Y:Financial literacy",
                                                   "age_18_29",
                                                   "age_30_49", 
                                                   "age_50_69", 
                                                   "age_70", 
                                                   "sex_1", 
                                                   "education_1", 
                                                   "networth",
                                                   "mother_educ_1",  
                                                   "father_educ_1", 
                                                   "employment_dummy"),
                             dependent.var = c("Y:Financial literacy"),
                             controls = c("age_18_29", "age_30_49" , "age_50_69",  "age_70",  "sex_1",  "education_1", "networth",
                                          "mother_educ_1" , "father_educ_1" , "employment_dummy"),
                             intercept = c("(Intercept)")){
  out <- data.frame(
    est.means=MIoutput$coefficients,
    se.means = sqrt(diag(MIoutput$variance)),
    ci.means_lower = confint(MIoutput)[,1],
    ci.means_upper = confint(MIoutput)[,2],
    model_name = model_name,
    var.name = m.variables.names)
  rownames(out) <- NULL
  out <- merge(out, res_model,
               all.x = TRUE,
               all.y = TRUE)
  
  out <- out %>% dplyr::mutate(var_class = case_when(var.name %in% dependent.var ~ "Dep.Var",
                                                     var.name %in% controls ~ "Control",
                                                     var.name %in% intercept ~ "Intercept"))
  out
}

oaxaca_whites <- prep_oaxaca_data(MIoutput = means.w,
                                  res_model = lm_whites_res,
                                  model_name = "Whites")

means.b<-MIcombine(with(subset(us.svymih,
                               black==1),
                        svymean(financial_literacy ~  age_18_29 + age_30_49+ age_50_69 + age_70 + sex_1 + education_1 + networth +
                                  mother_educ_1 + father_educ_1 + employment_dummy)))

oaxaca_blacks <- prep_oaxaca_data(MIoutput = means.b,
                                  res_model = lm_blacks_res,
                                  model_name = "Blacks")

### We can get the results now

oaxaca_results <- bind_rows(oaxaca_blacks, oaxaca_whites)

reshape(oaxaca_results,
        idvar = "var.name", #what we want to keep (variable name)
        timevar = "model_name",
        direction = "wide") -> oaxaca_results_w

data <- oaxaca_results_w

mean.A = "est.means.Blacks"
mean.B = "est.means.Whites"
beta.A = "betas.Blacks"
beta.B = "betas.Whites"
beta.ref = "betas.Whites"
X.ref = "est.means.Blacks"
var_class="var_class.Whites"
var_name ="var.name"

do_oaxaca <- function(data,
                      mean.A = "est.means.Blacks",
                      mean.B = "est.means.Whites",
                      beta.A = "betas.Blacks",
                      beta.B = "betas.Whites",
                      beta.ref = "betas.Whites",
                      X.ref = "est.means.Blacks",
                      var_class="var_class.Whites",
                      var_name ="var.name"){
  
  dat <-  data %>% dplyr::select(mean.A={{mean.A}},
                                 mean.B={{mean.B}},
                                 beta.A={{beta.A}},
                                 beta.B = {{beta.B}},
                                 beta.ref = {{beta.A}},
                                 X.ref = {{X.ref}},
                                 var_class ={{var_class}},
                                 var_name = {{var_name}})
  classes <- c("Intercept",
               "Control",
               "Dep.Var")
  
  classes_test <- dat$var_class %in% classes
  if(sum(classes_test==FALSE)){
    print("Please check the variable Varible Classes (var_class),
          all must be either Intercept, Control or Dep.Var")
  }
  
  
  outcome.diff <- dat %>% filter(var_class=="Dep.Var") %>%
    dplyr::mutate(outcome_diff = mean.A - mean.B) %>%
    select(outcome_diff)
  
  res <- dat %>% filter(var_class=="Control") %>%
    dplyr::mutate(diff_in_X = (mean.A - mean.B),
                  exp_by_var = (diff_in_X *beta.ref),
                  total_exp = sum(exp_by_var),
                  diff_in_beta = beta.A - beta.B,
                  unex_by_var = X.ref *diff_in_beta,
                  unex_total = sum(unex_by_var)) %>%
    dplyr::select(var_name, var_class, diff_in_X, exp_by_var, total_exp,
                  diff_in_beta, unex_by_var, unex_total)
  
  res$outcome.diff <- outcome.diff[1,1]
  
  intercept.diff <- dat %>% filter(var_class=="Intercept") %>%
    dplyr::mutate(unex_interc = beta.A - beta.B) %>%
    select(unex_interc)
  
  res$unex_interc <-  intercept.diff[1,1]
  
  res <- res %>% dplyr::mutate(total_unexplained = unex_interc + unex_total,
                               test = total_unexplained + total_exp,
                               fraction_exp = total_exp/outcome.diff,
                               fraction_unexp = total_unexplained/outcome.diff) %>%
    dplyr::mutate(across(where(is.numeric), round, 2))
  
  return(res)
}

res_a <- do_oaxaca(data = oaxaca_results_w)
res_b <- do_oaxaca(dat = oaxaca_results_w,
                   beta.ref = "betas.Blacks",
                   X.ref = "est.means.Whites")

######################### STANDARD ERRORS #######################

## T test
black <- dataset1[which(dataset1$black=="1"),]
white <- dataset1[which(dataset1$black=="0"),]

t.test(black$financial_literacy, white$financial_literacy)

# Create vectors for the means without financial literacy
oaxaca_blacks = oaxaca_blacks[2:11,]
oaxaca_whites = oaxaca_whites[2:11,]

mean_A <- oaxaca_blacks$est.means
mean_B <- oaxaca_blacks$est.means

beta_A <- oaxaca_blacks$betas
beta_B <- oaxaca_whites$betas


####var(q) --> explained part
## first term
diff.means <- as.matrix(mean_B - mean_A)
tr.diff.means <- t(diff.means)
var_cov_w <- lm_whites$variance[2:11, 2:11] # variance covariance matrix
var_q_1 <- tr.diff.means%*%var_cov_w%*%diff.means

##second term
var.cov.w <- means.w$variance[2:11, 2:11]
var.cov.b <- means.b$variance[2:11, 2:11]
sum.vars <- var.cov.w + var.cov.b
beta_B <- as.matrix(beta_B)
t_beta_B <- t(beta_B)
var_q_2 <- t_beta_B%*%sum.vars%*%beta_B

## explained part
var.q <- var_q_1 + var_q_2
se.q <- sqrt(var.q)

####var(u)
# first term
means.f <- as.matrix(mean_A)
t.means.f <- t(means.f)
var_cov_b <- lm_blacks$variance[2:11, 2:11]
sum_vars <- var_cov_b+var_cov_w
var_u_1 <- t.means.f%*%sum_vars%*%means.f

diff.betas <- as.matrix(beta_B - beta_A)
t.diff.betas <- t(diff.betas)
var_u_2 <- t.diff.betas%*%var.cov.b%*%diff.betas

var.u <- var_u_1 + var_u_2
se.u <- sqrt(var.u)

### var(r)
var.decomp.terms <- var.q + var.u 
se.decomp.terms <- sqrt(var.decomp.terms)

