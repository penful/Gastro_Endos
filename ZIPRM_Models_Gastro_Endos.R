#### MODELS PROPOSED IN THE  PAPER  ##### 
# Risk of adverse events in gastrointestinal endoscopy:  Zero-inflated Poisson regression mixture model for count data 
# and multinomial logit model for the type of event
# Authors: Gemma, M., Pennoni, F., Tritto, R, and Agostoni, M. #
##############################################################

#### ZIPRM MODEL FOR  ADULT POPULATION  ####
# A is a dataframe with the response variable n_eventi and
# continous (e.g. asa score) and categorical covariates
require(flexmix)
Model_pois <- FLXMRziglm(family = "poisson")
set.seed(16653)
Fitted_poisa <- stepFlexmix(A$n_event ~ A$gender + 
                              A$age + A$bmi + 
                              A$ASA + A$Mallampati +
                              A$smoke + A$regime + 
                              A$intype + A$time + A$opioid +
                              A$gas + A$benzodiaze + A$other +
                              A$curare + A$inductive | id,
                              model=Model_pois, k=1:5, nrep = 3, data=A)

# Model output
BIC(Fitted_poisa)
Fitted_poisa@logLiks
Fitted_poisa@k
plot(Fitted_poisa)

#### To perform model selection based on the Bayesian Information Criterion (BIC) ####
mod2<- getModel(Fitted_poisa, which = "BIC")
mod2 <- relabel(mod2, "model", "Intercept")

# Extract estimated parameters  #
Rmod2 <-refit(mod2)
# To plot confidence intervals
print(plot(refit(mod2)))
parameters(mod2)
summary(refit(mod2), which = "concomitant")
plot(mod2)
print(plot(mod2, mark = 2, col = "grey", markcol = 1))
table(mod2@cluster, A$n_event)
table(clusters(mod2))

#### ZIPRM MODEL FOR  ADULT POPULATION treated with propofol vs 
#### Treated with propofol alone vs propofol in  addition to other drugs ####
Model_pois0<- FLXMRziglm(family = "poisson")
set.seed(62634)
Fitted_poisa0 <- stepFlexmix(B$n_eventi ~  
                               B$gender + 
                               B$age + 
                               B$bmi + 
                               B$ASA + 
                               B$Mallampati + 
                               B$smoke + 
                               B$regime + 
                               B$type + 
                               B$time + 
                               B$drug |id,
                             model = Model_pois0, 
                             k=1:3, 
                             nrep = 3, 
                             data=B, 
                             control = list(minprior = 0.01))
BIC(Fitted_poisa0)

##### TEST for overdispersion: Poisson Model  #####
require(AER)
rd <- glm(B$n_eventi ~  
            B$gender + 
            B$age + 
            B$bmi + 
            B$ASA + 
            B$Mallampati + 
            B$smoke + 
            B$regime + 
            B$type + 
            B$time + 
            B$drug, 
            family = poisson)
summary(rd)
dispersiontest(rd)
dispersiontest(rd, trafo = 1)
dispersiontest(rd, trafo = 2)

#### TEST for Zero-Inflated Negative Binomial Model vs Zero-Inflated Poisson Model ####
require(pscl)
rdNB <- zeroinfl(B$n_eventi ~  
                   B$gender + 
                   B$age + 
                   B$bmi + 
                   B$ASA + 
                   B$Mallampati + 
                   B$smoke + 
                   B$regime + 
                   B$type + 
                   B$time + 
                   B$drug, 
                   dist = "negbin")
rdP <- zeroinfl(B$n_eventi ~  
                  B$gender + 
                  B$age + 
                  B$bmi + 
                  B$ASA + 
                  B$Mallampati + 
                  B$smoke + 
                  B$regime + 
                  B$type + 
                  B$time + 
                  B$drug,
                  dist = "poisson")

#### Model comparison using Bayesian Information Criterion ####
dfNB<-rdNB$df.null-rdNB$df.residual
BICnp<- 2*rdNB$loglik-log(rdNB$n)*dfNB; BICnp
dfP<-rdP$df.null-rdP$df.residual
BICp<- 2*rdP$loglik-log(rdP$n)*dfP; BICp

#### To estimate the multinomial logit model ####
require(foreign)
require(nnet)
# C is a data frame
# event: categorical response variable with the following categories
# 0 = "None"
# 1 = "Cardiovascular"
# 2 = "Respiratory"
# 3 = "Hemorrhagic"
# 4 = "Others"

require(nnet)
mult  <- multinom(event ~ gender + smoke_ + 
                   age_ + 
                   bmi +
                   p_asa +
                   p_mallampati+ 
                   reg +
                   type_int + 
                   dryug +
                   time,
                   maxit = 2000,
                   data = C)

# Model output 
str(test$residuals)
summary(test$residuals)
plot(test$residuals[,1])
values<- summary(test)
test$vcoefnames
z<-values$coefficients/values$standard.errors; round(z,3)
p <- (1 - pnorm(abs(z), 0, 1)) * 2; round(p,3)
round(exp(coef(test)),3) 
