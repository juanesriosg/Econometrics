## Installing the packages
list.of.packages <- c('tidyverse', 'ivreg', 'stargazer', 'fastDummies','dplyr','OneR','stringr')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

## Question 1.f
df <- readRDS("data\\procura.rds")

colnames(df)

desc_stats <- select(df, SA, duration_years, ln_Bprice,
                     GPP_n, small_supplier_n, ln_Budget, ln_minor)

stargazer(as.data.frame(desc_stats), 
          type = "html",
          out = "output\\desc_stat.doc")

#Question 1.g

df$entity_power <- factor(df$entity_power)
df$contract_type <- factor(df$contract_type)

controls <- paste( "entity_power", "contract_type", "ct_publication", "duration_years",
 "ln_Bprice", "small_supplier_n", "ln_Budget", sep = "+")

form <- as.formula((paste0("GPP_n ~ SA +", controls)))

est1 <- lm(form, data=df)

summary(est1)

stargazer(est1,
    type = "html",
    out = "output\\est_1.doc"
)

#Question 1.k

df <- df %>%
  mutate(SA_d = ifelse(ln_Bprice <= ln_minor, 1, 0))%>% #Creation of a new binary variable depending on Bprice and minor
  mutate(k = ln_Bprice - ln_minor) #Creation of a new variable as the difference of Bprice and Minor

#Creation of a new df grouped by the varaible SA_d that were just created
df_sa <- df %>%
    group_by(SA_d) %>% #Grouping by SA_d
    mutate(bins = OneR::bin(k, nbins = 20, method = c("length")))%>% #Bin variable k into 20 bins
    ungroup() #Ungrouping the data

table <- as.data.frame(str_split_fixed(df_sa$bins, ",", 2)) #Split of columns in two parts


table <- table %>%
  mutate(low = as.numeric(substring(V1,2)))%>% #Getting the lower limit of each bin
  mutate(high = as.numeric(str_sub(V2,-8,-2)))%>% #Getting the upper limit of each bin
  mutate(bin = (low+high)/2) #Calculating the mean of each bin

df_sa  <- df_sa  %>%
  mutate(bins = table$bin) #Adding the bin midpoints just calculated

df_sa  <- df_sa  %>%
  group_by(bins)%>% #Grouping by bins
  mutate(prob_mean = mean(SA)) #Calculation of the mean SA for each bin

#Defining a formula for a linear regression model with polynomial terms
F1 <- as.formula(SA ~ poly(k,SA_d,degree=1,raw=TRUE))

#Fitting the linear model using the formula
linhat <- lm(formula = F1, data=df_sa)

#Predicting and saving the values in a new column
df_sa$lin_pred <- predict(linhat)

#Creating separate predicted values based in the category SA_d 
df_sa <- df_sa %>%
  mutate(lin_pred_r = ifelse(SA_d==0,NA,lin_pred))%>% 
  mutate(lin_pred_l = ifelse(SA_d==1,NA,lin_pred))


graph <- ggplot(data=df_sa, aes(x=bins, y=prob_mean))+ #Initialization of a plot
  geom_point(shape= 21, fill = "steelblue4", size = 2, alpha = 0.5) + #Addition of points 
  geom_line(aes(x=k, y=lin_pred_l), color ="black") + #Fitted line for SA_d = 1
  geom_line(aes(x=k, y=lin_pred_r), color ="black") + #Fitted line for SA_d = 0
  geom_vline(xintercept=0, color="steelblue3", size = 1.2) + #Adding vertical line
  xlim(-3,3)+ #Setting screen limits
  theme_classic()+
  labs(x="k = ln_Bprice - ln_minor", y=" ") #Labeling axis
  

graph #Displaying graph

#Question 1.k (2)
form2 <- as.formula(paste0("SA ~ SA_d + k + SA_d*k + ",controls))
est2 <- lm(form2, data = df)
summary(est2)

stargazer(est2,
    type = "html",
    out = "output\\est_2.doc"
)

#Question 1.m
form3 <- as.formula(paste0("GPP_n ~ SA_d + k + SA_d*k+ ",controls))
est3 <- lm(form3, data = df)
summary(est3)

stargazer(est3,
    type = "html",
    out = "output\\est_3.doc"
)

#Question 1.n

#Creating formula for the regression linear model with SA as response variable from SA_d and k with the controls
fs <- as.formula(paste0("SA ~ SA_d*k +", controls)) 

#Fitting lrm with the formula using the dataframe df_sa
reg_fs <- lm(fs, data = df_sa)

#Using the fitted model to predict values of SA storing the predicted values in hat_sa 
df_sa$hat_sa <- predict(reg_fs)

#Creating another formula for other lrm this time with GPP_n as response variable, using control, hat_sa and k as predictors
f2s <- as.formula(paste0("GPP_n ~ hat_sa*k +", controls))

#Fitting the new lrm 
reg_iv <- lm(f2s, data = df_sa)

#Question 1.o
stargazer(est1,est2,est3, reg_iv,
        type="html",
        out = "output\\estimations.doc")