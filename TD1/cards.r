## Installing the packages
list.of.packages <- c('tidyverse', 'stargazer', 'car', 'ggplot2', 'ivreg')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

## Question 1.a
df <- readRDS('data\\card.rds')

## Question 1.b
desc_stats <- select(df, wage, educ, exper, black, smsa, south, IQ, nearc4)
stargaze  r(as.data.frame(desc_stats), 
          type = "html",
          out = "output\\desc_stat.doc")

## Question 1.c
ggplot(desc_stats, aes(x = educ, y = log(wage))) +
  geom_point(color = "darkgreen", alpha = 0.5) + #Scatter points with transp
  labs(x = "educ", y = "log(wage)") +  # Label axes
  theme_minimal()  # Simple theme

## Question 1.d - word doc

## Question 1.e
ggplot(df, aes(x = exper, y = log(wage))) +
  geom_point(color = "darkgreen", alpha = 0.5) +  # Scatter points with transp
  geom_smooth(method = "lm", formula = y ~ x, color = "darkblue", se = FALSE) +  # Line ar fit
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "darkred ", se = FALSE) +  # Quadratic fit
  labs(x = "Experience (Years)", y = "Log of Wage",
       title = "Scatter Plot of Log(Wage) vs Experience with Linear and Quadratic Fits") +
  theme_minimal()  # Simple theme

## Question 1.f
est1 <- lm(log(wage) ~ educ + exper + I(exper^2) + black + south + smsa + 
             reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, 
           data = df)

summary(est1)
stargazer(est1, 
          type = "html",
          out = "output\\est1.doc")

## Question 1.g - word doc

## Question 1.h
linearHypothesis(est1, c("exper = 0", "I(exper^2) = 0"))

## Question 1.i and 1.j - word doc

## Question 1.k
est2 <- lm(log(wage) ~ educ + IQ + exper + I(exper^2) + black + south + smsa + 
             reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, 
           data = df)

summary(est2)
stargazer(est2, 
          type = "html",
          out = "output\\est2.doc")

## Question 1.l - word doc

## Question 1.m
est3 <- lm(educ ~ nearc4 + exper + I(exper^2) + black + south + smsa + 
             reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, 
           data = df)

summary(est3)
stargazer(est3, 
          type = "html",
          out = "output\\est3.doc")

## Question 1.n - word doc
linearHypothesis(est3, c("nearc4 = 0"))

## Question 1.o
est4 <- lm(log(wage) ~ nearc4 + exper + I(exper^2) + black + south + smsa + 
             reg662 + reg663 + reg664 + reg665 + reg666 + reg667 + reg668 + reg669, 
           data = df)

summary(est4)
stargazer(est4, 
          type = "html",
          out = "output\\est4.doc")

# Question 1.p
controls <- paste("exper", "I(exper^2)", "black", "south", "smsa", "reg662", 
                  "reg663", "reg664", "reg665", "reg666", "reg667","reg668","reg669",
                  sep = "+")

form5 <- as.formula(paste("lwage ~ educ","+", controls, "|", controls, "+", "nearc4"))

est5 <- ivreg(form5, data = df)
summary(est5)
stargazer(est5, 
          type = "html",
          out = "output\\est5.doc")

# Question 1.q
stargazer(est1, est2, est3, est4, est5, 
          type = "html",
          omit = c("reg6"),
          omit.labels = "Region dummies?",
          out = "output\\estimations.doc")