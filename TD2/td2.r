## Installing the packages
list.of.packages <- c("tidyverse", "stargazer", "car", "ggplot2", "ivreg")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

## Question 1.b
df <- readRDS("data\\tippelzones.rds")

view(df)

# Question 1.c
grouped_df <- df %>%
    group_by(city) %>%
    summarise(sum_opening = sum(opening))

df_implemented <- grouped_df %>%
    filter(sum_opening != 0)

df_not_implemented <- grouped_df %>%
    filter(sum_opening == 0)

df_implemented

df_not_implemented

## Question 1.d

desc_stats <- df %>%
    group_by(treat) %>%
    summarise(
        mlnrapesexaN = round(mean(lnrapesexaN), 2),
        sdlnrapesexaN = round(sd(lnrapesexaN), 4),
        mlnsexassaultN = round(mean(lnsexassaultN), 2),
        sdlnsexassaultN = round(sd(lnsexassaultN), 4),
        mlnrapeN = round(mean(lnrapeN), 2),
        sdlnrapeN = round(sd(lnrapeN), 4),
        mlnmaltreatN = round(mean(lnmaltreatN), 2),
        sdlnmaltreatN = round(sd(lnmaltreatN), 4),
        mlnweaponsN = round(mean(lnweaponsN), 2),
        sdlnweaponsN = round(sd(lnweaponsN), 4),
        mlndrugsN = round(mean(lndrugsN), 2),
        sdlndrugsN = round(sd(lndrugsN), 4)
    ) %>%
    ungroup()

desc_stats <- as.data.frame(t(desc_stats))

for (i in seq(from = 1, to = 13, by = 2)) {
    desc_stats[i, ] <- paste0("(", format(unlist(desc_stats[i, ])), ")")
}

desc_stats <- desc_stats[c(-1), ]

var_names <- c(
    "Sexual abuse and Rape", "",
    "Sexual abuse", "",
    "Rape", "",
    "Assault", "",
    "Weapons", "",
    "Drugs", ""
)

desc_stats <- cbind(var_names, desc_stats)

desc_stats <- desc_stats %>%
    rename(
        No_Tippelzones = V1,
        Tippelzones = V2
    )

stargazer(as.data.frame(desc_stats),
    summary = FALSE,
    rownames = FALSE,
    type = "html",
    out = "output\\desc_stat.doc"
)


## Question 1.f

city_stats <- df %>%
    group_by(treat) %>%
    summarise(
        mlogpopdens = round(mean(logpopdens), 2),
        sdlogpopdens = round(sd(logpopdens), 4),
        mlogpopmale1565 = round(mean(logpopmale1565), 2),
        sdlogpopmale1565 = round(sd(logpopmale1565), 4),
        mnondutchpc = round(mean(nondutchpc), 2),
        sdnondutchpc = round(sd(nondutchpc), 4),
        meduchpc = round(mean(educhpc), 2),
        sdeduchpc = round(sd(educhpc), 4),
        minkhh = round(mean(inkhh), 2),
        sdinkhh = round(sd(inkhh), 4),
        minsurWWAO = round(mean(insurWWAO), 2),
        sdinsurWWAO = round(sd(insurWWAO), 4)
    ) %>%
    ungroup()

city_stats <- as.data.frame(t(city_stats))

for (i in seq(from = 1, to = 13, by = 2)) {
    city_stats[i, ] <- paste0("(", format(unlist(city_stats[i, ])), ")")
}

city_stats <- city_stats[c(-1), ]

var_names_cities <- c(
    "Population Density", "",
    "Population Male 15-65", "",
    "Inmigrants (percent)", "",
    "Higher Educated (percent)", "",
    "Income", "",
    "unemployment insurance recipients (percent)", ""
)

city_stats <- cbind(var_names_cities, city_stats)

city_stats <- city_stats %>%
    rename(
        No_Tippelzones = V1,
        Tippelzones = V2
    )

stargazer(as.data.frame(city_stats),
    summary = FALSE,
    rownames = FALSE,
    type = "html",
    out = "output\\cities_stat.doc"
)

## Question 1.i

est1 <- lm(
    lnmaltreatN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est1)

est2 <- lm(
    lnweaponsN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est2)

est3 <- lm(
    lndrugsN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est3)


stargazer(est1, est2, est3,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_1.doc"
)


## Question 1.j

est_rape <- lm(
    lnrapeN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_rape)

est_sa <- lm(
    lnsexassaultN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_sa)

est_rs <- lm(
    lnrapesexaN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_rs)


stargazer(est_rape, est_sa, est_rs,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_2.doc"
)

## Question 1.k

df <- df %>%
    group_by(city) %>%
    mutate(post = ifelse(year >= 2002, 1, 0)) %>%
    ungroup()

view(df)

## Question 1.l
est1 <- lm(
    lnmaltreatN ~ treat + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)


est_post_1 <- lm(
    lnmaltreatN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_1)

est_post_2 <- lm(
    lnweaponsN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_2)

est_post_3 <- lm(
    lndrugsN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_3)


stargazer(est_post_1, est_post_2, est_post_3,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_post_1.doc"
)


## Question 1.m

est_post_4 <- lm(
    lnrapeN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_4)

est_post_5 <- lm(
    lnsexassaultN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_5)

est_post_6 <- lm(
    lnrapesexaN ~ post + logpopmale1565 + logpopdens + inkhh +
        nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 + mayorVVD +
        mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_post_6)


stargazer(est_post_4, est_post_5, est_post_6,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_post_2.doc"
)

## Question 1.p

df$dif <- df$post * df$treat

est_dif_1 <- lm(
    lnmaltreatN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_1)

est_dif_2 <- lm(
    lnweaponsN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_2)

est_dif_3 <- lm(
    lndrugsN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_3)

stargazer(est_dif_1, est_dif_2, est_dif_3,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_dif_1.doc"
)


## Question 1.q

est_dif_4 <- lm(
    lnrapeN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_4)

est_dif_5 <- lm(
    lnsexassaultN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_5)

est_dif_6 <- lm(
    lnrapesexaN ~ post + treat + dif + logpopmale1565 + logpopdens +
        inkhh + nondutchpc + insurWWAO + educhpc + mayorCU + mayorD66 +
        mayorVVD + mayorCDA + mayorLib + mayorChr + mayorSoc,
    data = df
)

summary(est_dif_6)

stargazer(est_dif_4, est_dif_5, est_dif_6,
    type = "html",
    omit = c("mayor"),
    out = "output\\est_dif_2.doc"
)

## Question 1.S

# Adjust year to be relative to 2002
df <- df %>%
  mutate(year_since_regulation = year - 2002)  # Create new column

# Summarize data by year_since_regulation and treatment group
df_summary <- df %>%
  group_by(year_since_regulation, treat) %>%
  summarize(mean_lndrugsN = mean(lndrugsN, na.rm = TRUE))

# Create the plot with the new adjusted years
ggplot(df_summary, aes(
    x = year_since_regulation, y = mean_lndrugsN, group = factor(treat),
    color = factor(treat), linetype = factor(treat)
)) +
  geom_line(size = 1) + # Line graph
  geom_point(size = 2) + # Points on the line
  geom_vline(xintercept = 0, color = "red") +
  labs(
    title = "Inspecting the parallel trends assumption",
    x = "Years since regulation started",
    y = "log(trafficking drugs crimes)",
    color = "Treated cities",
    linetype = "Treated cities"
  ) +
  ylim(4, 6.125) +
  theme_minimal() +
  scale_color_manual(values = c("lightblue", "blue")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  coord_fixed(ratio = 6)

# Summarize data by year_since_regulation and treatment group
df_summary_2 <- df %>%
  group_by(year_since_regulation, treat) %>%
  summarize(mean_lnweaponsN = mean(lnweaponsN, na.rm = TRUE))

# Create the plot with the new adjusted years
ggplot(df_summary_2, aes(
    x = year_since_regulation, y = mean_lnweaponsN, group = factor(treat),
    color = factor(treat), linetype = factor(treat)
)) +
  geom_line(size = 1) + # Line graph
  geom_point(size = 2) + # Points on the line
  geom_vline(xintercept = 0, color = "red") +
  labs(
    title = "Inspecting the parallel trends assumption",
    x = "Years since regulation started",
    y = "log(trafficking weapons crimes)",
    color = "Treated cities",
    linetype = "Treated cities"
  ) +
  ylim(3.25, 5.125) +
  theme_minimal() +
  scale_color_manual(values = c("lightblue", "blue")) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  coord_fixed(ratio = 6)
