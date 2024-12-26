library(tidyr)
library(readxl)
library(dplyr)
library(fixest)
library(modelsummary)
library(corrplot)
library(texreg)
library(ggplot2)
library(plm)

# ETL data process



dt_hiv <- read_excel("Dataset_HIV.xlsx")
dt_hiv <- unite(dt_hiv, "Indicator name, unit and code", indicator_name, indicator_unit, indicator_code, sep = "_") 
dt_hiv <- dt_hiv %>% select(-c(indicator_section, comment, source, ...14, ...15, object_oktmo, object_okato, reason_na))
dt_hiv <- filter(dt_hiv, object_level == "регион")
dt_hiv <- dt_hiv %>% select(-c(object_level))
dt_hiv_piv <- pivot_wider(dt_hiv, names_from = "Indicator name, unit and code", values_from = indicator_value, values_fn = list)

#**в данном источнике наблюдалась неполнота данных**
dt_hiv_piv <- dt_hiv_piv %>% select(-c("Выявлено за год (всего)_человек_Y904000003"))

#**в данном источнике наблюдалась неполнота данных**
dt_hiv_piv <- dt_hiv_piv %>% select(-c("Выявлено за год (всего)_на 100 тыс. населения_Y904000004"))

#**в данном источнике наблюдалась неполнота данных**
dt_hiv_piv <- dt_hiv_piv %>% select(-c("Выявлено новых случаев ВИЧ на 100 тысяч обследованных_на 100 тыс. населения_Y904000005"))

#**является суммой двух переменных, так как меняются стандарты отчётности**
dt_hiv_piv$`num_ART` <- rowSums(dt_hiv_piv[, c("Число пациентов, получавших антиретровирусную терапию (АРВ) в течение года_человек_Q304000012", "Число пациентов, получавших антиретровирусную терапию (АРВ) на конец года_человек_Q304000023")], na.rm = TRUE)
dt_hiv_piv <- dt_hiv_piv %>% select(-c(`Число пациентов, получавших антиретровирусную терапию (АРВ) в течение года_человек_Q304000012`))
dt_hiv_piv <- dt_hiv_piv %>% select(-c(`Число пациентов, получавших антиретровирусную терапию (АРВ) на конец года_человек_Q304000023`))

## Fix type of values 

#**Identify list columns**
list_cols <- sapply(dt_hiv_piv, is.list)

#**Function to handle NULL and standardize list elements**
standardize_length <- function(col) {
  max_length <- max(sapply(col, function(x) if (is.null(x)) 0 else length(x))) # Determine max length
  lapply(col, function(x) {
    if (is.null(x)) {
      # Replace NULL with NA of appropriate length
      return(rep(NA, max_length))
    }
    # Pad or truncate elements
    length(x) <- max_length
    return(x)
  })
}

#**Standardize list columns**
dt_hiv_piv[list_cols] <- lapply(dt_hiv_piv[list_cols], function(col) {
  standardized_col <- standardize_length(col)  # Standardize lengths
  do.call(rbind, standardized_col)            # Combine into a matrix
})

#**Convert to numeric**
dt_hiv_piv[list_cols] <- lapply(dt_hiv_piv[list_cols], function(col) as.numeric(unlist(col)))

names(dt_hiv_piv)[1] <- "region"
names(dt_hiv_piv)[2] <- "year"
names(dt_hiv_piv)[3] <- "num_HIV" 
names(dt_hiv_piv)[4] <- "num_HIV_per100k_pop"
names(dt_hiv_piv)[5] <- "new_HIV"
names(dt_hiv_piv)[6] <- "new_HIV_per100k_pop"
names(dt_hiv_piv)[7] <- "num_HIV_child_from_HIV_mother"
names(dt_hiv_piv)[8] <- "num_alive_child_from_HIV_mother"
names(dt_hiv_piv)[9] <- "num_compl_pregn_women_tested_HIV"
names(dt_hiv_piv)[10] <- "num_compl_pregn_women_with_HIV_ab"
names(dt_hiv_piv)[11] <- "num_HIV_death"
names(dt_hiv_piv)[12] <- "num_HIV_death_per100k_pop"
names(dt_hiv_piv)[13] <- "num_HIV_prisoners"
names(dt_hiv_piv)[14] <- "prc_HIV_prisoners_from_all_prisoners"
names(dt_hiv_piv)[15] <- "num_inf_dis_doc"
names(dt_hiv_piv)[16] <- "num_inf_dis_doc_per100k_pop"
names(dt_hiv_piv)[17] <- "num_tested_HIV_ab"
names(dt_hiv_piv)[18] <- "prc_tested_HIV_ab_from_pop"
names(dt_hiv_piv)[19] <- "num_registered_dispensary_obs"
names(dt_hiv_piv)[20] <- "num_ART_purchased"
names(dt_hiv_piv)[21] <- "num_HIV_tested_viral_load"
names(dt_hiv_piv)[22] <- "num_HIV_tested_CD4"
names(dt_hiv_piv)[23] <- "num_HIV_tested_ART_resist"
names(dt_hiv_piv)[24] <- "num_HIV_with_undetect_viral_load"
names(dt_hiv_piv)[25] <- "prc_HIV_with_undetect_viral_load_from_num_ART"
names(dt_hiv_piv)[26] <- "reports_supply_disruption"
names(dt_hiv_piv)[27] <- "num_removed_dispensary_obs_dtd"
names(dt_hiv_piv)[28] <- "num_new_HIV_per100k_tested_per100k_pop"
names(dt_hiv_piv)[29] <- "num_new_HIV_men"
names(dt_hiv_piv)[30] <- "num_new_HIV_women"
names(dt_hiv_piv)[31] <- "prc_HIV_registered_fed_regist_from_HIV"

dt_hiv_piv$num_HIV <- round(dt_hiv_piv$num_HIV, 0)
dt_hiv_piv <- mutate(dt_hiv_piv, population = 100000 * `num_HIV_death` / `num_HIV_death_per100k_pop`)
dt_hiv_piv <- mutate(dt_hiv_piv, `prc_ART_from_HIV_pop` = `num_ART` / num_HIV)
dt_hiv_piv <- mutate(dt_hiv_piv, `prc_HIV_with_undetect_viral_load_from_HIV_pop` = `num_HIV_with_undetect_viral_load` / num_HIV)
dt_hiv_piv <- mutate(dt_hiv_piv, `num_inf_dis_doc_per_HIV_pop` = num_inf_dis_doc / num_HIV)
dt_hiv_piv <- mutate(dt_hiv_piv, cross_effect_x1x2 = prc_ART_from_HIV_pop * num_inf_dis_doc_per_HIV_pop)

final_HIV <- dt_hiv_piv[, c("region", "year", "num_HIV", "num_inf_dis_doc_per_HIV_pop", "prc_HIV_with_undetect_viral_load_from_HIV_pop", "prc_ART_from_HIV_pop", 
                            "reports_supply_disruption", "num_HIV_tested_viral_load", "num_tested_HIV_ab", "cross_effect_x1x2")]
final_HIV$region <- factor(final_HIV$region)


#Problem of missing values for certain years



na_analysis_1 <- dt_hiv_piv %>%
  select(year, num_HIV, num_inf_dis_doc_per_HIV_pop,
         prc_HIV_with_undetect_viral_load_from_HIV_pop, 
         num_HIV_tested_viral_load) %>%
  pivot_longer(-year, names_to = "Variable", values_to = "Value") %>%
  filter(is.na(Value)) %>%
  group_by(Variable, year) %>%
  summarise(na_count = n(), .groups = "drop")

# Graph constructing
ggplot(na_analysis_1, aes(x = Variable, y = na_count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") + # position = "dodge" разделяет столбцы по годам
  labs(
    title = "Number of NA values",
    x = "Value",
    y = "Number of NA",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13.1),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 15),  
    legend.title = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top" # Положение легенды сверху
  )

na_analysis_2 <- dt_hiv_piv %>%
  select(year, prc_ART_from_HIV_pop, reports_supply_disruption, 
         cross_effect_x1x2, num_tested_HIV_ab) %>%
  pivot_longer(-year, names_to = "Variable", values_to = "Value") %>%
  filter(is.na(Value)) %>%
  group_by(Variable, year) %>%
  summarise(na_count = n(), .groups = "drop")

# Graph constructing
ggplot(na_analysis_2, aes(x = Variable, y = na_count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") + # position = "dodge" разделяет столбцы по годам
  labs(
    x = "Value",
    y = "Number of NA",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13.1),
    legend.position = "none",
    axis.title.y = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )


final_HIV <- filter(final_HIV, year == 2018 | year == 2019 |year == 2020 |year == 2021 )
final_HIV <- final_HIV %>%
  filter(!(region == "Архангельская область" | region == "Санкт-Петербург и Ленинградская область" | region == "Республика Крым и Севастополь" | (region == "Красноярский край" & year == 2021)))
final_HIV <- panel(final_HIV, ~ region + year)



#Explanatory data analysis



##Descriptive statistics

datasummary(prc_HIV_with_undetect_viral_load_from_HIV_pop + year + num_HIV + num_inf_dis_doc_per_HIV_pop + prc_ART_from_HIV_pop + reports_supply_disruption + 
            num_HIV_tested_viral_load + num_tested_HIV_ab + cross_effect_x1x2 ~ N + Mean + SD + Min + Max, data = final_HIV)

##Visualization

###PDF function of dependent variable
mean_value <- mean(final_HIV$prc_HIV_with_undetect_viral_load_from_HIV_pop, na.rm = TRUE)
n_elements <- sum(!is.na(final_HIV$prc_HIV_with_undetect_viral_load_from_HIV_pop))
sd_value <- sd(final_HIV$prc_HIV_with_undetect_viral_load_from_HIV_pop, na.rm = TRUE)

subtitle <- paste("Mean:", round(mean_value, 2), 
                  " N:", n_elements, 
                  " SD:", round(sd_value, 2))

ggplot(final_HIV, aes(x = prc_HIV_with_undetect_viral_load_from_HIV_pop)) +
  geom_density(na.rm = TRUE, fill = "blue", alpha = 0.6) +
  labs(y = "Density",
       x = "Percentage of HIV+ people with UVL from all HIV population",
       subtitle = subtitle) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 12, hjust = 0.5))

###Scatter plots
ggplot(data = final_HIV, aes(x = prc_ART_from_HIV_pop , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point() 
ggplot(data = final_HIV, aes(x = reports_supply_disruption , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point()
ggplot(data = final_HIV, aes(x = num_inf_dis_doc_per_HIV_pop , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point()
ggplot(data = final_HIV, aes(x = cross_effect_x1x2 , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point() 

ggplot(data = final_HIV, aes(x = num_HIV , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point()
ggplot(data = final_HIV, aes(x = num_HIV_tested_viral_load , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point()
ggplot(data = final_HIV, aes(x = num_tested_HIV_ab , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point()

###Box plots
boxplot(final_HIV$prc_HIV_with_undetect_viral_load_from_HIV_pop, names=c('Percentage of patients with UVL from all HIV+ population'), show.names=TRUE)
boxplot(final_HIV$num_HIV, names=c('Number of people living with HIV'), show.names=TRUE)
boxplot(final_HIV$num_inf_dis_doc_per_HIV_pop, names=c('Number of infectious disease doctors per all HIV'), show.names=TRUE)
boxplot(final_HIV$prc_ART_from_HIV_pop, names=c('Percentage of people on ART from all HIV+ population '), show.names=TRUE)
boxplot(final_HIV$reports_supply_disruption, names=c('Reports of supply disruptions'), show.names=TRUE)
boxplot(final_HIV$num_HIV_tested_viral_load, names=c('Number of people with HIV tested for viral load during the year'), show.names=TRUE)
boxplot(final_HIV$cross_effect_x1x2, names=c('Cross effect between percentage of ART and number of doctors'), show.names=TRUE)



#Intelligence data analysis



cor_matrix <- cor(final_HIV[, -1], use = "complete.obs")
colnames(cor_matrix)[colnames(cor_matrix) == "prc_HIV_with_undetect_viral_load_from_HIV_pop"] <- "prc_HIV_with_UVL_from_HIV_pop"
rownames(cor_matrix)[rownames(cor_matrix) == "prc_HIV_with_undetect_viral_load_from_HIV_pop"] <- "prc_HIV_with_UVL_from_HIV_pop"
corrplot(cor_matrix, method = "color", 
         tl.col = "black", 
         tl.srt = 90, 
         addgrid.col = "grey",
         mar = c(0, 0, 1, 0))



#Regression analysis



##Baseline model

ols_1 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption, data = final_HIV, vcov = ~region)
summary(ols_1)

screenreg(list(ols_1), 
          digits = 5, 
          stars = c(0.001, 0.01, 0.05, 0.1),
          custom.gof.rows = list("F-statistic" = c(fitstat(ols_1, type = ~ f)$f$stat), "p-value for F-statistic" = c(fitstat(ols_1, type = ~ f)$f$p)))

##Regression with control variables

ols_2 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_HIV), data = final_HIV, vcov = ~region)
summary(ols_2)

ols_3 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab), data = final_HIV, vcov = ~region)
summary(ols_3)

ols_4 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_HIV_tested_viral_load), data = final_HIV, vcov = ~region)
summary(ols_4)

ols_5 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab) + log(num_HIV_tested_viral_load), data = final_HIV, vcov = ~region)
summary(ols_5)

ols_6 <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab) + log(num_HIV_tested_viral_load) + log(num_HIV), data = final_HIV, vcov = ~region)
summary(ols_6)

screenreg(list(ols_2, ols_3, ols_4), 
          digits = 5, 
          stars = c(0.001, 0.01, 0.05, 0.1), 
          custom.model.names = c("Model 2", "Model 3", "Model 4"),
          custom.gof.rows = list("F-statistic" = c(fitstat(ols_2, type = ~ f)$f$stat, fitstat(ols_3, type = ~ f)$f$stat, fitstat(ols_4, type = ~ f)$f$stat), 
                                 "p-value for F-statistic" = c(fitstat(ols_2, type = ~ f)$f$p, fitstat(ols_3, type = ~ f)$f$p, fitstat(ols_4, type = ~ f)$f$p))) 
screenreg(list(ols_5, ols_6), 
          digits = 5, 
          stars = c(0.001, 0.01, 0.05, 0.1), 
          custom.model.names = c("Model 5", "Model 6"),
          custom.gof.rows = list("F-statistic" = c(fitstat(ols_5, type = ~ f)$f$stat, fitstat(ols_6, type = ~ f)$f$stat), 
                                 "p-value for F-statistic" = c(fitstat(ols_5, type = ~ f)$f$p, fitstat(ols_6, type = ~ f)$f$p))) 

##Regression with individual (state) fixed effects

ols_4_ife <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_HIV_tested_viral_load) | region, data = final_HIV, vcov = ~region)
summary(ols_4_ife)

ols_5_ife <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab) + log(num_HIV_tested_viral_load) | region, data = final_HIV, vcov = ~region)
summary(ols_5_ife)

ols_6_ife <- feols(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab) + log(num_HIV_tested_viral_load) + log(num_HIV) | region, data = final_HIV, vcov = ~region)
summary(ols_6_ife)

screenreg(list(ols_4_ife, ols_5_ife, ols_6_ife), 
          digits = 5, 
          stars = c(0.001, 0.01, 0.05, 0.1), 
          custom.model.names = c("Model 7", "Model 8", "Model 9"),
          custom.gof.rows = list("F-statistic" = c(fitstat(ols_4_ife, type = ~ f)$f$stat, fitstat(ols_5_ife, type = ~ f)$f$stat, fitstat(ols_6_ife, type = ~ f)$f$stat), 
                                 "p-value for F-statistic" = c(fitstat(ols_4_ife, type = ~ f)$f$p, fitstat(ols_5_ife, type = ~ f)$f$p, fitstat(ols_6_ife, type = ~ f)$f$p)))



# Analysis of results



fixedEffects_6 <- fixef(ols_6_ife)
summary(fixedEffects_6)
plot(fixedEffects_6$region)

## Wald Test for ols_1
wald_test_ols_1 <- wald(
  ols_1, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption")
)

print(wald_test_ols_1)

## Wald Test for ols_2
wald_test_ols_2 <- wald(
  ols_2, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_HIV)")
)

print(wald_test_ols_2)

## Wald Test for ols_3
wald_test_ols_3 <- wald(ols_3, c("prc_ART_from_HIV_pop", 
                                 "num_inf_dis_doc_per_HIV_pop", 
                                 "cross_effect_x1x2", 
                                 "reports_supply_disruption", 
                                 "log(num_tested_HIV_ab)"))

print(wald_test_ols_3)

## Wald Test for ols_4
wald_test_ols_4 <- wald(
  ols_4, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_HIV_tested_viral_load)")
)

print(wald_test_ols_4)

## Wald Test for ols_5
wald_test_ols_5 <- wald(
  ols_5, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_tested_HIV_ab)", 
    "log(num_HIV_tested_viral_load)")
)

print(wald_test_ols_5)

## Wald Test for ols_6
wald_test_ols_6 <- wald(
  ols_6, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_tested_HIV_ab)", 
    "log(num_HIV_tested_viral_load)", 
    "log(num_HIV)")
)

print(wald_test_ols_6)

## Wald Test for ols_4_ife
wald_test_ols_4_ife <- wald(
  ols_4_ife, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_HIV_tested_viral_load)")
)
print(wald_test_ols_4_ife)

## Wald Test for ols_5_ife
wald_test_ols_5_ife <- wald(
  ols_5_ife, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_tested_HIV_ab)", 
    "log(num_HIV_tested_viral_load)")
)
print(wald_test_ols_5_ife)

## Wald Test for ols_6_ife
wald_test_ols_6_ife <- wald(
  ols_6_ife, 
  c("prc_ART_from_HIV_pop", 
    "num_inf_dis_doc_per_HIV_pop", 
    "cross_effect_x1x2", 
    "reports_supply_disruption", 
    "log(num_tested_HIV_ab)", 
    "log(num_HIV_tested_viral_load)", 
    "log(num_HIV)")
)
print(wald_test_ols_6_ife)

##Hausman test

ols_6_re <- plm(prc_HIV_with_undetect_viral_load_from_HIV_pop ~ prc_ART_from_HIV_pop + num_inf_dis_doc_per_HIV_pop + cross_effect_x1x2 + reports_supply_disruption + log(num_tested_HIV_ab) + log(num_HIV_tested_viral_load) + log(num_HIV) , data = final_HIV, index = c("region", "year"), model = "random")
summary(ols_6_re)

hausman_test <- phtest(ols_6_ife, ols_6_re)
print(hausman_test)




#Possible expansions



ggplot(data = final_HIV, aes(x = cross_effect_x1x2 , y = prc_HIV_with_undetect_viral_load_from_HIV_pop)) + geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 2) ,se = FALSE, color = "blue")
