# ------ DHS data for Tanzania-----


library(haven)
library(tidyverse)
library(expss)
library(gtsummary)
library(gt)
library(cards)
library(psych)
library(labelled)
library(survey)
library(dplyr)


# ---- load the file-----


mydata <- read_dta("D:/R for public health/Files/Tanzania/TZ_2022_DHS_01292026_1346_240008/TZKR82DT/TZKR82FL.DTA")

view(mydata)




hh <- mydata %>%
  select(v012, v013, v025, v106, v190) %>%
  mutate(
    v013 = as_factor(v013),
    v025 = as_factor(v025),
    v106 = as_factor(v106),
    v190 = as_factor(v190),
    v012 = as.numeric(v012)
  )

# -----------Short summary of the demographics---------
tbl_summary(hh)





# ================ Child malnutrition -----------------------

nutri <- mydata %>%
  select(caseid, bidx, v005, v021, v023, v024, v025, v012, v106, v190, m14, v445, b4, hw1, hw70, hw71, hw72) %>%
  mutate(
    v024 = as_factor(v024),
    v025 = as_factor(v025),
    v106 = as_factor(v106),
    v190 = as_factor(v190),
    b4 = as_factor(b4),
    v005 = as.numeric(v005),
    v021 = as.numeric(v021),
    v023 = as.numeric(v023),
    v012 = as.numeric(v012),
    v445 = as.numeric(v445),
    hw1 = as.numeric(hw1),
    hw70 = as.numeric(hw70),
    hw71 = as.numeric(hw71),
    hw72 = as.numeric(hw72),
    swt = v005/1000000,
    bmi = ifelse(v445 > 9000, NA, v445/100),
    haz = ifelse(hw70 > 9000, NA, hw70/100),
    waz = ifelse(hw71 > 9000, NA, hw71/100),
    whz = ifelse(hw72 > 9000, NA, hw72/100),
    anc = ifelse(m14 == 98, NA, m14),
    stunting = ifelse(haz < -2, 1, 0),
    underweight = ifelse(waz < -2 , 1 , 0),
    wasting = ifelse(whz < -2, 1, 0),
    bmi_cat = ifelse(bmi < 18.5, 1, 0),
    anc4 = ifelse(anc >= 4, 1, 0)
  ) %>%
  select(caseid, bidx, swt, v021, v023, v024, v025, v012, v106, v190, bmi, bmi_cat, anc, anc4, haz, waz, whz, stunting, underweight, wasting, b4, hw1) %>%
  mutate(
    v025 = factor(v025, labels = c('Urban', 'Rural'),exclude = NA),
    v106 = factor(v106, labels = c('No education', 'Primary','Secondary','Higher'), exclude = NA),
    v190 = factor(v190, labels = c('Poorest','Poorer','Middle','Richer','Richest'), exclude = NA),
    b4 = factor(b4, labels = c('Male','Female'), exclude = NA)
  ) %>%
  rename(
    psu = v021,
    stratum = v023,
    region = v024,
    Urbanrural = v025,
    mage = v012,
    medu = v106,
    wealth = v190,
    csex = b4,
    cage = hw1
  )

var_label(nutri) <- list(
  swt = "Sampling Weight",
  psu = "Cluster/ Primary sampling unit",
  stratum = "Strata",
  region = "Region of the country",
  Urbanrural = "Place of residence",
  mage = "Maternal age",
  medu = "Maternal education",
  wealth = "Wealth index",
  bmi = "Body mass index",
  bmi_cat = "maternal underweight",
  anc = "Antenatal care",
  anc4 = "At least 4 ANC",
  haz = "Height-for-age z score",
  waz = "Weight-for-age z score",
  whz = "weight-for-age z score",
  stunting = "Childhood stunting",
  underweight = "Childhood underweight",
  wasting = "childhood wasting",
  csex = "child sex",
  cage = "child's cage"
)


nutri %>%
  tbl_summary(
    include = -c(caseid, bidx,swt, psu, stratum, region),
    missing = 'no'
  ) %>%
  bold_labels()

  )


save(nutri, file =  "D:/R for public health/DHS data analysis for tanzania/nutri.Rdata")

#--------------Univariate descriptive analysis

nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage) %>%
  tbl_summary(
    missing = 'no'
  )

# --------- Format categorical variables as n(%)

nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage) %>%
  tbl_summary(
    missing = 'no',
  statistic = list(
    all_categorical() ~ '{n} ({p})'
  )
  )


# ------- Format continuouls variables as mean and standard deviation

nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage) %>%
  tbl_summary(
    missing = 'no',
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ '{mean}' +- '{sd}'
    )
  )


# ---------- Set the decimals

nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage) %>%
  tbl_summary(
    missing = 'no',
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ "{mean} +- {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0,2),
      all_continuous() ~ c(2,2)
    
    )
  )

#------- Bivariate analysis


nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage,csex,wealth,bmi_cat,anc4,mage,medu,haz,waz,whz) %>%
  tbl_summary(
    missing = 'no',
    by = Urbanrural,
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ "{mean} +- {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0,2),
      all_continuous() ~ c(2,2)
      
    )
  ) %>%
  add_overall(last = TRUE) %>%
   bold_labels()
  )            




# ----------------- Adding p-value


Tab_1 <- nutri %>%
  select(stunting,underweight,wasting,Urbanrural,bmi,cage,csex,wealth,bmi_cat,anc4,mage,medu,haz,waz,whz) %>%
  tbl_summary(
    missing = 'no',
    by = Urbanrural,
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ "{mean} +- {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0,2),
      all_continuous() ~ c(2,2)
      
    )
  ) %>%
  add_overall(last = TRUE) %>%
  bold_labels() %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      all_continuous() ~ "t.test"
  ),
  pvalue_fun = ~ style_pvalue(.x, digits =3)
  )


#----------------- Save the table and export to MS word

print(Tab_1)

Tab_1 %>%
  as_gt() %>%
  gtsave(
    filename = "Table_1.docx",
    path = "D:/R for public health/DHS data analysis for tanzania"
  )





#----------------- Survey set design


svy_dat  <- survey::svydesign(
  id =~ psu,
  strata =~ stratum,
  weights =~ swt,
  data = nutri,
  nest = TRUE
)

# -------- Univariate of svy 

svy_dat %>%
  tbl_svysummary(
    missing = "no",
    include = c(stunting, underweight, wasting, Urbanrural, bmi, csex, cage)
  ) 
   


#-------------- Define statistics n(%) --- mean +- (sd)


svy_dat %>%
  tbl_svysummary(
    missing = "no",
    include = c(stunting, underweight, wasting, Urbanrural, bmi, csex, cage),
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ "{mean} +- {sd}"
    ),
  digits = list(
    all_categorical() ~ c(0,2),
    all_continuous() ~ c(2,2)
  )
  ) %>%
  add_overall(last= TRUE)

#----------------- p-value and chi square test

svy_dat %>%
  tbl_svysummary(
    missing = "no",
    by = Urbanrural,
    include = c(stunting, underweight, wasting, Urbanrural, bmi, csex, cage),
    statistic = list(
      all_categorical() ~ '{n} ({p})',
      all_continuous() ~ "{mean} +- {sd}"
    ),
    digits = list(
      all_categorical() ~ c(0,2),
      all_continuous() ~ c(2,2)
    )
  ) %>%
  add_overall(last= TRUE) %>%
  bold_labels() %>%
  add_p(
    test = list(
      all_categorical() ~ "svy.chisq.test",
      all_continuous() ~ "svy.t.test"
    ),
    pvalue_fun =~ style_pvalue(.x, digits = 3)
  )



# --------------- Bar Diagram for 1 categorical variabels

A1 <- nutri %>%
  select(medu) %>%
  drop_na() %>%
  count(medu) %>%
  mutate(
    p = n/sum(n)*100
  )
A1 %>%
  ggplot(aes(x=medu, y = p)) + 
  geom_bar(
    stat = 'identity',
    fill = 'grey',
    width = 0.5
  )

# customisation and labelling 



A1 <- nutri %>%
  select(medu) %>%
  drop_na() %>%
  count(medu) %>%
  mutate(
    p = n/sum(n)*100
  )
A1 %>%
  ggplot(aes(x=medu, y = p)) + 
  geom_bar(
    stat = 'identity',
    fill = 'grey',
    width = 0.5
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%",p)),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    x = "Maternal Education",
    y = "Percentage"
  ) +
  expand_limits(y = max(A1$p)+5)+
  theme_classic()+
  theme(
    axis.title = element_text(
      size = 12,
      face = "bold"
    )
  )


# --------------Bar diagram of 2 categorical varaibles

A2 <- nutri %>%
  select(medu, Urbanrural) %>%
  drop_na() %>%
  count(medu, Urbanrural) %>%
  group_by(Urbanrural) %>%
  mutate(
    p = n/sum(n)*100
  ) %>%
  ungroup()

A2 %>%
  ggplot(aes(x = medu,  y = p, fill = Urbanrural)) + geom_col(
    position = position_dodge(width = 0.7),
    width = 0.55
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%",p)),
    position = position_dodge(width = 0.7),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    x = "Maternal education",
    y= "Percentage",
    fill = "Place of residence"
  ) +
  theme_classic()+
  theme(
    legend.position = "top"
  )+
  expand_limits(y = max(A2$p)+3)



#----------------- Bar diagram for Binary Outcome

B1 <- nutri %>%
  select(stunting, wealth) %>%
  drop_na() %>%
  group_by(wealth) %>%
  summarise(
    p = mean(stunting)*100,
    .groups = "drop"
  )

B1 %>%
  ggplot(aes(x=wealth, y=p))+
  geom_bar(
    stat = "identity",
    fill ="skyblue"
  )+
  labs(
    x= "Wealth index",
    y = "Percentage"
  ) + 
  geom_text(
    aes(label = paste0(round(p,1))),
    position = position_dodge(width = 0.95),
    size = 4,
    vjust = -0.5
  )



#---------------------- One binary outcome by two group variables

B2 <- nutri %>%
  select(stunting,wealth,Urbanrural) %>%
  drop_na() %>%
  group_by(wealth,Urbanrural) %>%
  summarise(
    p = mean(stunting)*100,
    .groups = "drop"
  )

B2 %>%
  ggplot(aes(x= wealth, y = p, fill = Urbanrural))+
  geom_col(
    position = position_dodge(width = 0.70),
    width = 0.60,
    color = "white"
  ) +
  labs(
    x = "Wealth index",
    y = "Percentage of stunting",
    fill = "Place of residence"
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%",p)),
    position = position_dodge(width = 0.70),
    size = 4,
    vjust = -0.5
  ) +
  theme_minimal() +
  theme(
    legend.position = "top"
  )+
  expand_limits(y=max(B2$p)+5) +
  theme_classic()


#--------- multople binary outcome: stunding, wasting, underweight-----

B3 <- nutri %>%
  select(stunting, underweight, wasting, wealth) %>%
  drop_na() %>%
  pivot_longer(
    cols = c(stunting, underweight, wasting),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  group_by(wealth, outcome) %>%
  summarise(
    p=mean(as.numeric(value))*100,
    .groups = "drop"
  ) %>%
  mutate(
    outcome = dplyr::recode(outcome,
    stunting = "Stunting",
    underweight = "Underweight",
    wasting = " Wasting")
  )
B3 %>%
  ggplot(aes(x=wealth, y=p, fill = outcome))+
  geom_col(
    position = position_dodge(width = 0.7),
                              color = "white",
                              width = 0.6
       ) +
      geom_text(
        aes(label = sprintf("%.1f%%",p)),
        position = position_dodge(width = 0.7),
        vjust = -0.4,
        size = 3
      )+
  labs(
    x= "Wealth index",
    y= "Percerntage",
    fill= "Outcomes"
  )+
  theme_minimal()+ 
  theme(
    legend.position = "top"
  )+
  expand_limits(y=max(B3$p)+5)
   



#---------------- Data analysis Box plot


nutri %>%
  select(bmi) %>%
  drop_na() %>%
  
  ggplot(aes(y=bmi))+
  geom_boxplot(
    fill= "skyblue",
    outlier.color = "red"
    )+
  labs(
    y = "Maternal BMI"
  ) +
  theme_classic()+
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
      )


#---------------Box plot by categorical variable

nutri %>%
  select(bmi, wealth) %>%
  drop_na() %>%
  ggplot(aes(x=wealth, y=bmi))+
  geom_boxplot(
    fill = "skyblue",
    outlier.color = "red"
  )+
  labs(
    x= "Wealth index",
    y= "Maternal BMI"
  )+
  theme_classic()



#------------ Boxplot of two categorical variable 

nutri %>%
  select(bmi,wealth,Urbanrural) %>%
  drop_na() %>%
  ggplot(aes(x=Urbanrural, y =bmi, fill =wealth)) +
  geom_boxplot(
    outliers = TRUE,
    outlier.color = "red",
    outlier.size = 2,
    notch = TRUE
  ) +
  labs(
    x= "Place of residence",
    y = "Maternal BMI",
    fill = "Wealth Index"
  ) +
  scale_fill_brewer(palette = "set1")+
  theme_classic() +
    theme(
      axis.title = element_text (
        hjust = 0.5,
        size = 12,
        face = "bold"
      ),
      legend.position = "top"
    )



#-----------------Density plot

nutri %>%
  select(bmi) %>%
  drop_na() %>%
  ggplot(aes(x=bmi))+
  geom_density(
  color = "red",
  fill = "skyblue" 
 )+
  labs(
    x="Respondent's BMI"
  )


#------------------ Histogram------

nutri %>%
  select(bmi) %>%
  drop_na() %>%
  ggplot(aes(x=bmi))+
  geom_histogram(
    fill= "skyblue",
    color = "black",
    binwidth = 1,
    aes(y= after_stat(density))
)+
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(nutri$bmi, na.rm = TRUE),
      sd = sd(nutri$bmi, na.rm = TRUE)
    ),
    color = "red"
  )+
  labs(
    x= "Respondent;s BMI",
    y= "Density"
  )
 




#---------------Simple Logistic regression

install.packages("")
glm(
  stunting ~ Urbanrural,
  family = binomial(link = "logit"),
  data = nutri
) %>%
  tbl_regression(
    exponentiate = TRUE
  )



#---------- MUltiple logistic Regression


glm(
  stunting ~ Urbanrural + csex + cage + mage + wealth,
  family = binomial(link = "logit"),
  data = nutri
) %>%
  tbl_regression(
    exponentiate = TRUE
  )



#--------- Variable modification------ change reference------

nutri <- nutri %>%
  mutate(
    csex = relevel(csex, ref = "Female"),
    wealth = relevel(wealth, ref = "Richest")
  )


#------- Unadjusted odds ratio

OR1 <- nutri %>%
  select(stunting, csex, cage, mage, wealth, bmi_cat) %>%
  tbl_uvregression(
  method = glm,
  y = stunting,
  method.args = list(family=binomial(link= "logit")),
  exponentiate = TRUE,
  pvalue_fun =~ style_pvalue(.x,digits = 3)
) %>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate~ "**OR(95% CI)**") %>%
  bold_labels()
print(OR1)



#---------- Adjusted odds ratio, multiple regression 

OR2 <- glm(
  stunting ~ csex + cage + mage + wealth + bmi_cat,
  data = nutri,
  family = binomial(link = "logit")
) %>%
  tbl_regression(
    exponentiate =  TRUE,
    pvalue_fun =~ style_pvalue(.x, digits = 3)
  )%>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate~ "**OR(95% CI)**") %>%
  bold_labels()
print(OR2)


#-------------- final table adjusted and unadjusted 



OR_tab <- tbl_merge(
  tbls = list(OR1, OR2),
  tab_spanner = c("**unadjusted**", "**adjusted**")
)
print(OR_tab)





#-------Logistic regression - Binary outcome --- sampling weight ---
glm(
  stunting ~ Urbanrural,
  family = quasibinomial(link = "logit"),
  data = nutri,
  weights = swt
  ) %>%
  tbl_regression(
    exponentiate = TRUE
  )




# ---------- Weighted multiple logistic regression 


glm(
  stunting ~ Urbanrural + csex + cage + mage + wealth,
  family = quasibinomial(link = "logit"),
  data = nutri,
  weights = swt
) %>%
  tbl_regression(
    exponentiate = TRUE
  )



aOR <- glm(
  stunting ~ csex + cage + mage + wealth + bmi_cat + anc4,
  data = nutri,
  family = quasibinomial(link = "logit"),
  weights = swt
) %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = ! is.na(estimate)
  ) %>%
  modify_header(estimate ~ "**aOR (95% CI)**") %>%
  bold_labels()

print(aOR)



#--------------- factors associated with stunting, underweight, and wasting

st <- glm(
  stunting ~ csex + cage + mage + wealth + bmi_cat + anc4,
  data = nutri,
  family = quasibinomial(link = "logit"),
  weights = swt
) %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = ! is.na(estimate)
  ) %>%
  modify_header(estimate ~ "**aOR (95% CI)**") %>%
  bold_labels()
print(st)


und <- glm(
  stunting ~ csex + cage + mage + wealth + bmi_cat + anc4,
  data = nutri,
  family = quasibinomial(link = "logit"),
  weights = swt
) %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = ! is.na(estimate)
  ) %>%
  modify_header(estimate ~ "**aOR (95% CI)**") %>%
  bold_labels()
print(und)

wt <- glm(
  stunting ~ csex + cage + mage + wealth + bmi_cat + anc4,
  data = nutri,
  family = quasibinomial(link = "logit"),
  weights = swt
) %>%
  tbl_regression(
    exponentiate = TRUE
  ) %>%
  modify_column_merge(
    pattern = "{estimate}({conf.low}, {conf.high})",
    rows = ! is.na(estimate)
  ) %>%
  modify_header(estimate ~ "**aOR (95% CI)**") %>%
  bold_labels()
print(wt)

Table_5 <- tbl_merge(
  tbls = list(st,und,wt),
  tab_spanner = c("**stunting**", "**underweight**","**wasting**")
)
print(Table_5)




#---------------Linear regression, continuous outcome


lm(haz~ bmi,
   data = nutri) %>%
  tbl_regression()
  )



#----------- Multiple linear regression

lm(haz~ bmi + csex + cage + Urbanrural + wealth + anc4,
   data = nutri) %>%
  tbl_regression(
  pvalue_fun =~ style_pvalue(.x, digits =3)
) %>%
  bold_labels()






#------------- Survey logistic regression---------

mydesign <- svydesign(
  id =~ psu,
  strata =~ stratum,
  weights =~ swt,
  data = nutri
)

svyglm(
  stunting ~ Urbanrural,
  family = quasibinomial(link = "logit"),
  design = mydesign
) %>%
  tbl_regression(
    exponentiate = TRUE
  )



#------- Multiple survey logistic regression 


svyglm(
  stunting ~ csex+cage+wealth,
  family = quasibinomial(link = "logit"),
  design = mydesign
) %>%
  tbl_regression(
    exponentiate = TRUE
  )


#-------------------- Simple multiple level logistic regression
library(lme4)

glmer(
  stunting ~ Urbanrural +(1|psu),
  family = binomial(link = "logit"),
  data = nutri
) %>%
  tbl_regression(
    exponentiate = TRUE
  )

















