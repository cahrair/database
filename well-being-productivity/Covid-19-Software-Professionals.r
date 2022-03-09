# Data analysis Wave 1

library(lavaan)
library(semPlot)
library(car)
library(foreign)
library(ggplot2)
library(ggstatsplot)
library(gridExtra)
library(DescTools)
library(psychometric)
library(effsize)
library(psych)
# Cohen's U3
u3 <- function(d){pnorm(d)}
PCR <- function(d){2*pnorm((-abs(d))/2)} * 100 # Percentages of Common Responses (measure of similarities/overlap between two groups)

df <- read.spss("AnonymisedData", use.missings=TRUE, use.value.labels = F, to.data.frame = TRUE) # 'use.missings' kicks all the missing values as defined in the SPSS data file out
summary(df$Age)
sd(df$Age, na.rm = T)
table(df$Gender) # 1: Men, 2: Women
summary(df$Duration__in_seconds_)
sd(df$Duration__in_seconds_)


# Internal consistencies and factor creation -----
# Well-being (adapted Satisfaction with Life Scale)
df$Well_being <- rowMeans(df[,21:25], na.rm = TRUE) - 14
alpha(df[,21:25])$total$raw_alpha

# Productivity
df$Productivity <- (df$Productivity_1/df$Productivity_4) * ((df$Productivity_1.1 + 100)/100) 

# Boredom
df$Boredom <- rowMeans(df[,49:56]) - 10
alpha(df[,49:56])$total$raw_alpha

# Compliance
df$Compliance <- rowMeans(df[,57:59])
alpha(df[,57:59])$total$raw_alpha

# Conspiracy beliefs
df$Conspiracy <- rowMeans(df[,60:64]) - 10
alpha(df[,60:64])$total$raw_alpha

#Brief COPE (coping strategies)
df$Self_distraction <- rowMeans(cbind(df$Brief_COPE_1, df$Brief_COPE_19), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_1, df$Brief_COPE_19))$total$raw_alpha
df$Active_coping <- rowMeans(cbind(df$Brief_COPE_2, df$Brief_COPE_7), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_2, df$Brief_COPE_7))$total$raw_alpha
df$Denial <- rowMeans(cbind(df$Brief_COPE_3, df$Brief_COPE_8), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_3, df$Brief_COPE_8))$total$raw_alpha
df$Substance_use <- rowMeans(cbind(df$Brief_COPE_4, df$Brief_COPE_11), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_4, df$Brief_COPE_11))$total$raw_alpha
df$Emotional_support <- rowMeans(cbind(df$Brief_COPE_5, df$Brief_COPE_15), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_5, df$Brief_COPE_15))$total$raw_alpha
df$Instrumental_support <- rowMeans(cbind(df$Brief_COPE_10, df$Brief_COPE_23), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_10, df$Brief_COPE_23))$total$raw_alpha
df$Behavioral_disengagement <- rowMeans(cbind(df$Brief_COPE_6, df$Brief_COPE_16), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_6, df$Brief_COPE_16))$total$raw_alpha
df$Venting <- rowMeans(cbind(df$Brief_COPE_9, df$Brief_COPE_21), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_9, df$Brief_COPE_21))$total$raw_alpha
df$Positive_reframing <- rowMeans(cbind(df$Brief_COPE_12, df$Brief_COPE_17), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_12, df$Brief_COPE_17))$total$raw_alpha
df$Planning <- rowMeans(cbind(df$Brief_COPE_14, df$Brief_COPE_25), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_14, df$Brief_COPE_25))$total$raw_alpha
df$Humor <- rowMeans(cbind(df$Brief_COPE_18, df$Brief_COPE_28), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_18, df$Brief_COPE_28))$total$raw_alpha
df$Acceptance <- rowMeans(cbind(df$Brief_COPE_20, df$Brief_COPE_24), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_20, df$Brief_COPE_24))$total$raw_alpha
df$Religion <- rowMeans(cbind(df$Brief_COPE_22, df$Brief_COPE_27), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_22, df$Brief_COPE_27))$total$raw_alpha
df$Self_blame <- rowMeans(cbind(df$Brief_COPE_13, df$Brief_COPE_26), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_13, df$Brief_COPE_26))$total$raw_alpha

# Need for relatedness
df$psychological_needs_2 <- 6 - df$psychological_needs_2
df$psychological_needs_4 <- 6 - df$psychological_needs_4
df$psychological_needs_6 <- 6 - df$psychological_needs_6
df$Relatedness <- rowMeans(df[,97:102], na.rm = T)
alpha(df[,97:102])$total$raw_alpha

# Need for competence
df$psychological_needs_8 <- 6 - df$psychological_needs_8
df$psychological_needs_10 <- 6 - df$psychological_needs_10
df$psychological_needs_12 <- 6 - df$psychological_needs_12
df$Competence <- rowMeans(df[,103:108], na.rm = T)
alpha(df[,103:108])$total$raw_alpha

# Need for autonomy
df$psychological_needs_14 <- 6 - df$psychological_needs_14
df$psychological_needs_16 <- 6 - df$psychological_needs_16
df$psychological_needs_18 <- 6 - df$psychological_needs_18
df$Autonomy <- rowMeans(df[,109:114], na.rm = T)
alpha(df[,109:114])$total$raw_alpha

# Office set-up
df$Office_setup <- rowMeans(df[,115:117], na.rm = T)
alpha(df[,115:117])$total$raw_alpha

# Communication with colleagues and line managers
df$Communication <- rowMeans(df[,118:120], na.rm = T)
alpha(df[,118:120])$total$raw_alpha

# Stress
df$Stress_2 <- 6 - df$Stress_2
df$Stress_3 <- 6 - df$Stress_3
df$Stress <- rowMeans(df[,122:125], na.rm = T)
alpha(df[,122:125])$total$raw_alpha

# Daily Routines
df$Daily_routines <- rowMeans(df[,126:128], na.rm = T)
alpha(df[,126:128])$total$raw_alpha

# Self-control
df$Self_Control_2 <- 6 - df$Self_Control_2
df$Self_Control_3 <- 6 - df$Self_Control_3
df$Self_Control <- rowMeans(df[,129:131], na.rm = T)
alpha(df[,129:131])$total$raw_alpha

# Volunteering
df$Volunteering <- rowMeans(df[,133:135], na.rm = T)
alpha(df[,133:135])$total$raw_alpha

# Extraversion
df$Extraversion_1 <- 6 - df$Extraversion_1
df$Extraversion_4 <- 6 - df$Extraversion_4
df$Extraversion_now <- rowMeans(df[,136:139], na.rm = T)
alpha(df[,136:139])$total$raw_alpha

# Diet
df$Diet <- rowMeans(df[,144:145], na.rm = T)
alpha(df[,144:145])$total$raw_alpha

# Distractions at home
df$Distractions_at_home_1 <- df$Distractions_at_home_1 - 19
df$Distractions_at_home_2 <- 25 - df$Distractions_at_home_2
df$Distractions <- rowMeans(df[,146:147], na.rm = T)
alpha(df[,146:147])$total$raw_alpha

# Exercising
df$Exercising <- df$Exercising * 3
df$Exercising.0 <- df$Exercising.0 * 5
df$Exercising.1 <- df$Exercising.1 * 9
df$Exercising_overall <- rowMeans(df[,148:150], na.rm = T)

# Financial situation
df$Financial_situation <- rowMeans(df[,151:152], na.rm = T)
alpha(df[,151:152])$total$raw_alpha

# Generalized anxiety disorder
df[,153:159][df[,153:159] == 4] <- 2
df[,153:159][df[,153:159] == 5] <- 3
df[,153:159][df[,153:159] == 9] <- 4
df[,153:159][df[,153:159] == 10] <- 5
df$Generalized_anxiety <- rowMeans(df[,153:159], na.rm = T)
alpha(df[,153:159])$total$raw_alpha

# Covid-19 Anxiety
df$Covid19_anxiety <- rowMeans(df[,160:161], na.rm = T)
alpha(df[,160:161])$total$raw_alpha

# Loneliness
df$Emotional_loneliness <- rowMeans(df[,162:164], na.rm = T) - 36
alpha(df[,162:164])$total$raw_alpha
df$Social_loneliness <- 42- rowMeans(df[,165:167], na.rm = T)
alpha(df[,165:167])$total$raw_alpha

# Mental exercising
df$Mental_exercise <- rowMeans(df[,168:169], na.rm = T) - 10
alpha(df[,168:169])$total$raw_alpha

# Work Motivation
df$Extrinsic_social <- rowMeans(df[,170:172], na.rm = T)
alpha(df[,170:172])$total$raw_alpha
df$Extrinsic_material <- rowMeans(df[,173:175], na.rm = T)
alpha(df[,173:175])$total$raw_alpha
df$Intrinsic_motivation <- rowMeans(df[,176:178], na.rm = T)
alpha(df[,176:178])$total$raw_alpha

# Social contact
df$social_contacts <- rowMeans(df[,179:181], na.rm = T)
alpha(df[,179:181])$total$raw_alpha

# People living in the same house
df$People <- rowSums(df[,195:199], na.rm = T)

# Flights
df$Q54_1_TEXT <- df$Q54_1_TEXT * 1000
df$Q54_2_TEXT <- df$Q54_2_TEXT * 3000
df$Q54_3_TEXT <- df$Q54_3_TEXT * 6000
df$Flights <- rowSums(df[,209:211])
cor(df$Flights, df[,205], use = "pairwise.complete.obs")

df$filler1 <- 1:length(df[,1])
df$filler2 <- 1:length(df[,1])
df$filler3 <- 1:length(df[,1])
df$filler4 <- 1:length(df[,1])
df$filler5 <- 1:length(df[,1])
df$filler6 <- 1:length(df[,1])
df$filler7 <- 1:length(df[,1])
df$filler8 <- 1:length(df[,1])
df$filler9 <- 1:length(df[,1])
df$filler10 <- 1:length(df[,1])



# Principle Component Analysis ----
pca <- principal(cbind(df$Boredom, df$Compliance, df$Conspiracy, df$Self_distraction, df$Active_coping, df$Denial, df$Substance_use, df$Emotional_support, df$Instrumental_support, df$Behavioral_disengagement, df$Venting, df$Positive_reframing, df$Planning, df$Humor, df$Acceptance, df$Religion, df$Self_blame, df$Relatedness, df$Competence, df$Autonomy, df$Need_Cognition, df$Office_setup, df$Communication, df$Stress, df$Daily_routines, df$Self_Control, df$Volunteering, df$Diet, df$Distractions, df$Exercising_overall, df$Financial_situation, df$Generalized_anxiety, df$Covid19_anxiety, df$Emotional_loneliness, df$Social_loneliness, df$Mental_exercise, df$Extrinsic_social, df$Extrinsic_material, df$Intrinsic_motivation, df$social_contacts, df$Quality_of_Sleep_1, df$Technological_Skills_1), rotate = "varimax", nfactors = 3)
pca$values # Eigenvalues
plot(pca$values) # screeplot (suggests 3 components with eigenvalues of 10.56, 4.5, 2.90)
summary(pca)
rownames(pca$loadings) <- c("Boredom", "Compliance", "Conspiracy", "Self_distraction", "Active_coping" , "Denial", "Substance_use", "Emotional_support", "Instrumental_support", "Behavioral_disengagement", "Venting", "Positive_reframing", "Planning", "Humor", "Acceptance", "Religion", "Self_blame", "Relatedness", "Competence", "Autonomy", "Need for Cognition", "Office_setup", "Communication", "Stress", "Daily_routines", "Self_Control", "Volunteering", "Diet", "Distractions", "Exercising_overall", "Financial_situation", "Generalized_anxiety", "Covid19_anxiety", "Emotional_loneliness", "Social_loneliness", "Mental_exercise", "Extrinsic_social", "Extrinsic_material", "Intrinsic_motivation", "social_contacts", "Extraversion now", "Quality_of_Sleep_1", "Technological_Skills_1")
pca$loadings


# Multiple regressions ---- 
# Predictors of well-being
wb <- lm(Well_being ~ Boredom + Conspiracy + Self_distraction + Active_coping +Denial + Substance_use + Emotional_support + Instrumental_support + Behavioral_disengagement + Venting + Positive_reframing + Planning + Humor + Acceptance + Religion + Self_blame + Relatedness + Competence + Autonomy + Need_Cognition + Office_setup + Communication + Stress + Daily_routines + Self_Control + Volunteering + Diet + Distractions + Exercising_overall + Financial_situation + Generalized_anxiety + Covid19_anxiety + Emotional_loneliness + Social_loneliness + Mental_exercise + Extrinsic_social + Extrinsic_material + Intrinsic_motivation + social_contacts + People + Quality_of_Sleep_1 + Technological_Skills_1 + Time_remote + Age, data = df) 
vif(wb)
max(vif(wb))
summary(wb)

# Predictors of well-being with groups of people living in the same household separately entered (e.g., toddlers, adults)
wb1 <- lm(Well_being ~ Boredom + Conspiracy + Self_distraction + Active_coping +Denial + Substance_use + Emotional_support + Instrumental_support + Behavioral_disengagement + Venting + Positive_reframing + Planning + Humor + Acceptance + Religion + Self_blame + Relatedness + Competence + Autonomy + Office_setup + Communication + Stress + Daily_routines + Self_Control + Volunteering + Diet + Distractions + Exercising_overall + Financial_situation + Generalized_anxiety + Covid19_anxiety + Emotional_loneliness + Social_loneliness + Mental_exercise + Extrinsic_social + Extrinsic_material + Intrinsic_motivation + social_contacts  + Quality_of_Sleep_1 + Technological_Skills_1 + Time_remote + Age + Babies + Toddlers + Children + Teenager + Adults, data = df) 
vif(wb1)
max(vif(wb1))
summary(wb1)

# Predictors of Productivity
pro <- lm(Productivity ~ Boredom + Conspiracy + Self_distraction + Active_coping +Denial + Substance_use + Emotional_support + Instrumental_support + Behavioral_disengagement + Venting + Positive_reframing + Planning + Humor + Acceptance + Religion + Self_blame + Relatedness + Competence + Autonomy + Office_setup + Communication + Stress + Daily_routines + Self_Control + Volunteering + Diet + Distractions + Exercising_overall + Financial_situation + Generalized_anxiety + Covid19_anxiety + Emotional_loneliness + Social_loneliness + Mental_exercise + Extrinsic_social + Extrinsic_material + Intrinsic_motivation + social_contacts + People + Quality_of_Sleep_1 + Technological_Skills_1 + Time_remote + Age, data = df) 
vif(pro)
max(vif(pro))
summary(pro)

# Predictors of productivity with groups of people living in the same household separately entered (e.g., toddlers, adults)
pro1 <- lm(Productivity ~ Boredom + Conspiracy + Self_distraction + Active_coping +Denial + Substance_use + Emotional_support + Instrumental_support + Behavioral_disengagement + Venting + Positive_reframing + Planning + Humor + Acceptance + Religion + Self_blame + Relatedness + Competence + Autonomy + Office_setup + Communication + Stress + Daily_routines + Self_Control + Volunteering + Diet + Distractions + Exercising_overall + Financial_situation + Generalized_anxiety + Covid19_anxiety + Emotional_loneliness + Social_loneliness + Mental_exercise + Extrinsic_social + Extrinsic_material + Intrinsic_motivation + social_contacts + Quality_of_Sleep_1 + Technological_Skills_1 + Time_remote + Age + Babies + Toddlers + Children + Teenager + Adults, data = df) 
vif(pro1)
max(vif(pro1))
summary(pro1)

# Regressions with predictors correlating r >= |.30| with outcomes only
m_wb <- lm(Well_being ~ Boredom + Behavioral_disengagement + Self_blame + Relatedness + Competence + Autonomy + Communication + Stress + Daily_routines + Distractions + Generalized_anxiety + Emotional_loneliness + Social_loneliness + social_contacts + Extraversion_now + Quality_of_Sleep_1, data = df)
summary(m_wb)
ggcoefstats(m_wb, xlab = "Regression coefficient", ylab = "Dependent variable: Well-being (time 1)")
vif
# SEM
# -> Poor model fit.
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Boredom =~ Boredom_1 + Boredom_2 + Boredom_3 + Boredom_4 + Boredom_5 + Boredom_6 + Boredom_7 + Boredom_8
Behavioral_disengagement =~ Brief_COPE_6 + Brief_COPE_16
Self_blame =~ Brief_COPE_13 + Brief_COPE_26
Distractions =~ Distractions_at_home_1 + Distractions_at_home_2
Generalized_anxiety =~ Generalized_Anxiety_1 + Generalized_Anxiety_2 + Generalized_Anxiety_3 + Generalized_Anxiety_4 + Generalized_Anxiety_5 + Generalized_Anxiety_6 + Generalized_Anxiety_7
Emotional_loneliness =~ Loneliness_1 + Loneliness_2 + Loneliness_3
Social_loneliness =~ Loneliness_2_1 + Loneliness_2_2 + Loneliness_2_3
Relatedness =~ psychological_needs_1 + psychological_needs_2 + psychological_needs_3 + psychological_needs_4 + psychological_needs_5 + psychological_needs_6
Competence =~ psychological_needs_7 + psychological_needs_8 + psychological_needs_9 + psychological_needs_10 + psychological_needs_11 + psychological_needs_12
Autonomy =~ psychological_needs_13 + psychological_needs_14 + psychological_needs_15 + psychological_needs_16 + psychological_needs_17 + psychological_needs_18
social_contacts =~ social_contacts_1 + social_contacts_2 + social_contacts_3
Communication =~ communication_1 + communication_2 + communication_3
Stress =~ Stress_1 + Stress_2 + Stress_3 + Stress_4
Daily_routines =~ Daily_Routines_1 + Daily_Routines_2 + Daily_Routines_3
Extraversion =~ Extraversion_1 + Extraversion_2 + Extraversion_3 + Extraversion_4
# Paths
Productivity ~ Boredom + Behavioral_disengagement + Self_blame + Distractions + Generalized_anxiety + Emotional_loneliness + Social_loneliness + Relatedness + Competence + Autonomy + social_contacts + Communication + Stress + Daily_routines + Extraversion
Well_being ~ Boredom + Behavioral_disengagement + Self_blame + Distractions + Generalized_anxiety + Emotional_loneliness + Social_loneliness + Relatedness + Competence + Autonomy + social_contacts + Communication + Stress + Daily_routines + Extraversion
# Covariances
Well_being ~~ Productivity
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

m_p <- lm(Productivity ~ Boredom + Competence + Communication + Distractions, data = df)
summary(m_p)
ggcoefstats(m_p, xlab = "Regression coefficient", ylab = "Dependent variable: Productivity")

vif(lm(Productivity ~ Boredom + Competence + Communication + Distractions, data = df))


# Correlations ====
cmn <- cor(cbind(df$Well_being, df$Productivity, df$Boredom, df$Compliance, df$Conspiracy, df$Self_distraction, df$Active_coping, df$Denial, df$Substance_use, df$Emotional_support, df$Instrumental_support, df$Behavioral_disengagement, df$Venting, df$Positive_reframing, df$Planning, df$Humor, df$Acceptance, df$Religion, df$Self_blame, df$Relatedness, df$Competence, df$Autonomy, df$Office_setup, df$Communication, df$Stress, df$Daily_routines, df$Self_Control, df$Volunteering, df$Diet, df$Distractions, df$Exercising_overall, df$Financial_situation, df$Generalized_anxiety, df$Covid19_anxiety, df$Emotional_loneliness, df$Social_loneliness, df$Mental_exercise, df$Extrinsic_social, df$Extrinsic_material, df$Intrinsic_motivation, df$social_contacts, df$People, df$Flights, df$Extraversion_now, df$Quality_of_Sleep_1, df$Technological_Skills_1, df$Time_remote, df$Age, df$Babies, df$Toddlers, df$Children, df$Teenager, df$Adults), use = "pairwise.complete.obs", method = "pearson")
rownames(cmn) <- c("Well-being", "Productivity", "Boredom", "Compliance", "Conspiracy", "Self_distraction", "Active_coping" , "Denial", "Substance_use", "Emotional_support", "Instrumental_support", "Behavioral_disengagement", "Venting", "Positive_reframing", "Planning", "Humor", "Acceptance", "Religion", "Self_blame", "Relatedness", "Competence", "Autonomy", "Office_setup", "Communication", "Stress", "Daily_routines", "Self_Control", "Volunteering", "Diet", "Distractions", "Exercising_overall", "Financial_situation", "Generalized_anxiety", "Covid19_anxiety", "Emotional_loneliness", "Social_loneliness", "Mental_exercise", "Extrinsic_social", "Extrinsic_material", "Intrinsic_motivation", "social_contacts", "People", "Flights", "Extraversion now", "Quality_of_Sleep_1", "Technological_Skills_1", "Time_remote", "Age", "Babies", "Toddlers", "Children", "Teenagers", "Adults")
round(cmn[,1:3],2)

round(cor(cbind(df$Well_being, df$Productivity, df[,30:44]), use = "pairwise.complete.obs")[,1:2],2)
cor(cbind(df$Well_being, df$Productivity, df$People), method = "spearman", use = "pairwise.complete.obs")

# Loop to get CIs for Pearson's r and Spearman's rho
dc <- as.data.frame(cbind(df$Well_being, df$Productivity, df$Boredom, df$Compliance, df$Conspiracy, df$Self_distraction, df$Active_coping, df$Denial, df$Substance_use, df$Emotional_support, df$Instrumental_support, df$Behavioral_disengagement, df$Venting, df$Positive_reframing, df$Planning, df$Humor, df$Acceptance, df$Religion, df$Self_blame, df$Relatedness, df$Competence, df$Autonomy, df$Office_setup, df$Communication, df$Stress, df$Daily_routines, df$Self_Control, df$Volunteering, df$Diet, df$Distractions, df$Exercising_overall, df$Financial_situation, df$Generalized_anxiety, df$Covid19_anxiety, df$Emotional_loneliness, df$Social_loneliness, df$Mental_exercise, df$Extrinsic_social, df$Extrinsic_material, df$Intrinsic_motivation, df$social_contacts, df$People, df$Flights, df$Extraversion_now, df$Quality_of_Sleep_1, df$Technological_Skills_1, df$Time_remote, df$Age))
ex <- 1:length(dc[1,])
x <- matrix(data = NA, nrow = max(ex), ncol = 24)
for(i in ex){
  wb_pearson <- cor.test(dc[,1], dc[,i], use = "p", method = "pearson") # Cor of Well-being
  wb_spearman <- cor.test(dc[,1], dc[,i], use = "p", method = "spearman") # Cor of Well-being
  p_pearson <- cor.test(dc[,2], dc[,i], use = "p", method = "pearson") # Cor of Well-being
  p_spearman <- cor.test(dc[,2], dc[,i], use = "p", method = "spearman") # Cor of Well-being
  x[i,] <- c(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[3]), CIr(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[2]) + 2, level = .95), CIr(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[2]) + 2, level = .999), 
             as.numeric(wb_spearman[4]), as.numeric(wb_spearman[3]), CIr(as.numeric(wb_spearman[4]), as.numeric(wb_pearson[2]) + 2, level = .95), CIr(as.numeric(wb_spearman[4]), as.numeric(wb_pearson[2]) + 2, level = .999),
             as.numeric(p_pearson[4]), as.numeric(p_pearson[3]), CIr(as.numeric(p_pearson[4]), as.numeric(p_pearson[2]) + 2, level = .95), CIr(as.numeric(p_pearson[4]), as.numeric(p_pearson[2]) + 2, level = .999), 
             as.numeric(p_spearman[4]), as.numeric(p_spearman[3]), CIr(as.numeric(p_spearman[4]), as.numeric(p_pearson[2]) + 2, level = .95), CIr(as.numeric(p_spearman[4]), as.numeric(p_pearson[2]) + 2, level = .999)) # r, p, 95% CI, 99.9% CI. First for Pearson's correlations of well-being with the other variables, than Spearman's rho, than again for productivity.
}
View(round(x, 3))








# wave2 ==========
# Internal consistencies and factor creation -----

# Well-beingW2 (adapted Satisfaction with Life Scale)
df$Well_beingW2 <- rowMeans(df[,332:336], na.rm = TRUE) - 14
alpha(df[,332:336])$total$raw_alpha
cor(df$Well_being, df$Well_beingW2, use = "p") # Test-retest reliability

# ProductivityW2
df$ProductivityW2 <- (df$Productivity_12/df$Productivity_42) * ((df$Productivity_1.12 + 100)/100)
cor(df$Productivity, df$ProductivityW2, use = "p")

# BoredomW2
df$BoredomW2 <- rowMeans(df[,360:367]) - 10
alpha(df[,360:367])$total$raw_alpha
cor(df$Boredom, df$BoredomW2, use = "p")

#Brief COPEW2 (coping strategies)
df$Behavioral_disengagementW2 <- rowMeans(cbind(df$Brief_COPE_1.2, df$Brief_COPE_2.2), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_1.2, df$Brief_COPE_2.2))$total$raw_alpha
cor(df$Behavioral_disengagement, df$Behavioral_disengagementW2, use = "p")
df$Self_blameW2 <- rowMeans(cbind(df$Brief_COPE_3.2, df$Brief_COPE_4.2), na.rm = T) - 3
alpha(cbind(df$Brief_COPE_3.2, df$Brief_COPE_4.2))$total$raw_alpha
cor(df$Self_blame, df$Self_blameW2, use = "p")

# Distractions at homeW2
df$Distractions_at_home_12 <- df$Distractions_at_home_12 - 19
df$Distractions_at_home_22 <- 25 - df$Distractions_at_home_22
df$DistractionsW2 <- rowMeans(df[,413:414], na.rm = T)
alpha(df[,413:414])$total$raw_alpha
cor(df$Distractions, df$DistractionsW2, use = "p")

# Generalized anxiety disorderW2
df[,415:421][df[,415:421] == 4] <- 2
df[,415:421][df[,415:421] == 5] <- 3
df[,415:421][df[,415:421] == 9] <- 4
df[,415:421][df[,415:421] == 10] <- 5
df$Generalized_anxietyW2 <- rowMeans(df[,415:421], na.rm = T)
alpha(df[,415:421])$total$raw_alpha
cor(df$Generalized_anxiety, df$Generalized_anxietyW2, use = "p")

# LonelinessW2
df$Emotional_lonelinessW2 <- rowMeans(df[,422:424], na.rm = T) - 36
alpha(df[,422:424])$total$raw_alpha
cor(df$Emotional_loneliness, df$Emotional_lonelinessW2, use = "p")
df$Social_lonelinessW2 <- 42- rowMeans(df[,427:429], na.rm = T)
alpha(df[,427:429])$total$raw_alpha
cor(df$Social_loneliness, df$Social_lonelinessW2, use = "p")

# Need for relatednessW2
df$psychological_needs_22 <- 6 - df$psychological_needs_22
df$psychological_needs_42 <- 6 - df$psychological_needs_42
df$psychological_needs_62 <- 6 - df$psychological_needs_62
df$RelatednessW2 <- rowMeans(df[,376:381], na.rm = T)
alpha(df[,376:381])$total$raw_alpha
cor(df$Relatedness, df$RelatednessW2, use = "p")

# Need for competenceW2
df$psychological_needs_82 <- 6 - df$psychological_needs_82
df$psychological_needs_102 <- 6 - df$psychological_needs_102
df$psychological_needs_122 <- 6 - df$psychological_needs_122
df$CompetenceW2 <- rowMeans(df[,382:387], na.rm = T)
alpha(df[,382:387])$total$raw_alpha
cor(df$Competence, df$CompetenceW2, use = "p")

# Need for autonomyW2
df$psychological_needs_142 <- 6 - df$psychological_needs_142
df$psychological_needs_162 <- 6 - df$psychological_needs_162
df$psychological_needs_182 <- 6 - df$psychological_needs_182
df$AutonomyW2 <- rowMeans(df[,388:393], na.rm = T)
alpha(df[,388:393])$total$raw_alpha
cor(df$Autonomy, df$AutonomyW2, use = "p")

# Social contactW2
df$social_contactsW2 <- rowMeans(df[,430:432], na.rm = T)
alpha(df[,430:432])$total$raw_alpha
cor(df$social_contacts, df$social_contactsW2, use = "p")

# Communication with colleagues and line managersW2
df$CommunicationW2 <- rowMeans(df[,394:396], na.rm = T)
alpha(df[,394:396])$total$raw_alpha
cor(df$Communication, df$CommunicationW2, use = "p")

# StressW2
df$Stress_22 <- 6 - df$Stress_22
df$Stress_32 <- 6 - df$Stress_32
df$StressW2 <- rowMeans(df[,398:401], na.rm = T)
alpha(df[,398:401])$total$raw_alpha
cor(df$Stress, df$StressW2, use = "p")

# Daily RoutinesW2
df$Daily_routinesW2 <- rowMeans(df[,402:404], na.rm = T)
alpha(df[,402:404])$total$raw_alpha
cor(df$Daily_routines, df$Daily_routinesW2, use = "p")

# difficulty of establishing social contact
df$difficulty_contactW2 <- rowMeans(df[,433:434], na.rm = T)
alpha(df[,433:434])$total$raw_alpha

# ExtraversionW2
df$Extraversion_12 <- 6 - df$Extraversion_12
df$Extraversion_42 <- 6 - df$Extraversion_42
df$Extraversion_W2 <- rowMeans(df[,405:408], na.rm = T)
alpha(df[,405:408])$total$raw_alpha


# Multiple regressions W2 ====
m_wb2 <- lm(Well_beingW2 ~  BoredomW2 + Behavioral_disengagementW2 + Self_blameW2 + RelatednessW2 + CompetenceW2 + AutonomyW2 + CommunicationW2 + StressW2 + Daily_routinesW2 + DistractionsW2 + Generalized_anxietyW2 + Emotional_lonelinessW2 + Social_lonelinessW2 + social_contactsW2 + Extraversion_W2 + df$Quality_of_Sleep_12, data = df)
summary(m_wb2)
vif(lm(Well_beingW2 ~  BoredomW2 + Behavioral_disengagementW2 + Self_blameW2 + RelatednessW2 + CompetenceW2 + AutonomyW2 + CommunicationW2 + StressW2 + Daily_routinesW2 + DistractionsW2 + Generalized_anxietyW2 + Emotional_lonelinessW2 + Social_lonelinessW2 + social_contactsW2 + Extraversion_W2 + df$Quality_of_Sleep_12, data = df))

m_p2 <- lm(ProductivityW2 ~ BoredomW2 + CompetenceW2 + CommunicationW2 + DistractionsW2, data = df)
summary(m_p2)
vif(lm(ProductivityW2 ~ BoredomW2 + CompetenceW2 + CommunicationW2 + DistractionsW2, data = df))

ggcoefstats(m_wb2, xlab = "Regression coefficient", ylab = "Dependent variable: Well-being (time 2)") #+ theme(text = element_text(size = 20))
ggcoefstats(m_p2, xlab = "Regression coefficient", ylab = "Dependent variable: Productivity (time 2)")


#correlations W2=====
cor(cbind(df$difficulty_contact, df$Well_beingW2, df$Extraversion_W2))
cor.test(df$difficulty_contact, df$Extraversion_W2)

cor(cbind(df$Extraversion_W2, df$difficulty_contact, df$Daily_routinesW2, df$StressW2, df$CommunicationW2, df$AutonomyW2, df$CompetenceW2, df$RelatednessW2, df$Social_lonelinessW2, df$Emotional_lonelinessW2, df$Generalized_anxietyW2, df$DistractionsW2, df$ProductivityW2, df$Well_beingW2, df$BoredomW2, df$Behavioral_disengagementW2, df$Self_blameW2 + df$Quality_of_Sleep_12), use="pairwise.complete.obs")

round(cor(df[, c(492:509, 397)], use="pairwise.complete.obs")[,1:2],2)

# Extraversion score for selected participants
which(colnames(df) == "Extraversion_now")
which(colnames(df) == "Extraversion_W2")

df[c(80, 179, 19, 130, 124), 467]
df[c(80, 179, 19, 130, 124), 509]

# Loop to get CIs for Pearson's r and Spearman's rho
dc2 <- as.data.frame(cbind(df$Well_beingW2, df$ProductivityW2, df$BoredomW2, df$Behavioral_disengagementW2, df$Self_blameW2, df$RelatednessW2, df$CompetenceW2, df$AutonomyW2, df$CommunicationW2, df$StressW2, df$Daily_routinesW2, df$DistractionsW2, df$Generalized_anxietyW2, df$Emotional_lonelinessW2, df$Social_lonelinessW2, df$social_contactsW2, df$Extraversion_W2, df$Quality_of_Sleep_12, df$Age))
ex <- 1:length(dc2[1,])
x <- matrix(data = NA, nrow = max(ex), ncol = 24)
for(i in ex){
  wb_pearson <- cor.test(dc2[,1], dc2[,i], use = "p", method = "pearson") # Cor of Well-being
  wb_spearman <- cor.test(dc2[,1], dc2[,i], use = "p", method = "spearman") # Cor of Well-being
  p_pearson <- cor.test(dc2[,2], dc2[,i], use = "p", method = "pearson") # Cor of Well-being
  p_spearman <- cor.test(dc2[,2], dc2[,i], use = "p", method = "spearman") # Cor of Well-being
  x[i,] <- c(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[3]), CIr(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[2]) + 2, level = .95), CIr(as.numeric(wb_pearson[4]), as.numeric(wb_pearson[2]) + 2, level = .999), 
             as.numeric(wb_spearman[4]), as.numeric(wb_spearman[3]), CIr(as.numeric(wb_spearman[4]), as.numeric(wb_pearson[2]) + 2, level = .95), CIr(as.numeric(wb_spearman[4]), as.numeric(wb_pearson[2]) + 2, level = .999),
             as.numeric(p_pearson[4]), as.numeric(p_pearson[3]), CIr(as.numeric(p_pearson[4]), as.numeric(p_pearson[2]) + 2, level = .95), CIr(as.numeric(p_pearson[4]), as.numeric(p_pearson[2]) + 2, level = .999), 
             as.numeric(p_spearman[4]), as.numeric(p_spearman[3]), CIr(as.numeric(p_spearman[4]), as.numeric(p_pearson[2]) + 2, level = .95), CIr(as.numeric(p_spearman[4]), as.numeric(p_pearson[2]) + 2, level = .999)) # r, p, 95% CI, 99.9% CI. First for Pearson's correlations of well-being with the other variables, than Spearman's rho, than again for productivity.
}
View(round(x, 3))




###
# Longitudinal data analysis ==== 
# Example code: https://gist.github.com/mkearney/eb8f4a6cc94680d8bfda94ee62c7e9c5 
# See SI of this paper: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0209133

# Well-being #### 
# Well-being-Productivity
well_prod <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
# Paths
ProductivityW2 ~ Well_being + Productivity
Well_beingW2 ~ Well_being + Productivity
# Covariances
Well_being ~~ Productivity
Well_beingW2 ~~ ProductivityW2
'
m <- sem(well_prod, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being-Boredom
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Boredom =~ Boredom_1 + Boredom_2 + Boredom_3 + Boredom_4 + Boredom_5 + Boredom_6 + Boredom_7 + Boredom_8
BoredomW2 =~ Boredom_12 + Boredom_22 + Boredom_32 + Boredom_42 + Boredom_52 + Boredom_62 + Boredom_72 + Boredom_82
# Paths
BoredomW2 ~ Well_being + Boredom
Well_beingW2 ~ Well_being + Boredom
# Covariances
Well_being ~~ Boredom
Well_beingW2 ~~ BoredomW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being-Behavioral disengagement
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Behavioral_disengagement =~ Brief_COPE_6 + Brief_COPE_16
Behavioral_disengagementW2 =~ Brief_COPE_1.2 + Brief_COPE_2.2
# Paths
Behavioral_disengagementW2 ~ Well_being + Behavioral_disengagement
Well_beingW2 ~ Well_being + Behavioral_disengagement
# Covariances
Well_being ~~ Behavioral_disengagement
Well_beingW2 ~~ Behavioral_disengagementW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being-Self-blame
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Self_blame =~ Brief_COPE_13 + Brief_COPE_26
Self_blameW2 =~ Brief_COPE_3.2 + Brief_COPE_4.2
# Paths
Self_blameW2 ~ Well_being + Self_blame
Well_beingW2 ~ Well_being + Self_blame
# Covariances
Well_being ~~ Self_blame
Well_beingW2 ~~ Self_blameW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being - Distractions_at_home
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Distractions =~ Distractions_at_home_1 + Distractions_at_home_2
DistractionsW2 =~ Distractions_at_home_12 + Distractions_at_home_22
# Paths
DistractionsW2 ~ Well_being + Distractions
Well_beingW2 ~ Well_being + Distractions
# Covariances
Well_being ~~ Distractions
Well_beingW2 ~~ DistractionsW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being - Generalized_anxiety
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Generalized_anxiety =~ Generalized_Anxiety_1 + Generalized_Anxiety_2 + Generalized_Anxiety_3 + Generalized_Anxiety_4 + Generalized_Anxiety_5 + Generalized_Anxiety_6 + Generalized_Anxiety_7
Generalized_anxietyW2 =~ Generalized_Anxiety_12 + Generalized_Anxiety_22 + Generalized_Anxiety_32 + Generalized_Anxiety_42 + Generalized_Anxiety_52 + Generalized_Anxiety_62 + Generalized_Anxiety_72
# Paths
Generalized_anxietyW2 ~ Well_being + Generalized_anxiety
Well_beingW2 ~ Well_being + Generalized_anxiety
# Covariances
Well_being ~~ Generalized_anxiety
Well_beingW2 ~~ Generalized_anxietyW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Emotional_loneliness
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Emotional_loneliness =~ Loneliness_1 + Loneliness_2 + Loneliness_3
Emotional_lonelinessW2 =~ Loneliness_12 + Loneliness_22 + Loneliness_32
# Paths
Emotional_lonelinessW2 ~ Well_being + Emotional_loneliness
Well_beingW2 ~ Well_being + Emotional_loneliness
# Covariances
Well_being ~~ Emotional_loneliness
Well_beingW2 ~~ Emotional_lonelinessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Social_loneliness
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Social_loneliness =~ Loneliness_2_1 + Loneliness_2_2 + Loneliness_2_3
Social_lonelinessW2 =~ Loneliness_2_12 + Loneliness_2_22 + Loneliness_2_32
# Paths
Social_lonelinessW2 ~ Well_being + Social_loneliness
Well_beingW2 ~ Well_being + Social_loneliness
# Covariances
Well_being ~~ Social_loneliness
Well_beingW2 ~~ Social_lonelinessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Need for Relatedness
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Relatedness =~ psychological_needs_1 + psychological_needs_2 + psychological_needs_3 + psychological_needs_4 + psychological_needs_5 + psychological_needs_6
RelatednessW2 =~ psychological_needs_1_2 + psychological_needs_22 + psychological_needs_32 + psychological_needs_42 + psychological_needs_52 + psychological_needs_62
# Paths
RelatednessW2 ~ Well_being + Relatedness
Well_beingW2 ~ Well_being + Relatedness
# Covariances
Well_being ~~ Relatedness
Well_beingW2 ~~ RelatednessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Need for Competence
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Competence =~ psychological_needs_7 + psychological_needs_8 + psychological_needs_9 + psychological_needs_10 + psychological_needs_11 + psychological_needs_12
CompetenceW2 =~ psychological_needs_72 + psychological_needs_82 + psychological_needs_92 + psychological_needs_102 + psychological_needs_112 + psychological_needs_122
# Paths
CompetenceW2 ~ Well_being + Competence
Well_beingW2 ~ Well_being + Competence
# Covariances
Well_being ~~ Competence
Well_beingW2 ~~ CompetenceW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Need for Autonomy
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Autonomy =~ psychological_needs_13 + psychological_needs_14 + psychological_needs_15 + psychological_needs_16 + psychological_needs_17 + psychological_needs_18
AutonomyW2 =~ psychological_needs_132 + psychological_needs_142 + psychological_needs_152 + psychological_needs_162 + psychological_needs_172 + psychological_needs_182
# Paths
AutonomyW2 ~ Well_being + Autonomy
Well_beingW2 ~ Well_being + Autonomy
# Covariances
Well_being ~~ Autonomy
Well_beingW2 ~~ AutonomyW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Social contacts
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
social_contacts =~ social_contacts_1 + social_contacts_2 + social_contacts_3
social_contactsW2 =~ social_contacts_12 + social_contacts_22 + social_contacts_32
# Paths
social_contactsW2 ~ Well_being + social_contacts
Well_beingW2 ~ Well_being + social_contacts
# Covariances
Well_being ~~ social_contacts
Well_beingW2 ~~ social_contactsW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Communication with colleagues
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Communication =~ communication_1 + communication_2 + communication_3
CommunicationW2 =~ communication_12 + communication_22 + communication_32
# Paths
CommunicationW2 ~ Well_being + Communication
Well_beingW2 ~ Well_being + Communication
# Covariances
Well_being ~~ Communication
Well_beingW2 ~~ CommunicationW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Stress
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Stress =~ Stress_1 + Stress_2 + Stress_3 + Stress_4
StressW2 =~ Stress_12 + Stress_22 + Stress_32 + Stress_42
# Paths
StressW2 ~ Well_being + Stress
Well_beingW2 ~ Well_being + Stress
# Covariances
Well_being ~~ Stress
Well_beingW2 ~~ StressW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Daily routines
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Daily_routines =~ Daily_Routines_1 + Daily_Routines_2 + Daily_Routines_3
Daily_routinesW2 =~ Daily_Routines_12 + Daily_Routines_22 + Daily_Routines_32
# Paths
Daily_routinesW2 ~ Well_being + Daily_routines
Well_beingW2 ~ Well_being + Daily_routines
# Covariances
Well_being ~~ Daily_routines
Well_beingW2 ~~ Daily_routinesW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Well-being and Extraversion
model <- '
# Measurement model
Well_being =~ Well_being_1 + Well_being_2 + Well_being_3 + Well_being_4 + Well_being_5
Well_beingW2 =~ Well_being_12 + Well_being_22 + Well_being_32 + Well_being_42 + Well_being_52
Extraversion =~ Extraversion_1 + Extraversion_2 + Extraversion_3 + Extraversion_4
ExtraversionW2 =~ Extraversion_12 + Extraversion_22 + Extraversion_32 + Extraversion_42
# Paths
ExtraversionW2 ~ Well_being + Extraversion
Well_beingW2 ~ Well_being + Extraversion
# Covariances
Well_being ~~ Extraversion
Well_beingW2 ~~ ExtraversionW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)


# Productivity ####
# Productivity-Boredom
model <- '
# Measurement model
Boredom =~ Boredom_1 + Boredom_2 + Boredom_3 + Boredom_4 + Boredom_5 + Boredom_6 + Boredom_7 + Boredom_8
BoredomW2 =~ Boredom_12 + Boredom_22 + Boredom_32 + Boredom_42 + Boredom_52 + Boredom_62 + Boredom_72 + Boredom_82
# Paths
BoredomW2 ~ Productivity + Boredom
ProductivityW2 ~ Productivity + Boredom
# Covariances
Productivity ~~ Boredom
ProductivityW2 ~~ BoredomW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity-Behavioral disengagement
model <- '
# Measurement model
Behavioral_disengagement =~ Brief_COPE_6 + Brief_COPE_16
Behavioral_disengagementW2 =~ Brief_COPE_1.2 + Brief_COPE_2.2
# Paths
Behavioral_disengagementW2 ~ Productivity + Behavioral_disengagement
ProductivityW2 ~ Productivity + Behavioral_disengagement
# Covariances
Productivity ~~ Behavioral_disengagement
ProductivityW2 ~~ Behavioral_disengagementW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity-Self-blame
model <- '
# Measurement model
Self_blame =~ Brief_COPE_13 + Brief_COPE_26
Self_blameW2 =~ Brief_COPE_3.2 + Brief_COPE_4.2
# Paths
Self_blameW2 ~ Productivity + Self_blame
ProductivityW2 ~ Productivity + Self_blame
# Covariances
Productivity ~~ Self_blame
ProductivityW2 ~~ Self_blameW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity - Distractions_at_home
model <- '
# Measurement model
Distractions =~ Distractions_at_home_1 + Distractions_at_home_2
DistractionsW2 =~ Distractions_at_home_12 + Distractions_at_home_22
# Paths
DistractionsW2 ~ Productivity + Distractions
ProductivityW2 ~ Productivity + Distractions
# Covariances
Productivity ~~ Distractions
ProductivityW2 ~~ DistractionsW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity - Generalized_anxiety
model <- '
# Measurement model
Generalized_anxiety =~ Generalized_Anxiety_1 + Generalized_Anxiety_2 + Generalized_Anxiety_3 + Generalized_Anxiety_4 + Generalized_Anxiety_5 + Generalized_Anxiety_6 + Generalized_Anxiety_7
Generalized_anxietyW2 =~ Generalized_Anxiety_12 + Generalized_Anxiety_22 + Generalized_Anxiety_32 + Generalized_Anxiety_42 + Generalized_Anxiety_52 + Generalized_Anxiety_62 + Generalized_Anxiety_72
# Paths
Generalized_anxietyW2 ~ Productivity + Generalized_anxiety
ProductivityW2 ~ Productivity + Generalized_anxiety
# Covariances
Productivity ~~ Generalized_anxiety
ProductivityW2 ~~ Generalized_anxietyW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Emotional_loneliness
model <- '
# Measurement model
Emotional_loneliness =~ Loneliness_1 + Loneliness_2 + Loneliness_3
Emotional_lonelinessW2 =~ Loneliness_12 + Loneliness_22 + Loneliness_32
# Paths
Emotional_lonelinessW2 ~ Productivity + Emotional_loneliness
ProductivityW2 ~ Productivity + Emotional_loneliness
# Covariances
Productivity ~~ Emotional_loneliness
ProductivityW2 ~~ Emotional_lonelinessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Social_loneliness
model <- '
# Measurement model
Social_loneliness =~ Loneliness_2_1 + Loneliness_2_2 + Loneliness_2_3
Social_lonelinessW2 =~ Loneliness_2_12 + Loneliness_2_22 + Loneliness_2_32
# Paths
Social_lonelinessW2 ~ Productivity + Social_loneliness
ProductivityW2 ~ Productivity + Social_loneliness
# Covariances
Productivity ~~ Social_loneliness
ProductivityW2 ~~ Social_lonelinessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Need for Relatedness
model <- '
# Measurement model
Relatedness =~ psychological_needs_1 + psychological_needs_2 + psychological_needs_3 + psychological_needs_4 + psychological_needs_5 + psychological_needs_6
RelatednessW2 =~ psychological_needs_1_2 + psychological_needs_22 + psychological_needs_32 + psychological_needs_42 + psychological_needs_52 + psychological_needs_62
# Paths
RelatednessW2 ~ Productivity + Relatedness
ProductivityW2 ~ Productivity + Relatedness
# Covariances
Productivity ~~ Relatedness
ProductivityW2 ~~ RelatednessW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Need for Competence
model <- '
# Measurement model
Competence =~ psychological_needs_7 + psychological_needs_8 + psychological_needs_9 + psychological_needs_10 + psychological_needs_11 + psychological_needs_12
CompetenceW2 =~ psychological_needs_72 + psychological_needs_82 + psychological_needs_92 + psychological_needs_102 + psychological_needs_112 + psychological_needs_122
# Paths
CompetenceW2 ~ Productivity + Competence
ProductivityW2 ~ Productivity + Competence
# Covariances
Productivity ~~ Competence
ProductivityW2 ~~ CompetenceW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Need for Autonomy
model <- '
# Measurement model
Autonomy =~ psychological_needs_13 + psychological_needs_14 + psychological_needs_15 + psychological_needs_16 + psychological_needs_17 + psychological_needs_18
AutonomyW2 =~ psychological_needs_132 + psychological_needs_142 + psychological_needs_152 + psychological_needs_162 + psychological_needs_172 + psychological_needs_182
# Paths
AutonomyW2 ~ Productivity + Autonomy
ProductivityW2 ~ Productivity + Autonomy
# Covariances
Productivity ~~ Autonomy
ProductivityW2 ~~ AutonomyW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Social contacts
model <- '
# Measurement model
social_contacts =~ social_contacts_1 + social_contacts_2 + social_contacts_3
social_contactsW2 =~ social_contacts_12 + social_contacts_22 + social_contacts_32
# Paths
social_contactsW2 ~ Productivity + social_contacts
ProductivityW2 ~ Productivity + social_contacts
# Covariances
Productivity ~~ social_contacts
ProductivityW2 ~~ social_contactsW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Communication with colleagues
model <- '
# Measurement model
Communication =~ communication_1 + communication_2 + communication_3
CommunicationW2 =~ communication_12 + communication_22 + communication_32
# Paths
CommunicationW2 ~ Productivity + Communication
ProductivityW2 ~ Productivity + Communication
# Covariances
Productivity ~~ Communication
ProductivityW2 ~~ CommunicationW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Stress
model <- '
# Measurement model
Stress =~ Stress_1 + Stress_2 + Stress_3 + Stress_4
StressW2 =~ Stress_12 + Stress_22 + Stress_32 + Stress_42
# Paths
StressW2 ~ Productivity + Stress
ProductivityW2 ~ Productivity + Stress
# Covariances
Productivity ~~ Stress
ProductivityW2 ~~ StressW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Daily routines
model <- '
# Measurement model
Daily_routines =~ Daily_Routines_1 + Daily_Routines_2 + Daily_Routines_3
Daily_routinesW2 =~ Daily_Routines_12 + Daily_Routines_22 + Daily_Routines_32
# Paths
Daily_routinesW2 ~ Productivity + Daily_routines
ProductivityW2 ~ Productivity + Daily_routines
# Covariances
Productivity ~~ Daily_routines
ProductivityW2 ~~ Daily_routinesW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)

# Productivity and Extraversion
model <- '
# Measurement model
Extraversion =~ Extraversion_1 + Extraversion_2 + Extraversion_3 + Extraversion_4
ExtraversionW2 =~ Extraversion_12 + Extraversion_22 + Extraversion_32 + Extraversion_42
# Paths
ExtraversionW2 ~ Productivity + Extraversion
ProductivityW2 ~ Productivity + Extraversion
# Covariances
Productivity ~~ Extraversion
ProductivityW2 ~~ ExtraversionW2
'
m <- sem(model, check.lv.names = F, estimator = "MLM", se = "robust", test = "Satorra-Bentler", data = df) # check.lv.names = F is required from lavaan version 0.6 onwards
summary(m, fit.measures = T)
semPaths(m, fixedStyle = 1, nCharNodes = 0, whatLabels = "est", layout = "spring", sizeMan = 8)




# Changes in variables pre-post ====
df1 <- as.data.frame(cbind(df$Well_being, df$Well_beingW2, df$Productivity, df$ProductivityW2, df$Boredom, df$BoredomW2, df$Behavioral_disengagement, df$Behavioral_disengagementW2, df$Self_blame, df$Self_blameW2, df$Relatedness, df$RelatednessW2, df$Competence, df$CompetenceW2, df$Autonomy, df$AutonomyW2, df$Communication, df$CommunicationW2, df$Stress, df$StressW2, df$Daily_routines, df$Daily_routinesW2, df$Distractions, df$DistractionsW2, df$Generalized_anxiety, df$Generalized_anxietyW2, df$Emotional_loneliness, df$Emotional_lonelinessW2, df$Social_loneliness, df$Social_lonelinessW2, df$social_contacts, df$social_contactsW2, df$Extraversion_now, df$Extraversion_W2, df$Quality_of_Sleep_1, df$Quality_of_Sleep_12))

summary(glm(df$Well_being_down ~ df$Social_lonelinessW2*df$social_contactsW2, family = binomial(link = "logit")))
df$Well_being_down <- ifelse(df[,439] > df[,492], 1, 0)
(p <- ggbetweenstats(data = df, x = Well_being_down, y = Social_lonelinessW2, ylab = "Loneliness "))


t <- t.test(df1[,1], df1[,1+1], paired = T)
t.value <- round(as.numeric(t[1]),3)
p.value <- round(as.numeric(t[3]),4)
m1 <- round(mean(df1[,1], na.rm = T),3)
m2 <- round(mean(df1[,1 + 1], na.rm = T),3)
sd1 <- round(sd(df1[,1], na.rm = T),3)
sd2 <- round(sd(df1[,1 + 1], na.rm = T),3)
d <- round(as.numeric(cohen.d(df1[,1], df1[,1+1], paired = T, na.rm = T)[[3]]),3)
Greater <- length(which(df1[,1] < df1[,1+1])) # How many participants score higher at t2 than t1?
Lower <- length(which(df1[,1] > df1[,1+1]))
Equal <- length(which(df1[,1] == df1[,1+1]))


length(which(df1[,1] < df1[,1+1]))



# Loop
ex <- seq(1, 35, by = 2)
x <- matrix(data = NA, nrow = max(ex), ncol = 10)
for(i in ex){
  t <- t.test(df1[,i], df1[,i+1], paired = T)
  t.value <- round(as.numeric(t[1]),3)
  p.value <- round(as.numeric(t[3]),4)
  m1 <- round(mean(df1[,i], na.rm = T),3)
  m2 <- round(mean(df1[,i + 1], na.rm = T),3)
  sd1 <- round(sd(df1[,i], na.rm = T),3)
  sd2 <- round(sd(df1[,i + 1], na.rm = T),3)
  d <- round(as.numeric(effsize::cohen.d(df1[,i], df1[,i+1], paired = T, na.rm = T)[[3]]),3)
  Greater <- length(which(df1[,i] < df1[,i+1])) # How many participants score higher at t2 than t1?
  Lower <- length(which(df1[,i] > df1[,i+1]))
  Equal <- length(which(df1[,i] == df1[,i+1]))
  
  x[i,] <- c(m1, sd1, m2, sd2, t.value, p.value, d, Greater, Lower, Equal)
}
View(x[ex,])


df$Young_people <- rowSums(cbind(df$Babies, df$Toddlers, df$Children, df$Teenager), na.rm = T)
df2 <- as.data.frame(cbind(df$Gender, df$Well_being, df$Well_beingW2, df$Productivity, df$ProductivityW2, df$Boredom, df$BoredomW2, df$Behavioral_disengagement, df$Behavioral_disengagementW2, df$Self_blame, df$Self_blameW2, df$Relatedness, df$RelatednessW2, df$Competence, df$CompetenceW2, df$Autonomy, df$AutonomyW2, df$Communication, df$CommunicationW2, df$Stress, df$StressW2, df$Daily_routines, df$Daily_routinesW2, df$Distractions, df$DistractionsW2, df$Generalized_anxiety, df$Generalized_anxietyW2, df$Emotional_loneliness, df$Emotional_lonelinessW2, df$Social_loneliness, df$Social_lonelinessW2, df$social_contacts, df$social_contactsW2, df$Extraversion_now, df$Extraversion_W2, df$Quality_of_Sleep_1, df$Quality_of_Sleep_12, df$Compliance, df$Conspiracy, df$Self_distraction, df$Active_coping, df$Denial, df$Substance_use, df$Emotional_support, df$Instrumental_support, df$Venting, df$Positive_reframing, df$Planning, df$Humor, df$Acceptance, df$Religion, df$Office_setup, df$Self_Control, df$Volunteering, df$Diet, df$Exercising_overall, df$Financial_situation, df$Covid19_anxiety, df$Mental_exercise, df$Extrinsic_social, df$Extrinsic_material, df$Intrinsic_motivation, df$People, df$Technological_Skills_1, df$Time_remote, df$Young_people, df$Q43)) # Q43: In which country are you currently based in?

# Gender mean differences #### 
ex <- 2:66
x <- matrix(data = NA, nrow = max(ex), ncol = 8)
for(i in ex){
  t <- t.test(df2[,i][df$Gender == 1], df2[,i][df$Gender == 2]) # 1: men, 2: women
  t.value <- round(as.numeric(t[1]),3)
  p.value <- round(as.numeric(t[3]),4)
  m1 <- round(mean(df2[,i][df$Gender == 1], na.rm = T),3)
  m2 <- round(mean(df2[,i][df$Gender == 2], na.rm = T),3)
  sd1 <- round(sd(df2[,i][df$Gender == 1], na.rm = T),3)
  sd2 <- round(sd(df2[,i][df$Gender == 2], na.rm = T),3)
  d <- round(as.numeric(effsize::cohen.d(df2[,i][df$Gender == 1], df2[,i][df$Gender == 2], na.rm = T)[[3]]),3)
  x[i,] <- c(m1, sd1, m2, sd2, t.value, p.value, d, round(PCR(d),3))
}
View(x[ex,])


# Differences between the UK and the USA ====
ex <- 2:66
x <- matrix(data = NA, nrow = max(ex), ncol = 8)
for(i in ex){
  t <- t.test(df2[,i][df$Q43 == 1], df2[,i][df$Q43 == 2]) # 1: UK, 2: USA
  t.value <- round(as.numeric(t[1]),3)
  p.value <- round(as.numeric(t[3]),4)
  m1 <- round(mean(df2[,i][df$Q43 == 1], na.rm = T),3)
  m2 <- round(mean(df2[,i][df$Q43 == 2], na.rm = T),3)
  sd1 <- round(sd(df2[,i][df$Q43 == 1], na.rm = T),3)
  sd2 <- round(sd(df2[,i][df$Q43 == 2], na.rm = T),3)
  d <- round(as.numeric(effsize::cohen.d(df2[,i][df$Q43 == 1], df2[,i][df$Q43 == 2], na.rm = T)[[3]]),3)
  x[i,] <- c(m1, sd1, m2, sd2, t.value, p.value, d, round(PCR(d),3))
}
View(x[ex,])



# Mean comparisons within time using lme4 ####
library(lme4)
library(lmerTest)
library(reshape2)

# Well-being
ds <- df[,c(which(colnames(df) == "Well_being"), which(colnames(df) == "Well_beingW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p1 <- ggplot(ds, aes(x = variable, y = value)) +
  stat_summary(
  geom = "point",
  fun = "mean",
  col = "red",
  size = 3)  +
  geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
  geom_point(position = position_jitter(h = 0, w = .1)) +
  geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
  theme_light(base_size = 25) +
  ylab("Well-being") + 
  xlab("Time")) # add SEs: stat_summary(geom = "errorbar",aes(width=0.3),col = "blue")

# Productivity
ds <- df[,c(which(colnames(df) == "Productivity"), which(colnames(df) == "ProductivityW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p2 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Productivity") + 
    xlab("Time")) 

# Boredom
ds <- df[,c(which(colnames(df) == "Boredom"), which(colnames(df) == "BoredomW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p3 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Boredom") + 
    xlab("Time")) 

# Behavioral disengagement
ds <- df[,c(which(colnames(df) == "Behavioral_disengagement"), which(colnames(df) == "Behavioral_disengagementW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p4 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Behavioral disengagement") + 
    xlab("Time")) 

# Self-blame
ds <- df[,c(which(colnames(df) == "Self_blame"), which(colnames(df) == "Self_blameW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p5 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Self-blame") + 
    xlab("Time")) 

# Need for relatedness
ds <- df[,c(which(colnames(df) == "Relatedness"), which(colnames(df) == "RelatednessW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p6 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Need for relatedness") + 
    xlab("Time")) 

# Need for competence
ds <- df[,c(which(colnames(df) == "Competence"), which(colnames(df) == "CompetenceW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p7 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Need for competence") + 
    xlab("Time")) 

# Need for autonomy
ds <- df[,c(which(colnames(df) == "Autonomy"), which(colnames(df) == "AutonomyW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p8 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Need for autonomy") + 
    xlab("Time")) 

# Communication with colleagues
ds <- df[,c(which(colnames(df) == "Communication"), which(colnames(df) == "CommunicationW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p9 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Communication with colleagues") + 
    xlab("Time")) 

# Stress
ds <- df[,c(which(colnames(df) == "Stress"), which(colnames(df) == "StressW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p10 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Stress") + 
    xlab("Time")) 

# Daily routines
ds <- df[,c(which(colnames(df) == "Daily_routines"), which(colnames(df) == "Daily_routinesW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p11 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Daily routines") + 
    xlab("Time")) 

# Distractions
ds <- df[,c(which(colnames(df) == "Distractions"), which(colnames(df) == "DistractionsW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p12 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Distractions") + 
    xlab("Time")) 

# Generalized anxiety
ds <- df[,c(which(colnames(df) == "Generalized_anxiety"), which(colnames(df) == "Generalized_anxietyW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p13 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Generalized anxiety") + 
    xlab("Time")) 

# Emotional loneliness
ds <- df[,c(which(colnames(df) == "Emotional_loneliness"), which(colnames(df) == "Emotional_lonelinessW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p14 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Emotional loneliness") + 
    xlab("Time")) 

# Social loneliness
ds <- df[,c(which(colnames(df) == "Social_loneliness"), which(colnames(df) == "Social_lonelinessW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p15 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Social loneliness") + 
    xlab("Time")) 

# Quality of social contacts
ds <- df[,c(which(colnames(df) == "social_contacts"), which(colnames(df) == "social_contactsW2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p16 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Quality of social contacts") + 
    xlab("Time")) 

# Extraversion
ds <- df[,c(which(colnames(df) == "Extraversion_now"), which(colnames(df) == "Extraversion_W2"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p17 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Extraversion") + 
    xlab("Time")) 

# Quality of sleep
ds <- df[,c(which(colnames(df) == "Quality_of_Sleep_1"), which(colnames(df) == "Quality_of_Sleep_12"))]
colnames(ds) <- c("t1", "t2")
ds$ID <- 1:192
ds <- melt(ds, id.vars = "ID")
ds <- na.omit(ds)
summary(lmer(value ~ 1+ variable+(1|ID), data = ds)) # variable = time (t1 and t2)
round(confint(lmer(value ~ 1+ variable+(1|ID), data = ds)),3) # variable = time (t1 and t2)
(p18 <- ggplot(ds, aes(x = variable, y = value)) +
    stat_summary(
      geom = "point",
      fun = "mean",
      col = "red",
      size = 3)  +
    geom_violin(alpha = .1, col = "darkgrey", lwd = 1.2) +
    geom_point(position = position_jitter(h = 0, w = .1)) +
    geom_boxplot(alpha = 0.2, width = .4, notch=TRUE, col = "blue") +
    theme_light(base_size = 25) +
    ylab("Quality of sleep") + 
    xlab("Time")) 

ggsave("Within-subject comparisons1.png", plot = grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, nrow = 3), width = 35, height = 40, units = "cm", dpi = 600) # Save plot in high resolution
ggsave("Within-subject comparisons2.png", plot = grid.arrange(p10, p11, p12, p13, p14, p15, p16, p17, p18, ncol = 3, nrow = 3), width = 35, height = 40, units = "cm", dpi = 600) # Save plot in high resolution
