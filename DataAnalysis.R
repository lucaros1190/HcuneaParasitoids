
# Data analysis of H. cunea parasitoids
# Created by Luca Rossini on 27 February 2025
# Last update 2 March 2025
# E-mail: luca.rossini@ulb.be


# ANALYSIS 1: Plots of the development rate functions

# Acquisition of the dataset C. cunea - File 'DatasetComplete.xlsx'

library(readxl)

DatasetPath <- file.choose()

DevRate_CC = read_excel(DatasetPath, sheet = 'DevRate-CC', col_names = T)

head(DevRate_CC)


# Best fit parameters from Python curvefit

  # Females - Brière

# a = 5.74e-05 +/- 4.3e-06
# T low = 12.5738978 +/- 0.0848004
# T max = 33.0 +/- 0.3364957
# m = 2.4320271 +/- 0.1746293

a_CCF <- 5.74e-05
TL_CCF <- 12.5738978
TM_CCF <- 33.0
m_CCF <- 1 / 2.4320271

  # Females - Linear

sl_CCF <- 0.00294
int_CCF <- -0.0352

  # Males - Brière

# a = 5.77e-05 +/- 2.25e-05
# T low = 11.0781743 +/- 0.4008828
# T max = 33.0 +/- 2.1068521
# m = 2.8950669 +/- 1.2144429

a_CCM <- 5.77 * 10^(-5)
TL_CCM <- 11.0781743
TM_CCM <- 33.0
m_CCM <- 1 / 2.8950669

  # Males - Linear

sl_CCM <- 0.00276
int_CCM <- -0.03014 


  # Set the temperature range for the best fit lines

x_fit = seq(10, 35, by = 0.1)


# Boxplot - CC females

library(ggplot2)

boxPlot_Fit_CCF <- ggplot(DevRate_CC, aes(x=TempFem, y=DevRate_Fem,
                                           fill=as.factor(TempFem))) + 
  geom_boxplot(width=0.5, aes(group = TempFem, fill = "Experimental data")) + 
  xlab("Temperature (°C)") + 
  ylab("Development rate (1/day)") + 
  ggtitle("CC Females") +
  theme(plot.title = element_text(hjust=0.5), 
        text = element_text(size=21), legend.position = "left") +
  
  stat_function(
    fun = function(x_fit) a_CCF * x_fit * (x_fit - TL_CCF) * (TM_CCF - x_fit) ^ (m_CCF),
    aes(color = "Non-linear fit", group = 1),
    size = 0.7, xlim = c(0, 33)) +
  
  stat_function(
    fun = function(x_fit) sl_CCF * x_fit + int_CCF,
    aes(color = "Linear fit", group = 1),
    size = 0.7, xlim = c(0, 33)) + 
  
  scale_fill_manual(name = " ", values = c("Experimental data" = "skyblue")) +
  scale_colour_manual(name = " ", values = c("Non-linear fit" = "purple",
                                             "Linear fit" = "orange")) +
  scale_y_continuous(limits = c(0, 0.05))

boxPlot_Fit_CCF


# Boxplot - CC males

boxPlot_Fit_CCM <- ggplot(DevRate_CC, aes(x=TempMale, y=DevRate_Male,
                                          fill=as.factor(TempMale))) + 
  geom_boxplot(width=0.5, aes(group = TempMale, fill = "Experimental data")) + 
  xlab("Temperature (°C)") + 
  ylab("Development rate (1/day)") + 
  ggtitle("CCMales") +
  theme(plot.title = element_text(hjust=0.5), 
        text = element_text(size=21), legend.position = "left") +
  
  stat_function(
    fun = function(x_fit) a_CCM * x_fit * (x_fit - TL_CCM) * (TM_CCM - x_fit) ^ (m_CCM),
    aes(color = "Non-linear fit", group = 1),
    size = 0.7, xlim = c(0, 33)) +
  
  stat_function(
    fun = function(x_fit) sl_CCM * x_fit + int_CCM,
    aes(color = "Linear fit", group = 1),
    size = 0.7, xlim = c(0, 33)) + 
  
  scale_fill_manual(name = " ", values = c("Experimental data" = "skyblue")) +
  scale_colour_manual(name = " ", values = c("Non-linear fit" = "purple",
                                             "Linear fit" = "orange")) +
  scale_y_continuous(limits = c(0, 0.05))

boxPlot_Fit_CCM



# Acquisition of the dataset P. omnivorus - File 'DatasetComplete.xlsx'

DevRate_PO = read_excel(DatasetPath, sheet = 'DevRate-PO', col_names = T)

head(DevRate_PO)


# Best fit parameters from Python curvefit

  # Females - Brière

# a = 1.06e-05 +/- 4.9e-06
# T low = 11.646896 +/- 0.1783259
# T max = 36.2492463 +/- 1.1899721
# m = 0.9032453 +/- 0.1171198

a_POF <- 1.06e-05
TL_POF <- 11.646896
TM_POF <- 36.2492463
m_POF <- 1 / 0.9032453

# Females - Linear

sl_POF <- 0.00372
int_POF <- -0.04492


# Males

# a = 1.04e-05 +/- 2.4e-06
# T low = 10.1363514 +/- 0.204065
# T max = 36.0 +/- 0.5673854
# m = 0.9200658 +/- 0.0638349

a_POM <- 1.04 * 10^(-5)
TL_POM <- 10.1363514
TM_POM <- 36.0
m_POM <- 1 / 0.9200658

# Males - Linear

sl_POM <- 0.00315
int_POM <- -0.03241 

# Set the temperature range for the best fit lines

x_fit = seq(10, 35, by = 0.1)


# Boxplot - CC females

library(ggplot2)

boxPlot_Fit_POF <- ggplot(DevRate_PO, aes(x=TempFem, y=DevRate_Fem,
                                          fill=as.factor(TempFem))) + 
  geom_boxplot(width=0.5, aes(group = TempFem, fill = "Experimental data")) + 
  xlab("Temperature (°C)") + 
  ylab("Development rate (1/day)") + 
  ggtitle("PO Females") +
  theme(plot.title = element_text(hjust=0.5), 
        text = element_text(size=21), legend.position = "left") +
  
  stat_function(
    fun = function(x_fit) a_POF * x_fit * (x_fit - TL_POF) * (TM_POF - x_fit) ^ (m_POF),
    aes(color = "Non-linear fit", group = 1),
    size = 0.7, xlim = c(0, 36)) +
  
  stat_function(
    fun = function(x_fit) sl_POF * x_fit + int_POF,
    aes(color = "Linear fit", group = 1),
    size = 0.7, xlim = c(0, 36)) + 
  
  scale_fill_manual(name = " ", values = c("Experimental data" = "skyblue")) +
  scale_colour_manual(name = " ", values = c("Non-linear fit" = "purple",
                                             "Linear fit" = "orange")) +
  scale_y_continuous(limits = c(0, 0.06))

boxPlot_Fit_POF


# Boxplot - CC males

boxPlot_Fit_POM <- ggplot(DevRate_PO, aes(x=TempMale, y=DevRate_Male,
                                          fill=as.factor(TempMale))) + 
  geom_boxplot(width=0.5, aes(group = TempMale, fill = "Experimental data")) + 
  xlab("Temperature (°C)") + 
  ylab("Development rate (1/day)") + 
  ggtitle("PO Males") +
  theme(plot.title = element_text(hjust=0.5), 
        text = element_text(size=21), legend.position = "left") +
  
  stat_function(
    fun = function(x_fit) a_POM * x_fit * (x_fit - TL_POM) * (TM_POM - x_fit) ^ (m_POM),
    aes(color = "Non-linear fit", group = 1),
    size = 0.7, xlim = c(0, 36)) +
  
  stat_function(
    fun = function(x_fit) sl_POM * x_fit + int_POM,
    aes(color = "Linear fit", group = 1),
    size = 0.7, xlim = c(0, 36)) + 
  
  scale_fill_manual(name = " ", values = c("Experimental data" = "skyblue")) +
  scale_colour_manual(name = " ", values = c("Non-linear fit" = "purple",
                                             "Linear fit" = "orange")) +
  scale_y_continuous(limits = c(0, 0.06))

boxPlot_Fit_POM



# Second part of the analysis - C. CUNEA

DataTemp_CC = read_excel(DatasetPath, sheet = 'CC-Temperature', col_names = T)

head(DataTemp_CC)


  # Kruskal Wallis - Lifetime fecundity

kruskal.test(Fecundity ~ Temperature, data = DataTemp_CC)

  # Dunn post-hoc test

library(FSA)

PH_LTFec_CC = dunnTest(Fecundity ~ as.factor(Temperature), data = DataTemp_CC, method="bh") 
PH_LTFec_CC 

library(rcompanion)

  # Letters

PH_LTFec_CC_pVal <- PH_LTFec_CC$res

cldList(comparison = PH_LTFec_CC_pVal$Comparison,
        p.value    = PH_LTFec_CC_pVal$P.adj,
        threshold  = 0.05)


  # Kruskal Wallis - Adults' emergence

kruskal.test(DataTemp_CC$`Adults'emergence` ~ Temperature, data = DataTemp_CC)

# Dunn post-hoc test

PH_AdEm_CC = dunnTest(DataTemp_CC$`Adults'emergence` ~ as.factor(Temperature), data = DataTemp_CC, method="bh") 
PH_AdEm_CC 

library(rcompanion)

# Letters

PH_AdEm_CC_pVal <- PH_AdEm_CC$res

cldList(comparison = PH_AdEm_CC_pVal$Comparison,
        p.value    = PH_AdEm_CC_pVal$P.adj,
        threshold  = 0.05)


  # Offspring produced

DataOffspring_CC = read_excel(DatasetPath, sheet = 'CC-Offspring', col_names = T)

head(DataOffspring_CC)


GenLin_Off_CC <- glm(PercentageOffspring ~ as.factor(Temperature) + as.factor(Sex), data = DataOffspring_CC)

# Inspect the results:

summary(GenLin_Off_CC)

# Pairwise comparison

library(multcompView)
library(emmeans)

marginal_GLM_Off_CC = emmeans(GenLin_Off_CC, ~ Temperature + Sex)
pairs(marginal_GLM_Off_CC, adjust="bonferroni")

# Letters of significance:

library(multcomp)

letters_GLM_Off_CC <- cld(marginal_GLM_Off_CC, alpha=0.05, Letters=letters, 
                   adjust="bonferroni")
letters_GLM_Off_CC


# Plot of the data

barplot_Off_CC <- ggplot(DataOffspring_CC, aes(x=as.factor(Temperature), y=PercentageOffspring, fill=Sex)) +
  xlab("Temperature (°C)") + 
  ylab("Mean number of offspring") +
  
  # Barre affiancate con stat_summary
  stat_summary(fun=mean, geom="bar", width=0.4, position=position_dodge(width=0.5)) +
  
  # Barre di errore
  stat_summary(fun.data=mean_se, geom="errorbar", 
               position=position_dodge(width=0.5), width=0.2, colour="black", size=1) +
  
  theme_linedraw() +
  scale_x_discrete(label = c("10", "15", "20", "25", "30")) +
  theme(legend.position = "right", plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle=0), text = element_text(size=25)) +
  
  scale_fill_manual(values = c("Male" = "purple", "Female" = "darkorange"))

barplot_Off_CC




# Second part of the analysis - P. OMNIVORUS

DataTemp_PO = read_excel(DatasetPath, sheet = 'PO-Temperature', 
                         col_names = T)

head(DataTemp_PO)


# Kruskal Wallis - Lifetime fecundity

kruskal.test(Fecundity ~ Temperature, data = DataTemp_PO)

# Dunn post-hoc test

PH_LTFec_PO = dunnTest(Fecundity ~ as.factor(Temperature), data = DataTemp_PO, 
                       method="bh") 
PH_LTFec_PO 

# Letters

PH_LTFec_PO_pVal <- PH_LTFec_PO$res

cldList(comparison = PH_LTFec_PO_pVal$Comparison,
        p.value    = PH_LTFec_PO_pVal$P.adj,
        threshold  = 0.05)


# Kruskal Wallis - Adults' emergence

kruskal.test(DataTemp_PO$`Adults'emergence` ~ Temperature, data = DataTemp_PO)

# Dunn post-hoc test

PH_AdEm_PO = dunnTest(DataTemp_PO$`Adults'emergence` ~ as.factor(Temperature), 
                      data = DataTemp_PO, method="bh") 
PH_AdEm_PO 

# Letters

PH_AdEm_PO_pVal <- PH_AdEm_PO$res

cldList(comparison = PH_AdEm_PO_pVal$Comparison,
        p.value    = PH_AdEm_PO_pVal$P.adj,
        threshold  = 0.05)


# Offspring produced

DataOffspring_PO = read_excel(DatasetPath, sheet = 'PO-Offspring', 
                              col_names = T)

head(DataOffspring_PO)


GenLin_Off_PO <- glm(PercentageOffspring ~ as.factor(Temperature) + 
                       as.factor(Sex), data = DataOffspring_PO)

# Inspect the results:

summary(GenLin_Off_PO)

# Pairwise comparison

marginal_GLM_Off_PO = emmeans(GenLin_Off_PO, ~ Temperature + Sex)
pairs(marginal_GLM_Off_PO, adjust="bonferroni")

# Letters of significance:

letters_GLM_Off_PO <- cld(marginal_GLM_Off_PO, alpha=0.05, Letters=letters, 
                          adjust="bonferroni")
letters_GLM_Off_PO


barplot_Off_PO <- ggplot(DataOffspring_PO, aes(x=as.factor(Temperature), 
                                               y=PercentageOffspring, fill=Sex)) +
  xlab("Temperature (°C)") + 
  ylab("Mean number of offspring") +
  
  stat_summary(fun=mean, geom="bar", width=0.4, position=position_dodge(width=0.5)) +
  
  stat_summary(fun.data=mean_se, geom="errorbar", 
               position=position_dodge(width=0.5), width=0.2, colour="black", size=1) +
  
  theme_linedraw() +
  scale_x_discrete(label = c("10", "15", "20", "25", "30")) +
  theme(legend.position = "right", plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle=0), text = element_text(size=25)) +
  
  scale_fill_manual(values = c("Male" = "purple", "Female" = "darkorange"))

barplot_Off_PO






