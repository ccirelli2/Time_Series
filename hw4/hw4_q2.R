# Documentation _____________________________________________
' Analyse the DoctorVisits data using a Poisson regression for the 
  number of visits. Is the Possion model satisfactory?  If not, 
  where are the problems and what could be done about them?
'

# Clear namespace -------------------------------------------
rm(list=ls())

# Load Libraries --------------------------------------------
library(AER)

# Load data -------------------------------------------------
data(DoctorVisits)

# Inspect Data ----------------------------------------------
' Observations:   

  Target is not normally distributed.  
  Var > mean indicates over dispersion.
  Likely not a good candidate for a Poisson Regression model
  as the data deviates from the required assumptions. 
'
col_names = names(DoctorVisits)
col_names
summary(DoctorVisits)
hist(DoctorVisits$visits)
mean(DoctorVisits$visits)
var(DoctorVisits$visits)    # Variance > mean


# Fit Poisson Model
m.poisson <- glm(visits ~ ., DoctorVisits, family=poisson(link='log'))
summary(m.poisson)

# Interpretation
' Significance:  Pvalues indicate that genderfemal, income, illenss, 
                 reduced, health and freepooryes are significant and 
                 affect the response variable. 
                 
  Disperson:      Residual Deviance > dof, which indicates over 
                 dispersion. 
  
  Model is not satisfactory. 
'

# Check for overdispersion
dispersiontest(m.poisson, alternative='greater')
' Output

  data:  m.poisson
  z = 6.5386, p-value = 3.105e-11
  alternative hypothesis: true dispersion is greater than 1
  sample estimates:
  dispersion 
  1.415602 
'






