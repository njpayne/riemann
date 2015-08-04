# Looking closely at logistic regression
# October 7, 2013

# The UCBA Admissions data set provides aggregate data on applicants to graduate school at 
# Berkeley for the six largest departments in 1973 classified by admission and sex.
# The data is in a 3-dimensional array resulting from cross-tabulating 4526 observations 
# on 3 variables. The variables and their levels are as follows:
# No   Name 	Levels
# 1 	Admit 	Admitted, Rejected
# 2 	Gender 	Male, Female
# 3 	Dept 	A, B, C, D, E, F 

# This is an interesting data set
# This data set is frequently used for illustrating Simpson's paradox, 
# At issue is whether the data show evidence of sex bias in admission practices. There were 2691 
# male applicants, of whom 1198 (44.5%) were admitted, compared with 1835 female applicants 
# of whom 557 (30.4%) were admitted. This gives a sample odds ratio of 1.83, indicating that 
# males were almost twice as likely to be admitted. In fact, graphical methods 
# or log-linear modelling show that the apparent association between admission and sex stems 
# from differences in the tendency of males and females to apply to the individual departments 
# (females used to apply more to departments with higher rejection rates). 

# Let's load in the data
UCBAdmissions <-
  array(c(512, 313,
          89,  19,
          353, 207,
          17,   8,
          120, 205,
          202, 391,
          138, 279,
          131, 244,
          53, 138,
          94, 299,
          22, 351,
          24, 317),
        dim = c(2, 2, 6),
        dimnames =
          list(Admit = c("Admitted", "Rejected"),
               Gender = c("Male", "Female"),
               Dept = c("A", "B", "C", "D", "E", "F")))
class(UCBAdmissions) <- "table"

UCBAdmissions # Print the data tables

# Looked at as a two-way table, there appears to be some bias against admitting women...
dimnames(UCBAdmissions)
margin.table(UCBAdmissions, c(2,1))

# However, there are also relationships between "Gender" and "Dept" as well as between 
# "Dept" and "Admit", which means the above relationship may be confounded by "Dept" 
# (or "Dept" might be a lurking variable). Perhaps a logistic regression with the binomial 
# variable "Admit" as the response can tease these variables apart.

# Let's try to put the flat table into a data frame (without splitting an infinitive)

ucb.df = data.frame(gender=rep(c("Male","Female"),c(6,6)),
                    dept=rep(LETTERS[1:6],2),
                    yes=c(512,353,120,138,53,22,89,17,202,131,94,24),
                    no=c(313,207,205,279,138,351,19,8,391,244,299,317))

ucb.df # Review the data frame
#We do not have a binary coded response variable, so the last two columns of this data frame 
# will have to be bound into the columns of a table to serve as the response in the model formula...

mod.form = "cbind(yes,no) ~ gender * dept"

glm.out = glm(mod.form, family=binomial(logit), data=ucb.df)
# Note that there is a trick used here. what I did was store the model formula in a data object,
# And then I entered the name of this object into the glm( ) function. 
# This is useful because, if I made a mistake in the model formula (or want to run an alternative 
# model), I have only to edit the "mod.form" object to do it. Cool computer science trick!!

options(show.signif.stars=F)         # turn off significance stars (optional)
anova(glm.out, test="Chisq")
# If you look at the model, you will see that this is a saturated model, which means that
# We have used up all our degress of freedom. This leaves us with no residual
# deviance left over at the end. Saturated models always fit the data perfectly. 
# In this case, it appears the saturated model is required to explain the data adequately. 
# If we leave off the interaction term, for example, we will be left with a residual deviance of 
# 20.2 on 5 degrees of freedom, and the model will be rejected (p = .001144). It appears 
# all three terms are making a significant contribution to the model.

# How they are contributing appears if we use the other extractor...
summary(glm.out)

# These are the regression coefficients for each predictor in the model, with the base level of each factor being suppressed. Remember, we are predicting log odds, so to make sense of these coefficients, they need to be "antilogged"...
exp(-1.0521)                         # antilog of the genderMale coefficient
1/exp(-1.0521)

# This shows that men were actually at a significant disadvantage when department 
# and the interaction are controlled. The odds of a male being admitted were only 0.35 
# times the odds of a female being admitted. The reciprocal of this turns it on its head. 
# All else being equal, the odds of female being admitted were 2.86 times the odds of a male 
# being admitted.

# Each coefficient compares the corresponding predictor to the base level. So...

exp(-2.2046)

# Interestingly, the odds of being admitted to department C were only about 1/9th the odds of 
# being admitted to department A, all else being equal. If you want to compare, for example, 
# department C to department D, do this...
exp(-2.2046) / exp(-2.1662)          # C:A / D:A leaves C:D

# This tells us that, all else equal, the odds of being admitted to department C were 0.96 times 
# the odds of being admitted to department D. (To be honest, I'm not sure I'm comfortable with 
# the interaction in this model. You might want to examine the interaction, and if you think it 
# doesn't merit inclusion, run the model again without it. Statistics are nice, but in the end 
# it's what makes sense that should rule the day.)