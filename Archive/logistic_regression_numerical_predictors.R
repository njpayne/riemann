# Investigating Logistic Regression
# October 7, 2013

# Recently, a some students from the psychology department conducted a study in which they 
# studied a phenomenon called Inattentional Blindness (IB). IB refers to situations in which a 
#person fails to see an obvious stimulus right in front of his eyes. In their study, my colleagues 
# had subjects view an online video showing college students passing basketballs to each other, 
# and the task was to count the number of times students in white shirts passed the basketball. 
# During the video, a person in a black gorilla suit walked though the picture in a very obvious way. 
# At the end of the video, subjects were asked if they saw the gorilla. Most did not!

# Both of the researchers hypothesized that IB could be predicted from performance on the Stroop Color 
# Word test. This test produces three scores: "W" (word alone, i.e., a score derived from reading a 
# list of color words such as red, green, black), "C" (color alone, in which a score is derived from 
# naming the color in which a series of Xs are printed), and "CW" (the Stroop task, in which a score is 
# derived from the subject's attempt to name the color in which a color word is printed when the word 
# and the color do not agree).

# Create the data frame that holds the raw data
gorilla = data.frame(rep(c(0,1),c(30,19)),
                     c(126,118,61,69,57,78,114,81,73,93,116,156,90,120,99,113,103,123,
                       86,99,102,120,128,100,95,80,98,111,101,102,100,112,82,72,72,
                       89,108,88,116,100,99,93,100,110,100,106,115,120,97),
                     c(86,76,66,48,59,64,61,85,57,50,92,70,66,73,68,110,78,61,65,
                       77,77,74,100,89,61,55,92,90,85,78,66,78,84,63,65,71,46,70,
                       83,69,70,63,93,76,83,71,112,87,82),
                     c(64,54,44,32,42,53,41,47,33,45,49,45,48,49,44,47,52,28,42,51,54,
                       53,56,56,37,36,51,52,45,51,48,55,37,46,47,49,29,49,67,39,43,36,
                       62,56,36,49,66,54,41))
colnames(gorilla) = c("seen","W","C","CW")
str(gorilla)

# Note that we could also do the following to read in the data
# read.csv(file) -> gorilla
# str(gorilla)

# Let's first look for the correlation
cor(gorilla)

# Note that I could achieve the same thing by using the tapply() function
with(gorilla, tapply(W, seen, mean))
with(gorilla, tapply(C, seen, mean))
with(gorilla, tapply(CW, seen, mean))

# As I note from doing the analysis, none of the Stroop scale scores are moderately 
# positively correlated with each other, but none of them appears to be related to the 
# "seen" response variable, at least not to any impressive extent. There doesn't appear to be 
# much here to look at. Thus, we press on. 

# Since the response is a binomial variable, a logistic regression can be done
glm.out = glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)
summary(glm.out) # Generate a summary of the model

# Let's also look for the ANOVA of the model
anova(glm.out, test="Chisq")

# As we see from the initial results, none of the coefficients are significantly different from 
# zero (but a few are close). The deviance was reduced by 8.157 points on 7 degrees of freedom, for a p-value of...
1 - pchisq(8.157, df=7) # Calculate the p-value

# Overall, the model appears to have performed poorly, showing no significant reduction in deviance 
# (no significant difference from the null model). The second print out shows the same overall 
# reduction in deviance, from 65.438 to 57.281 on 7 degrees of freedom. In this print out, 
# however, the reduction in deviance is shown for each term, added sequentially first to last. 
# Of note is the three-way interaction term, which produced a nearly significant reduction in 
# deviance of 3.305 on 1 degree of freedom (p = 0.069).

# Let's look at a graph of the output. How would you interpret it?
png('c:/Users/Nate/Git/riemann/logistic_regression_numerical_predictors_plot.png')
plot(glm.out$fitted,
     main = "IB Plot: Multiple Categorical Predictors")
abline(v=30.5,col="red")
abline(h=.3,col="green")
abline(h=.5,col="green")
text(15,.9,"seen = 0")
text(40,.9,"seen = 1")
dev.off()