library(readr)
library(DescTools)
library(likert)
library(RColorBrewer)
library(patchwork)
library(rcompanion)
library(lattice)

# This line is only necessary for RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Define the Likert scale used in the questionnaire
importanceLevels = c("Not important", "Slightly important", "Moderately important", "Quite important", "Essential")
frequencyLevels = c("Almost never", "Once in a while", "Sometimes", "Often", "Almost always")
confidenceLevels = c("Not at all confident", "Slightly confident", "Moderately confident", "Quite confident", "Completely confident")

# Read in the answers using the factors defined above
# The expected format is as follows:
# - one column for each question, with the header being the question itself
# - one row for the answers of each student, using the levels defined above
#
# The questions and associated scales are as follows.
#
# | Question                                                                                                                                                                                                 | Scale      |
# |----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+------------|
# | How important have the implementations in Isabelle been for your understanding of the course topics?                                                                                                     | Importance |
# | How important has the reading material been for your understanding of the course topics?                                                                                                                 | Importance |
# | How important have the lectures been for your understanding of the course topics?                                                                                                                        | Importance |
# | How important do you think it is to experiment with your own examples when learning about a new concept?                                                                                                 | Importance |
# | When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)                                                                  | Frequency  |
# | How confident are you that you could implement a system introduced in this course (without any formal proofs) in a programming language of your choice using the Isabelle implementation as a reference? | Confidence |
# | How confident are you that you can design your own formal logical system to solve a practical problem?                                                                                                   | Confidence |
# | How confident are you that you can implement your own formal logical system in a programming language of your choice?                                                                                    | Confidence |
# | How confident are you that you can prove your own implementation of a formal logical system to be correct?                                                                                               | Confidence |
# | How confident did you feel with functional programming before starting the course?                                                                                                                       | Confidence |
# | How confident in your abilities have you felt when doing the exercises and assignments in this course?                                                                                                   | Confidence |
# | How confident do you feel with functional programming now?                                                                                                                                               | Confidence |

answers <- read_csv("answers.csv", col_types = list("How important have the implementations in Isabelle been for your understanding of the course topics?" = col_factor(importanceLevels, ordered=TRUE),
                                                    "How important has the reading material been for your understanding of the course topics?" = col_factor(importanceLevels, ordered=TRUE),
                                                    "How important have the lectures been for your understanding of the course topics?" = col_factor(importanceLevels, ordered=TRUE),
                                                    "How important do you think it is to experiment with your own examples when learning about a new concept?" = col_factor(importanceLevels, ordered=TRUE),
                                                    'When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)' = col_factor(frequencyLevels, ordered=TRUE),
                                                    .default = col_factor(confidenceLevels, ordered = TRUE)))

# We start by visualizing the dataset
importanceAnswers <- likert(as.data.frame(answers[1:4]))
frequencyAnswers <- likert(as.data.frame(answers[5]))
confidenceAnswers <- likert(as.data.frame(answers[6:12]))
impPlot <- plot(importanceAnswers,type="bar",group.order=names(as.data.frame(answers[1:4]))) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = importanceLevels,drop=FALSE) +
  guides(fill = guide_legend(title="Responses:"))
freqPlot <- plot(frequencyAnswers,type="bar",group.order=names(as.data.frame(answers[5]))) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = frequencyLevels,drop=FALSE) +
  guides(fill = guide_legend(title="Responses:"))
confPlot <- plot(confidenceAnswers,type="bar",group.order=names(as.data.frame(answers[6:12]))) +
  scale_fill_manual(values = brewer.pal(n=5,"RdYlBu"), breaks = confidenceLevels,drop=FALSE) +
  guides(fill = guide_legend(title="Responses:"))

(impPlot / freqPlot / confPlot) + plot_layout(ncol=1, height=c(4,1,7))
ggsave("answer-summary.pdf", device="pdf", width=15.99, height = 20, units = "cm", dpi = 300, scale = 1.8)

# HYPOTHESIS 1:
# Concrete implementations in a programming language aids understanding of
# concepts in logic.
# 
# ANALYSIS PLAN:
# Questions 1-3 ask about the perceived importance of the three elements of the course.
# We would like to compare the relative importance of the three elements to
# determine whether the implementations are an important part of the understanding.
# 
# If we had a measure of student learning outcomes, it would be interesting
# to perform a relative importance analysis on the three factors, but due to
# the anonymity of the questionnaire, we are not able to couple it to e.g. grades.
# 
# Instead, we simply qualitatively compare the results of the questionnaire.
# We see that students generally feel that implementations are much more important
# than the lectures, and slightly more important than the reading material.
barplot(rbind(table(answers$`How important have the implementations in Isabelle been for your understanding of the course topics?`),
              table(answers$`How important has the reading material been for your understanding of the course topics?`),
              table(answers$`How important have the lectures been for your understanding of the course topics?`)),
        col = c("red","green","blue"),beside=TRUE,legend.text=c("Implementations", "Reading", "Lectures"))
# The median student thinks that implementations are quite important, reading
# material moderately important, and lectures slightly important.
median(as.numeric(answers$`How important have the implementations in Isabelle been for your understanding of the course topics?`))
median(as.numeric(answers$`How important has the reading material been for your understanding of the course topics?`))
median(as.numeric(answers$`How important have the lectures been for your understanding of the course topics?`))
#
# RESULT:
# We conclude that the hypothesis seems to be plausible.


# HYPOTHESIS 2:
# Students experiment with definitions to gain understanding.
#
# ANALYSIS PLAN:
# Question 4 asks whether students think it is important to experiment with their
# own examples when learning about a new concept.
# Question 5 asks whether students actually evaluate concrete examples to gain
# understanding when they are using Isabelle.
# Descriptively, we can look at the frequencies to gain a basic understanding.
# We can also measure the association between the two variables.

# We see that all find it at least slightly important, while most students find
# it at least quite important, to experiment with examples while learning.
barplot(table(answers$`How important do you think it is to experiment with your own examples when learning about a new concept?`))
#
# The median student finds it essential.
median(as.numeric(answers$`How important do you think it is to experiment with your own examples when learning about a new concept?`))
#
# We see that there seems to be two distinct groups when we ask whether students
# actually do experiment with concrete examples in Isabelle.
barplot(table(answers$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`))
# We can look at the median to determine whether student generally do experiments.
median(as.numeric(answers$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`))
# 
# RESULT:
# The median student often experiments with concrete examples, confirming the
# hypothesis.
#
# POST HOC ANALYSIS:
# We can think of two possible reasons that a group of students almost never
# experiment with concrete examples in Isabelle: either there are a group of
# students who simply had not understood that this is possible, or some students
# do not find experimentation valuable enough to actually do it outside of the
# abstract.
#
# To investigate this further, we can look at the distribution of answers about
# whether students think that experimentation is important for students who
# answered that they almost never evaluate concrete examples in Isabelle and
# students who answered otherwise.
# H0: students who answered "almost never" are more, or equally likely to think
# experimentation is important.
# H1: students who answered "almost never" are less likely to think
# experimentation is important.
# We can split the data set in "Almost never" and "Other" and look at the
# distribution of how important students think experimentation is for each category.
answersNoAlmostNever <- answers[-which(answers$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`=="Almost never"),]
answersOnlyAlmostNever <- answers[which(answers$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`=="Almost never"),]
barplot(rbind(table(answersOnlyAlmostNever$`How important do you think it is to experiment with your own examples when learning about a new concept?`),
              table(answersNoAlmostNever$`How important do you think it is to experiment with your own examples when learning about a new concept?`)),
        col = c("red", "green"), beside=TRUE, legend.text = c("Almost never", "Others"),
              main="How important do you think it is to experiment with your own examples when learning about a new concept?",
              args.legend = list(title = "How often do you evaluate concrete examples in Isabelle?"))
# While the number of answers is too low to really show a definitive result, we
# note that the shapes of the two distributions do not seem to differ significantly.
# This suggests to reject the alternate hypothesis H1.
# 
# We can also measure the association between students who think that experimentation
# is important and students who often evaluate concrete examples in Isabelle.
# Selection criteria for association measure:
# - the variables don't have the same categories
# - both categories are naturally ranked from low to high
# - we are interested in question 4 as a predictor for question 5
# - there is no possibility of multiple choices
# We will use Somers' Delta statistic to measure association.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"

SomersDelta(answers$`How important do you think it is to experiment with your own examples when learning about a new concept?`,
            answers$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`,
            conf.level = 0.95)
# The value is -0.1039 (CI95%: -0.4777 -- 0.2699), which shows a small negative
# association between the variables, i.e. that students who find experimentation
# more important evaluate concrete examples slightly less often than ones who do
# not. This is surprising.
# If we assume that the answers of "almost never" are due to students not knowing
# about the feature, and thus remove them from the dataset as special cases, the
# association becomes -0.0814 (CI95%: -0.5046 -- 0.3418), which again shows a
# small negative association.
SomersDelta(answersNoAlmostNever$`How important do you think it is to experiment with your own examples when learning about a new concept?`,
            answersNoAlmostNever$`When using Isabelle, how often do you evaluate your own concrete examples to understand new concepts? (E.g. using the "value" command.)`,
            conf.level = 0.95)

# While there is not enough data to give good confidence intervals, it seems
# like there is no statistically significant association between the perceived
# importance of experimentation and the frequency of evaluating concrete
# examples in Isabelle.
# Note, however, that students still generally tend to believe that experimentation
# is important, and that they generally tend to evaluate their own concrete examples
# in Isabelle.
# One possible explanation is that some students who believe that experiments
# are essential are used to doing experiments with pen-and-paper, and thus do
# this instead of using Isabelle to evaluate examples, but we do not have data
# to support this hypothesis.

# HYPOTHESIS 3:
# Our formalizations make it clear to students how to implement the concepts in
# practice.
# 
# ANALYSIS PLAN:
# Question 6 asks directly about how the students perceive their abilities to do
# this.

# We can look at the frequencies of answers to get a feel of the general perception
# among students.
barplot(table(answers$`How confident are you that you could implement a system introduced in this course (without any formal proofs) in a programming language of your choice using the Isabelle implementation as a reference?`))
# While most students are not at all, or only slightly confident, there are also
# a number of students who are quite, or completely confident.
# 
# The median student, however, is only slightly confident that they could implement
# the concepts in practice.
median(as.numeric(answers$`How confident are you that you could implement a system introduced in this course (without any formal proofs) in a programming language of your choice using the Isabelle implementation as a reference?`))
#
# RESULT:
# This suggests to reject the hypothesis.
# 
# POST HOC ANALYSIS:
# The bimodality is interesting, since we know that many students did not have
# experience with functional programming previous to our course (even though it
# was an explicitly stated prerequisite).
# We can test whether there is an association between previous functional
# programming ability and perceived ability to implement the concepts in practice.
# We have data for perceived programming ability both before and after the course,
# so we can analyze both.
#
# Selection criteria for association measure:
# - the variables do have the same categories
# - both categories are naturally ranked from low to high
# - we are interested in questions 10 and 12 as predictors for question 6
# - there is no possibility of multiple choices
# We will use Somers' Delta statistic to measure association.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"
#
# For previous functional programming ability, the value is 0.194 (95% CI: -0.139--0.527).
SomersDelta(answers$`How confident did you feel with functional programming before starting the course?`,
            answers$`How confident are you that you could implement a system introduced in this course (without any formal proofs) in a programming language of your choice using the Isabelle implementation as a reference?`,
            conf.level = 0.95)
# This suggests that there is an association, but that it is barely significant.
# Ref: "Christopher J. Ferguson: An Effect Size Primer: A Guide for Clinicians and Researchers"
#
# For current functional programming ability, the value is 0.394 (95% CI: 0.0807--0.707).
SomersDelta(answers$`How confident do you feel with functional programming now?`,
            answers$`How confident are you that you could implement a system introduced in this course (without any formal proofs) in a programming language of your choice using the Isabelle implementation as a reference?`,
            conf.level = 0.95)
# This suggests a small to moderate, but statistically significant association.
# Ref: "Christopher J. Ferguson: An Effect Size Primer: A Guide for Clinicians and Researchers"
#
# We conclude that it seems that our formalizations do help students
# understand how to implement the concepts in practice, but only significantly
# so if they were confident functional programmers after the course.

# HYPOTHESIS 4:
# Our course makes students able to design and implement their own logical systems
#
# ANALYSIS PLAN:
# The meaning of this hypothesis is a bit vague, so we divide it into multiple
# concrete questions.
# Question 7 asks about perception of ability to design a formal system to solve
# a practical problem.
# Question 8 asks about perception of ability to implement a formal system in
# any programming language (without proofs).
# Question 9 asks about perception of ability to prove a formal system correct.
#
# We can look at the frequencies of answers to get a feel for the situation.
#
# The general trend is that student do not feel that they have the ability to
# design formal systems to prove practical problems.
barplot(table(answers$`How confident are you that you can design your own formal logical system to solve a practical problem?`))
# The median student is only slightly confident in their ability to do this.
median(as.numeric(answers$`How confident are you that you can design your own formal logical system to solve a practical problem?`))
# This suggests to reject the hypothesis.
# 
# The general trend is that students do not feel that they have the ability to
# implement their own logical systems (in any programming language).
barplot(table(answers$`How confident are you that you can implement your own formal logical system in a programming language of your choice?`))
# The median student is only slightly confident in their ability to do this.
median(as.numeric(answers$`How confident are you that you can implement your own formal logical system in a programming language of your choice?`))
# This suggests to reject the hypothesis.
#
# The general trend is that students are only slightly to moderately confident
# that they have the ability to prove a formal system correct.
barplot(table(answers$`How confident are you that you can prove your own implementation of a formal logical system to be correct?`))
# The median student is only slightly confident in their ability to do this.
median(as.numeric(answers$`How confident are you that you can prove your own implementation of a formal logical system to be correct?`))
# This suggests to reject the hypothesis.
# (But note that this is very difficult, and so we never expected students to
# be very confident in their ability to do this.)
# 
# RESULT:
# The data suggests to reject the hypothesis.
# 
# POST HOC ANALYSIS:
# It could be interesting to measure the association between perceived functional
# programming ability and perceived ability to implement logical systems.
# We have data for perceived programming ability both before and after the course,
# so we can analyze both.
#
# Selection criteria for association measure:
# - the variables do have the same categories
# - both categories are naturally ranked from low to high
# - we are interested in questions 10 and 12 as predictors for question 8
# - there is no possibility of multiple choices
# We will use Somers' Delta statistic to measure association.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"
#
# For previous functional programming ability, the value is 0.108 (95% CI: -0.259--0.474).
SomersDelta(answers$`How confident did you feel with functional programming before starting the course?`,
            answers$`How confident are you that you can implement your own formal logical system in a programming language of your choice?`,
            conf.level = 0.95)
# This suggests that there is no statistically significant association.
# Ref: "Christopher J. Ferguson: An Effect Size Primer: A Guide for Clinicians and Researchers"
#
# For current functional programming ability, the value is 0.419 (95% CI: 0.0445--0.794).
SomersDelta(answers$`How confident do you feel with functional programming now?`,
            answers$`How confident are you that you can implement your own formal logical system in a programming language of your choice?`,
            conf.level = 0.95)
# This suggests small to moderate association.
# Ref: "Christopher J. Ferguson: An Effect Size Primer: A Guide for Clinicians and Researchers"
#
# We conclude that it seems that our formalizations do help students
# understand how to implement logical systems, but only if they are confident
# functional programmers after the course.

# HYPOTHESIS 5:
# Prior experience with functional programming is useful for our course
#
# ANALYSIS PLAN:
# We ask about perceived ability with functional programming before starting the
# course and measure the association with perceived ability during the exercises
# and assignments in the course.

# Frequencies:
# We see that there is an approximately normal distribution of students who feel
# moderately to completely confident as well as a second mode of students who are
# not at all confident.
# We know that this second mode consists of students who did not have any experience
# with functional programming prior to the course (although it was an explicit
# prerequisite).
barplot(table(answers$`How confident did you feel with functional programming before starting the course?`))
# 
# The median student felt quite confident with functional programming before
# starting the course.
median(as.numeric(answers$`How confident did you feel with functional programming before starting the course?`))
#
# We see that nobody felt completely confident, while most students felt slightly
# to quite confident.
barplot(table(answers$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`))
#
# The median student felt moderately confident when doing the exercises and
# assignments.
median(as.numeric(answers$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`))
#
# We can measure the association between perceived ability with functional
# programming before starting the course and perceived ability during the
# exercises and assignments in the course.
#
# Selection criteria for association measure:
# - the variables do have the same categories
# - both categories are naturally ranked from low to high
# - we are interested in question 10 as a predictor for question 11
# - there is no possibility of multiple choices
# We will use Somers' Delta statistic to measure the association
SomersDelta(answers$`How confident did you feel with functional programming before starting the course?`,
            answers$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`,
            conf.level = 0.95)
# The value is 0.3642 (95% CI: 0.0038 -- 0.7247), which shows a small to moderate
# association between students who felt confident with functional programming
# before starting the course and students who felt confident during the exercises
# and assignments.
# Ref: "Christopher J. Ferguson: An Effect Size Primer: A Guide for Clinicians and Researchers"
#
# RESULT: 
# We confirm the hypothesis.
# 
# POST HOC ANALYSIS:
# If we look at the two modes independently, the students with the func. prog.
# prerequisite tend to feel moderately to quite confident...
answersFuncProgPrereq <- answers[-which(answers$`How confident did you feel with functional programming before starting the course?`=="Not at all confident"),]
barplot(table(answersFuncProgPrereq$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`))
# ...while most students without the func. prog. prerequisite felt only slightly
# confident.
answersNoFuncProgPrereq <- answers[which(answers$`How confident did you feel with functional programming before starting the course?`=="Not at all confident"),]
barplot(table(answersNoFuncProgPrereq$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`))
# This also supports our hypothesis.

# We can try to remove the students with no functional programming experience
# to see whether the course requires advanced functional programming skills.
SomersDelta(answersFuncProgPrereq$`How confident did you feel with functional programming before starting the course?`,
            answersFuncProgPrereq$`How confident in your abilities have you felt when doing the exercises and assignments in this course?`,
            conf.level = 0.95)
# The value is 0.0000 (95% CI: -0.5019 -- 0.5019), which we interpret as meaning
# that only basic knowledge of functional programming is needed in our course.
# This tracks well with our design choices in our systems, where we deliberately
# avoid "advanced" concepts such as folds in our implementations and exercises.

# HYPOTHESIS 6:
# Our course helps students gain proficiency in functional programming
#
# ANALYSIS PLAN:
# Question 10 asks about perceived ability before starting the course, while
# question 12 asks about perceived ability towards the end of the course.
# We can test whether the probability of high perceived ability has increased
# during the course by comparing the two answers for each student.
#
# Frequencies:
# We see that a number of students have no confidence (presumably due to them
# missing the functional programming prerequisite).
barplot(table(answers$`How confident did you feel with functional programming before starting the course?`))
# After the course, we obtain what looks more like a normal distribution centered
# around the "quite confident" answer (although it is quite skewed).
barplot(table(answers$`How confident do you feel with functional programming now?`))
#
# We can perform a two-sample paired signed-rank test (i.e. a paired
# Wilcoxon signed-rank test) to determine whether there is a significant
# difference in perceived functional programming ability before and after the
# course.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"
wilcox.test(as.numeric(answers$`How confident did you feel with functional programming before starting the course?`),
            as.numeric(answers$`How confident do you feel with functional programming now?`),
            paired = TRUE,
            conf.int = TRUE,
            exact=FALSE)
# The test shows V = 2 (95% CI: -3.5 -- 0.00), with p = 0.0498.
# Since p <= 0.05, we reject the null hypothesis, i.e. we conclude that there is
# a statistically significant difference in perceived functional programming
# ability before and after the course.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"
#
# The effect size of the paired Wilcoxon signed-rank test can be measured by
# the matched-pairs rank biserial correlation coefficient.
# Ref: "Salvatore S. Mangiafico: Summary and Analysis of Extension Program Evaluation in R"
timeTable <- make.groups(as.numeric(answers$`How confident did you feel with functional programming before starting the course?`),
                         as.numeric(answers$`How confident do you feel with functional programming now?`))
wilcoxonPairedRC(x = timeTable$data, g = timeTable$which,
                 conf = 0.95,
                 ci = TRUE,
                 R = 10000)
# The statistic is -0.857 (95% CI: -1.00 -- -0.333), which suggests that the
# course has a large positive effect on perceived functional programming ability.
# 
# RESULT:
# We confirm the hypothesis.
#
# POST HOC ANALYSIS:
# We can try to remove the students with no functional programming experience
# to see whether there is still a significant difference for those students
# who already have functional programming experience.
wilcox.test(as.numeric(answersFuncProgPrereq$`How confident did you feel with functional programming before starting the course?`),
            as.numeric(answersFuncProgPrereq$`How confident do you feel with functional programming now?`),
            paired = TRUE,
            conf.int = TRUE,
            exact=FALSE)
# The test shows V = 1, with p = 1. This means there is no statistically
# significant difference in perceived functional programming ability for the
# group of students who already have functional programming experience.
# In fact, by inspection of the dataset we see that every student has answered
# exactly the same for both questions, except one student who moved from feeling
# "Quite confident" to feeling only "Moderately confident" after having taken
# the course.

