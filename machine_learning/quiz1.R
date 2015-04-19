# quiz 1 

# q5 
# Suppose that we have created a machine learning algorithm that predicts whether a link will be 
# clicked with 99% sensitivity and 99% specificity. 
# The rate the link is clicked is 1/1000 of visits to a website. 
# If we predict the link will be clicked on a specific visit, what is the probability it will 
# actually be clicked?

# sensistivity = P(positive test | click)
# specificity = P(negative test | no click)
P_T_GIVEN_C = .99
P_C = .001
P_C_AND_T = P_C*P_T_GIVEN_C
# we have 
P_C = 1/1000
P_T_GIVEN_C = .99
P_NOT_T_GIVEN_NOT_C = .99
P_T_AND_C = P_T_GIVEN_C*P_C
P_NOT_T_AND_NOT_C = P_NOT_T_GIVEN_NOT_C * (1-P_C)

# need: P(C|T)
# P(C|T) = P(C&T)/P(T)

# P(T) = P(C&T) + P(^C&T) 
# P(^C) = P(^C&^T) + P(^C&T)
P_NOT_C = 1-P_C
P_NOT_C_AND_T = P_NOT_C - P_NOT_T_AND_NOT_C
P_T = P_NOT_C_AND_T + P_C_AND_T

P_C_AND_T/P_T # 9%
