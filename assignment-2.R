# HW assignment 2

# ------------------ Q4) ------------------

# The required vectors are defined below 
S = c(1:12)
S
p = c(1/2^c(1:11),1/2^11)
p

# For Part (a),(b) & (c) we use the following condition to verify independence -
# P(A).P(B) = P(A∩B)

# ***** part (a) ***** 

A = c(3,6,9)
B = c(3,6,10,11,12)
sum(p[A])*sum(p[B])==sum(p[intersect(A,B)]) 

# Ans : FALSE => Events are Not Independent 

# ***** part (b) ***** 

A = c(3,6,9)
B = c(1,6,9,11,12)
sum(p[A])*sum(p[B])==sum(p[intersect(A,B)])

# Ans : FALSE => Events are Not Independent 

# ***** part (c) *****

A = c(1,6,9)
B = c(1,6,9,11,12)
sum(p[A])*sum(p[B])==sum(p[intersect(A,B)])

# Ans : FALSE => Events are Not Independent 

# For part (d),(e) & (f), we use the following formula for conditional probability
# P(A|B) = P(A∩B)/P(B)

# ***** part (d) *****

A = S[S<9] 
B = S[S>5]
sum(p[intersect(A,B)])/sum(p[B])

# Ans : 0.875

# ***** part (e) *****

A = S[S>3 & S<9]
B = S[S<6]
sum(p[intersect(A,B)])/sum(p[B])

# Ans : 0.09677419

# ***** part (f) *****

A = S[S>3 & S<9]
B = S[S>=2]
sum(p[intersect(A,B)])/sum(p[B])

# Ans : 0.2421875




