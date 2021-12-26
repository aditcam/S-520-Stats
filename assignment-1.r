
# HW assignment 1 

# ------------------ Q3) ------------------
# The required vectors are defined below 
X = c(1:8)
A = c(1,2,7,8)
B = c(1,3,4,5,6,8)
C = c(1,2,5,6,7,8)

# Part **** (3a) **** 

# Q) " Is A a subset of B?  Why or why not."

all(A%in%B)

# output : [1] FALSE
# A is not a subset of B because B doesn't have all the elements present in A

# Q) " Is A a subset of C?  Why or why not."

all(A%in%C)

# output : [1] TRUE
# A is not a subset of C because C doesn't have all the elements present in A

# **** Part (3b) ***** 

#"Compute AUB and AnB."

#AUB
union(A,B)
# output: 1 2 7 8 3 4 5 6 

#AnB
intersect(A,B)

# Compute A' and B'

#A'
setdiff(X,A)
# output : [1] 3 4 5 6

#B' 
setdiff(X,B)
# output : [1] 2 7

# Compute A'U B' and A' n B'

#A'UB'
union(setdiff(X,A),setdiff(X,B))
# output : [1] 3 4 5 6 2 7

#A'nB'
intersect(setdiff(X,A),setdiff(X,B))
# output : integer(0)

# Compute (A' U B')' and (A' n B')
A_comp = setdiff(X,A)
B_comp = setdiff(X,B)

#(A' U B')'
setdiff(X,union(A_comp,B_comp))
# output : [1] 1 8

#(A' n B')
setdiff(X,intersect(A_comp,B_comp))
# output : [1] 1 2 3 4 5 6 7 8

# Compute C\A and C\B

#C\A
setdiff(C,A)
# output :[1] 5 6

#C\B
setdiff(C,B)
# output : [1] 2 7

# **** Part (3c) *****

# Defining the new sets

AA = c(1,2,7,8,1)
AAA = c(1,7,8,2)

# Before figuring out if the above vectors define the same set as the previously defined set,
# we must note that for 2 sets to be equal, every element in the first set should be in the second set and every element in the second set should be in the first, the cardinality must be the same as well
# https://math.stackexchange.com/questions/223405/can-elements-in-a-set-be-duplicated , https://math.stackexchange.com/questions/3551881/do-the-elements-of-a-set-have-to-be-unique: I referred to these links to verify that a set cannot contain duplicates, a multiset would be used for that scenario.

# checking the set equality for set A and AA using the above definition
all(c(all(A%in%AA),all(AA%in%A),length(A)==length(AA)))
# output [1]FALSE
# Answer : AA is not a set because it has repeated elements and fails the second condition for set equality even if we assume it to be a valid set 


# checking the set equality for set A and AAA using the above definition
all(c(all(A%in%AAA),all(AAA%in%A),length(A)==length(AAA)))
# output : [1] TRUE
# Answer : AAA and A are equal and are basically the same sets as the only difference in them is the order of the numbers within the set 

# ------------------ Q4) ------------------

# Define X vector
X = c(4,6,8,10,12,20)

# Part **** (4a) **** 

combn(X,3)
# output :      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18]
# [1,]    4    4    4    4    4    4    4    4    4     4     6     6     6     6     6     6     8     8
# [2,]    6    6    6    6    8    8    8   10   10    12     8     8     8    10    10    12    10    10
# [3,]    8   10   12   20   10   12   20   12   20    20    10    12    20    12    20    20    12    20
# [,19] [,20]
# [1,]     8    10
# [2,]    12    12
# [3,]    20    20

# Part **** (4b) **** 

enum = combn(X,3)

#class of prevoious output
class(enum)
# output : [1] "matrix" "array" 

# dimensions of the previous output
dim(enum)
# output : [1] 3 20 

# Part **** (4c) **** 

# Answer : The 'combn' function used above allows us to list all the possible combinations when choosing  'n' out of the entire vector/set where 'n'<= length(vector) i.e. 'n' is smaller than the count of the elements in the vector.
# When there are more constraints on the kinds of combinations we could apply filters using the 'FUN' (function) argument present in the combn(x, m, FUN = NULL, simplify = TRUE, ...) function definition to apply a function to the combinations. 
# To demonstrate using an example,We can use the 'combn' function to solve the Q2b,where we are asked to find out the ' number of ways to choose a set of three dice from the set of six dice ', the command used would be combn(3,V) where V would be a vector consisting of the 6 different dice. 
