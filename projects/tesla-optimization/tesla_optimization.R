#Math Project: Mabisa Chhetry, Harshada Pujari, Khushboo Surana
#Date: 12/09/2023

library(alabama)

# ANSWER 1: Objective function to minimize 
objective_function <- function(x) (60* (2* x["full_time_english_7_9"] + 2* x["full_time_spanish_7_9"] +
                        2*x["full_time_english_9_11"] + 2* x["full_time_spanish_9_11"] +
                        2* x["full_time_english_11_1"] + 2* x["full_time_spanish_11_1"]+
                        x["full_time_english_1_3"] + x["full_time_spanish_1_3"] +
                        x["full_time_english_3_5"] + x["full_time_spanish_3_5"] + x["part_time_english_3_5"]) +
                        90 * (2 *x["part_time_english_5_7"]+x["part_time_english_3_5"]+
                                 x["full_time_english_1_3"]+ x["full_time_spanish_1_3"]+
                           +x["full_time_english_3_5"]+ x["full_time_spanish_3_5"]))

constraint_equal <- function(x) {
  # equality constraints
    h=0
    h[1]= x["full_time_english_7_9"] + x["full_time_spanish_7_9"] - 8    # 7 A.M. – 9 A.M.
    h[2]= x["full_time_english_9_11"] + x["full_time_spanish_9_11"] - 15   # 9 A.M. – 11 A.M.
    h[3]= x["full_time_english_11_1"] + x["full_time_spanish_11_1"]+ x["full_time_english_7_9"]+x["full_time_spanish_7_9"] - 13   # 11 A.M. – 1 P.M.
    h[4]= x["full_time_english_1_3"] + x["full_time_spanish_1_3"]+ x["full_time_english_9_11"]+ x["full_time_spanish_9_11"] - 17    # 1 P.M. – 3 P.M.
    h[5]= x["full_time_english_3_5"] + x["full_time_spanish_3_5"] + x["part_time_english_3_5"]+x["full_time_english_11_1"]+ x["full_time_spanish_11_1"] - 14   # 3 P.M. – 5 P.M.
    h[6]= x["part_time_english_5_7"]+ x["part_time_english_3_5"] +x["full_time_english_1_3"]+ x["full_time_spanish_1_3"] - 7    # 5PM - 7PM
    h[7]= x["part_time_english_5_7"]+x["full_time_english_3_5"]+ x["full_time_spanish_3_5"] - 3    # 7PM - 9PM
    return(h)
}

# Constraints
constraint_inequal <- function(x) {
  # Inequality constraints
  h=0
  h[1]= 6 * x["full_time_english_7_9"] - 32    # 7 A.M. – 9 A.M.
  h[2]= 6 * x["full_time_spanish_7_9"] - 8    # 7 A.M. – 9 A.M.
  h[3]= 6 * x["full_time_english_9_11"] - 68    # 9 A.M. – 11 A.M.
  h[4]= 6 * x["full_time_spanish_9_11"] - 17   # 9 A.M. – 11 A.M.
  h[5]= 6 * x["full_time_english_11_1"] + 6 * x["full_time_english_7_9"] - 56   # 11 A.M. – 1 P.M.
  h[6]= 6 * x["full_time_spanish_11_1"] + 6 * x["full_time_spanish_7_9"] - 14   # 11 A.M. – 1 P.M.
  h[7]= 6 * x["full_time_english_1_3"] + 6 * x["full_time_english_9_11"]- 76    # 1 P.M. – 3 P.M.
  h[8]= 6 * x["full_time_spanish_1_3"] + 6 * x["full_time_spanish_9_11"] - 19    # 1 P.M. – 3 P.M.
  h[9]= 6 * x["full_time_english_3_5"] + 6 * x["part_time_english_3_5"]+ 6 * x["full_time_english_11_1"]- 64   # 3 P.M. – 5 P.M.
  h[10]= 6 * x["full_time_spanish_3_5"] + 6 * x["full_time_spanish_11_1"] - 16   # 3 P.M. – 5 P.M.
  h[11]= 6 * x["part_time_english_5_7"] + 6 * x["part_time_english_3_5"] + 6 * x["full_time_english_1_3"]- 28    # 5PM - 7PM
  h[12]= 6 * x["full_time_spanish_1_3"] - 7    # 5PM - 7PM
  h[13]= 6 * x["part_time_english_5_7"] + 6 * x["full_time_english_3_5"] - 8    # 7PM - 9PM
  h[14]= 6 * x["full_time_spanish_3_5"] - 2    # 7PM - 9PM
  h[15]= x["full_time_english_7_9"]
  h[16]= x["full_time_spanish_7_9"]
  h[17]= x["full_time_english_9_11"]
  h[18]= x["full_time_spanish_9_11"]
  h[19]= x["full_time_english_11_1"]
  h[20]= x["full_time_spanish_11_1"]
  h[21]= x["full_time_english_1_3"]
  h[22]= x["full_time_spanish_1_3"]
  h[23]= x["full_time_english_3_5"]
  h[24]= x["full_time_spanish_3_5"]
  h[25]= x["part_time_english_3_5"]
  h[26]= x["part_time_english_5_7"]
  return(h)
}

# Initial values for the decision variables
initial_values = c(
  full_time_english_7_9 = 6,
  full_time_spanish_7_9 = 2,
  full_time_english_9_11 = 12,
  full_time_spanish_9_11 = 3,
  full_time_english_11_1 = 4,
  full_time_spanish_11_1 = 2,
  full_time_english_1_3 = 1,
  full_time_spanish_1_3 = 2,
  full_time_english_3_5 = 4,
  full_time_spanish_3_5 = 1,
  part_time_english_3_5 = 3,
  part_time_english_5_7 = 2
)


 
# Solve the optimization problem with constraints
result = constrOptim.nl(initial_values, objective_function,
                        heq = constraint_equal, hin = constraint_inequal)  
result$par


# Answer A
cat("\n Full-time English-speaking agents, full-time Spanish-speaking agents, \n
    and part-time agents Alex should hire for each 2-hour shift to minimize operating costs are: \n")
ceiling(result$par)

#Answer B
result$value
cat("\n Minimum Cost for the Optimization Model: $", round(result$value, 2), "\n")





# ANSWER 2: Objective function to minimize
a_objective_function <- function(x) (60* (2* x["full_time_english_7_9"] + 2* x["full_time_spanish_7_9"] +
                                          2*x["full_time_english_9_11"] + 2* x["full_time_spanish_9_11"] +
                                          2* x["full_time_english_11_1"] + 2* x["full_time_spanish_11_1"]+
                                          x["full_time_english_1_3"] + x["full_time_spanish_1_3"] +
                                          x["full_time_english_3_5"] + x["full_time_spanish_3_5"] + x["part_time_english_3_5"]) +
                                     90 * (x["part_time_english_5_7"] + x["part_time_english_5_7"]+x["part_time_english_3_5"]+
                                             x["full_time_english_1_3"]+ x["full_time_spanish_1_3"]+
                                             +x["full_time_english_3_5"]+ x["full_time_spanish_3_5"]))

a_constraint_equal <- function(x) {
  # equality constraints
  h=0
  h[1]= x["full_time_english_7_9"] + x["full_time_spanish_7_9"] - 8    # 7 A.M. – 9 A.M.
  h[2]= x["full_time_english_9_11"] + x["full_time_spanish_9_11"] - 15   # 9 A.M. – 11 A.M.
  h[3]= x["full_time_english_11_1"] + x["full_time_spanish_11_1"]+ x["full_time_english_7_9"]+x["full_time_spanish_7_9"] - 13   # 11 A.M. – 1 P.M.
  h[4]= x["full_time_english_1_3"] + x["full_time_spanish_1_3"]+ x["full_time_english_9_11"]+ x["full_time_spanish_9_11"] - 17    # 1 P.M. – 3 P.M.
  h[5]= x["full_time_english_3_5"] + x["full_time_spanish_3_5"] + x["part_time_english_3_5"]+x["full_time_english_11_1"]+ x["full_time_spanish_11_1"] - 14   # 3 P.M. – 5 P.M.
  h[6]= x["part_time_english_5_7"]+ x["part_time_english_3_5"] +x["full_time_english_1_3"]+ x["full_time_spanish_1_3"] - 7    # 5PM - 7PM
  h[7]= x["part_time_english_5_7"]+x["full_time_english_3_5"]+ x["full_time_spanish_3_5"] - 3    # 7PM - 9PM
  # New constraint: Only one full-time English agent for 1 PM - 3 PM
  h[8] = x["full_time_english_1_3"] - 1

  return(h)
}

# Constraints
a_constraint_inequal <- function(x) {
  # Inequality constraints
  h=0
  h[1]= 6 * x["full_time_english_7_9"] - 32    # 7 A.M. – 9 A.M.
  h[2]= 6 * x["full_time_spanish_7_9"] - 8    # 7 A.M. – 9 A.M.
  h[3]= 6 * x["full_time_english_9_11"] - 68    # 9 A.M. – 11 A.M.
  h[4]= 6 * x["full_time_spanish_9_11"] - 17   # 9 A.M. – 11 A.M.
  h[5]= 6 * x["full_time_english_11_1"] + 6 * x["full_time_english_7_9"] - 56   # 11 A.M. – 1 P.M.
  h[6]= 6 * x["full_time_spanish_11_1"] + 6 * x["full_time_spanish_7_9"] - 14   # 11 A.M. – 1 P.M.
  h[7]= 6 * x["full_time_english_1_3"] + 6 * x["full_time_english_9_11"]- 76    # 1 P.M. – 3 P.M.
  h[8]= 6 * x["full_time_spanish_1_3"] + 6 * x["full_time_spanish_9_11"] - 19    # 1 P.M. – 3 P.M.
  h[9]= 6 * x["full_time_english_3_5"] + 6 * x["part_time_english_3_5"]+ 6 * x["full_time_english_11_1"]- 64   # 3 P.M. – 5 P.M.
  h[10]= 6 * x["full_time_spanish_3_5"] + 6 * x["full_time_spanish_11_1"] - 16   # 3 P.M. – 5 P.M.
  h[11]= 6 * x["part_time_english_5_7"]+ 6 * x["part_time_english_3_5"] + 6 * x["full_time_english_1_3"]- 28    # 5PM - 7PM
  h[12]= 6 * x["full_time_spanish_1_3"] - 7    # 5PM - 7PM
  h[13]= 6 * x["part_time_english_5_7"]+ 6 * x["full_time_english_3_5"] - 8    # 7PM - 9PM
  h[14]= 6 * x["full_time_spanish_3_5"] - 2    # 7PM - 9PM
  h[15]= x["full_time_english_7_9"]
  h[16]= x["full_time_spanish_7_9"]
  h[17]= x["full_time_english_9_11"]
  h[18]= x["full_time_spanish_9_11"]
  h[19]= x["full_time_english_11_1"]
  h[20]= x["full_time_spanish_11_1"]
  h[21]= x["full_time_english_1_3"]
  h[22]= x["full_time_spanish_1_3"]
  h[23]= x["full_time_english_3_5"]
  h[24]= x["full_time_spanish_3_5"]
  h[25]= x["part_time_english_3_5"]
  h[26]= x["part_time_english_5_7"]

  return(h)
}

# Initial values for the decision variables
a_initial_values = c(
  full_time_english_7_9 = 6,
  full_time_spanish_7_9 = 2,
  full_time_english_9_11 = 12,
  full_time_spanish_9_11 = 3,
  full_time_english_11_1 = 4,
  full_time_spanish_11_1 = 2,
  full_time_english_1_3 = 1,
  full_time_spanish_1_3 = 2,
  full_time_english_3_5 = 4,
  full_time_spanish_3_5 = 1,
  part_time_english_3_5 = 3,
  part_time_english_5_7 = 2
)



# Solve the optimization problem with constraints
a_result = constrOptim.nl(a_initial_values, a_objective_function,
                          heq = a_constraint_equal, hin = a_constraint_inequal)  
a_result$par
#Answer C
cat("\n Full-time English-speaking agents, full-time Spanish-speaking agents, and part-time agents should Alex hire \n
    when only one qualified English-speaking agent willing to start work at 1 P.M. and 3 P.M are:\n")
ceiling(a_result$par)


#Answer D
a_result$value
cat("\nMinimum Cost for the Optimization Model: $", round(a_result$value, 2), "\n")





library(alabama)

# ANSWER 3: Objective function to minimize
bilin_objective_function <- function(x) (60* (2* x["full_time_7_9"] +
                                          2*x["full_time_9_11"] +
                                          2* x["full_time_11_1"] + 
                                          x["full_time_1_3"] +
                                          x["full_time_3_5"]  + x["part_time_3_5"]) +
                                     90 * (2 *x["part_time_5_7"]+x["part_time_3_5"]+
                                             x["full_time_1_3"]+
                                             +x["full_time_3_5"]))

bilin_constraint_equal <- function(x) {
  # equality constraints
  h=0
  h[1]= x["full_time_7_9"] - 8    # 7 A.M. – 9 A.M.
  h[2]= x["full_time_9_11"]  - 15   # 9 A.M. – 11 A.M.
  h[3]= x["full_time_11_1"] + x["full_time_7_9"] - 13   # 11 A.M. – 1 P.M.
  h[4]= x["full_time_1_3"] + x["full_time_9_11"] - 17    # 1 P.M. – 3 P.M.
  h[5]= x["full_time_3_5"] +  x["part_time_3_5"] + x["full_time_11_1"] - 14   # 3 P.M. – 5 P.M.
  h[6]= x["part_time_5_7"] + x["part_time_3_5"] + x["full_time_1_3"] - 7    # 5PM - 7PM
  h[7]= x["part_time_5_7"] + x["full_time_3_5"] - 3    # 7PM - 9PM
  return(h)
}

# Constraints
bilin_constraint_inequal <- function(x) {
  # Inequality constraints
  h=0
  h[1]= 6 * x["full_time_7_9"] - 40    # 7 A.M. – 9 A.M.
  h[2]= 6 *x["full_time_9_11"]  - 85   # 9 A.M. – 11 A.M.
  h[3]= 6 *x["full_time_11_1"] + 6 *x["full_time_7_9"] - 70   # 11 A.M. – 1 P.M.
  h[4]= 6 *x["full_time_1_3"] + 6 *x["full_time_9_11"] - 95    # 1 P.M. – 3 P.M.
  h[5]= 6 *x["full_time_3_5"] +  6 *x["part_time_3_5"] + 6 *x["full_time_11_1"] - 80   # 3 P.M. – 5 P.M.
  h[6]= 6 *x["part_time_5_7"]+ 6 *x["part_time_3_5"] + 6 *x["full_time_1_3"] - 35    # 5PM - 7PM
  h[7]= 6 *x["part_time_5_7"] + 6 *x["full_time_3_5"] - 10    # 7PM - 9PM
  h[8]= x["full_time_7_9"]
  h[9]= x["full_time_9_11"]
  h[10]= x["full_time_11_1"]
  h[11]= x["full_time_1_3"]
  h[12]= x["full_time_3_5"]
  h[13]= x["part_time_3_5"]
  h[14]= x["part_time_5_7"]
  return(h)
}

# Initial values for the decision variables
bilin_initial_values = c(
  full_time_7_9 = 9,
  full_time_9_11 = 16,
  full_time_11_1 = 5,
  full_time_1_3 = 2,
  full_time_3_5 = 8,
  part_time_3_5 = 2,
  part_time_5_7 = 4
)



# Solve the optimization problem with constraints
bilin_result = constrOptim.nl(bilin_initial_values, bilin_objective_function,
                        heq = bilin_constraint_equal, hin = bilin_constraint_inequal)  
bilin_result$par
#Answer E:
cat("\n Number of bilingual agents Alex should hire: \n")
ceiling(bilin_result$par)

bilin_result$value
#Answer F:
cat("\nMinimum Cost for the Optimization Model (Bilingual): $", round(bilin_result$value, 2), "\n")

# Calculate the maximum percentage increase in the hourly wage rate for bilingual agents
hourly_wage_rate_increase_percentage = (a_result$value - bilin_result$value) / sum(bilin_result$par) * 100
#Answer G:
cat("\nMaximum Percentage Increase in Hourly Wage Rate for Bilingual Agents: ",
    round(hourly_wage_rate_increase_percentage, 1), "%\n")




