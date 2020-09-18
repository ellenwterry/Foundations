library(DMwR)

Emp_Turn <- read.csv("C:/Users/ellen/OneDrive/Documents/UH/Spring 2020/DA2/Section 1/Classification and SVM/Homework/EmpTurn2.csv")

smoteBase = data.frame(dplyr::select(Emp_Turn, 
                     Satisfaction,   
                     Last_Eval,
                     Number_Projects,
                     Avg_Mo_Hrs, 
                     Tenure,
                     Work_Accident,
                     Left,  
                     Promotion,
                     Dept, 
                     Salary))

smoteBase$Left = smoteBase$Left + 1
smoteBase$Left = factor(as.character(smoteBase$Left))
smoteBase$Dept = factor(as.character(smoteBase$Dept))
smoteBase$Salary = factor(as.character(smoteBase$Salary))
smoteData <- SMOTE(Left ~ ., data = smoteBase, perc.over = 350, perc.under=130) # SMOTE only works with factors



