#Identify your file path
file_path <- "C:/Users/Bryant Deleon/Desktop/OPR 9750/Project/Salary_Data.csv"

#create the variable that holds the CSV file
salary_data <- read.csv(file_path)

#Identify missing Data
sum(is.na(salary_data))      #gives ten
which(is.na(salary_data))    #want to know which ones are the rows that are missing

########################################Cleaning up the Data#############################################################
salary_data <- na.omit(salary_data)

#checking the unique values per columns
names(salary_data)
unique(salary_data$Gender)
unique(salary_data$Job.Title)
unique(salary_data$Education.Level)


#Cleaning up the Education Level Column
library(dplyr)
salary_data <- salary_data %>%
  mutate(Education.Level = case_when(
    Education.Level == "Bachelor's" ~ "Bachelor's Degree",
    Education.Level == "Master's" ~ "Master's Degree",
    Education.Level == "phD" ~ "PhD",
    Education.Level == "" ~ "Unknown",
    TRUE ~ Education.Level
  ))

unique(salary_data$Education.Level)

# Giving Education Levels and Oder
custom_order <- c("High School", "Bachelor's Degree", "Master's Degree", "PhD")
salary_data$Education.Level <- factor(salary_data$Education.Level, levels = custom_order)
unique(salary_data$Education.Level)

#New experience Bins based on job title
experience_bins <- c(0, 5, 10, 15, 20, 25)
experience_labels <- c("0-5 years", "6-10 years", "11-15 years", 
                       "16-20 years", "21-25 years")


salary_data$Experience_Category <- cut(salary_data$Years.of.Experience, 
                                       breaks = experience_bins, 
                                       labels = experience_labels, 
                                       include.lowest = TRUE, right = TRUE)

names(salary_data)

# New Age Categories based on job title
Age_bins <- c(0, 10, 20, 30, 40, 50, 60, 62)
Age_labels <- c("0-10 years", "11-20 years", "21-30 years",
                "31-40 years", "41-50 years", "51-60 years", "60-62 years")
salary_data$Age_Bins <- cut(salary_data$Age,
                            breaks = Age_bins,
                            labels = Age_labels,
                            include.lowest = TRUE, right = TRUE)

names(salary_data)


#New Department_Function Categories based on job title
salary_data <- salary_data %>%
  mutate(Department_Function = case_when(
    grepl("Marketing|Social Media", Job.Title) ~ "Marketing",
    grepl("Engineer|Developer|IT", Job.Title) ~ "Technology and Development",
    grepl("Data|Analyst|Business Intelligence", Job.Title) ~ "Data and Analytics",
    grepl("HR|Human Resources|Recruiter", Job.Title) ~ "Human Resources",
    grepl("Finance|Accountant|Financial", Job.Title) ~ "Finance and Accounting",
    grepl("Administrative|Customer Service|Support", Job.Title) ~ "Administration and Support",
    grepl("Sales|Business Development|Account Manager", Job.Title) ~ "Sales and Business Development",
    TRUE ~ "Other"
  ))

head(salary_data)
unique(salary_data$Department_Function)

#New Seniority_Level Categories based on job title
unique(salary_data$Job.Title)

salary_data <- salary_data %>%
  mutate(Seniority_Level = case_when(
    grepl("Entry|Support|Rep|Coordinator|Assistant|Analyst|Sales Executive|Receptionist|Recruiter|Delivery Drive|Accountant|Digital Content Producer|Financial Advisor|Digital Content Producer", Job.Title) ~ "Entry-Level",
    grepl("Junior|Generalist|Specialist|Developer|Engineer|Designer", Job.Title) ~ "Junior-Level",
    grepl("Senior|Manager|Principle|Associate|Scientist|Social Media Man", Job.Title) ~ "Senior-Level",
    grepl("CEO|Director|VP|Chief", Job.Title) ~ "Executive/Leadership",
    grepl("Technical Writer|Strategy Consultant|Copywriter|UX Researcher", Job.Title) ~ "Specialized",
  ))

custom_order2 <- c("Entry-Level","Junior-Level","Senior-Level","Executive/Leadership","Specialized")

salary_data$Seniority_Level <- factor(salary_data$Seniority_Level, levels = custom_order2)


levels(salary_data$Seniority_Level)
summary(salary_data$Seniority_Level)
table(salary_data$Seniority_Level)

na_seniority_job_titles <- salary_data %>%
  filter(is.na(Seniority_Level)) %>%
  select(Job.Title)
na_seniority_job_titles



#New Specialization Categories based on job title
salary_data <- salary_data %>%
  mutate(Specialization = case_when(
    grepl("Data Scientist|Software Engineer|Technical Writer|Developer|Engineer|Architect", Job.Title) ~ "Specialized Technical Roles",
    grepl("Graphic Designer|UX Designer|Creative Director|Product Designer|Web Designer|Copywriter|Content|Advertising", Job.Title) ~ "Creative and Design Roles",
    grepl("Research|Product Development|Scientist|Researcher", Job.Title) ~ "Research and Development",
    grepl("Marketing|Social Media|Public Relations|Digital Marketing|Marketing Coordinator|Marketing Manager|Marketing Analyst|Marketing Specialist|Event Coordinator", Job.Title) ~ "Marketing and Communications",
    grepl("Sales|Business Development|Account Manager|Sales Associate|Sales Executive|Sales Representative|Sales Manager|Sales Director", Job.Title) ~ "Sales and Business Development",
    grepl("HR|Human Resources|Recruiter|HR Generalist|HR Manager|HR Coordinator|HR Specialist", Job.Title) ~ "Human Resources",
    grepl("Finance|Accountant|Financial|Financial Analyst|Financial Manager|Financial Advisor|Accounting", Job.Title) ~ "Finance and Accounting",
    grepl("IT Support|Help Desk|Network Engineer|IT Manager|IT Consultant|IT Project Manager", Job.Title) ~ "IT and Technical Support",
    grepl("Operations|Project Manager|Operations Manager|Operations Analyst|Operations Director|Project Coordinator|Project Engineer|Supply Chain", Job.Title) ~ "Operations and Project Management",
    grepl("CEO|Director|VP|Executive|Principal|Chief|Manager|Coordinator|Consultant|Specialist|Advisor|Analyst", Job.Title) ~ "Management and Leadership",
    grepl("Customer Service|Customer Support|Customer Success|Receptionist", Job.Title) ~ "Customer Service and Support",
    TRUE ~ "General"
  ))


head(salary_data)

names(salary_data)

salary_data$Specialization <- factor(salary_data$Specialization)
levels(salary_data$Specialization)
summary(salary_data$Specialization)


general_specialization <- filter(salary_data, Specialization == "General")
job_titles_general <- unique(general_specialization$Job.Title)
print(job_titles_general)

#New Salary Bins Categories 
summary(salary_data$Salary)

salary_bins <- c(0, 50000, 100000, 150000, 200000, 250000)
salary_labels <- c("<$50,000", "$50,000-$100,000", "$100,000-$150,000", "$150,000-$200,000", ">$200,000")

salary_data$Salary_Category <- cut(salary_data$Salary, 
                                   breaks = salary_bins, 
                                   labels = salary_labels, 
                                   include.lowest = TRUE)

########################################Association Analysis################################################################################

library(ggplot2)
library(regclass)
library(RColorBrewer)


hist(salary_data$Salary)
qq(salary_data$Salary)

#Specialization vs Salary numerical (Categorical VS Continuous)

ggplot(salary_data , aes(x = Specialization, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Job Title",x = "Specialization", y = "Salary") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(salary_data, aes(x = Specialization, y = Salary)) +
  geom_boxplot() +  
  geom_jitter(width = 0.2, alpha = 0.5) +  
  labs(title = "Salary Distribution by Job Title",x = "Specialization", y = "Salary") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

associate(Salary~Specialization,data=salary_data,seed=2015)

#Regression
Salary_Specialization_model <- lm(Salary ~ Specialization, data = salary_data)
Salary_Specialization_model
summary(Salary_Specialization_model)

#Specialization vs Salary (Categorical VS Categorical)

ggplot(salary_data, aes(x = Salary_Category, fill = Specialization)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Distribution of Specializations Across Salary Categories",x = "Salary Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

ggplot(salary_data, aes(x = Salary_Category, fill = Specialization)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribution of Specializations Across Salary Categories",x = "Salary Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), legend.text = element_text(size = 8), legend.position = "bottom")  


associate(Salary_Category~Specialization,data=salary_data,seed=2015)

#Regression
sepcilization_contingency_table <- table(salary_data$Specialization, salary_data$Salary_Category)
chi_squared_result <- chisq.test(sepcilization_contingency_table )
print(chi_squared_result)






#Experience vs Salary (categorical VS Continuous)

ggplot(salary_data, aes(x = Experience_Category, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Years of Experience",x = "Years of Experience Category", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

associate(Salary~Experience_Category,data=salary_data,seed=2015,permutations=1500)

#Regression
Salary_Experience_Category_model <- lm(Salary ~ Experience_Category, data = salary_data)
Salary_Experience_Category_model
summary(Salary_Experience_Category_model)


#Experience vs Salary  (Continuous VS Continuous)
ggplot(salary_data, aes(x = Years.of.Experience, y = Salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Association between Salary and Years of Experience",
       x = "Years of Experience",
       y = "Salary") +
  theme_minimal()

associate(Salary~Years.of.Experience,data=salary_data,seed=2015,permutations=1500)

#Regression
Salary_Experience_continous_model <- lm(Salary ~ Years.of.Experience, data = salary_data)
Salary_Experience_continous_model
summary(Salary_Experience_continous_model)



#Seniority_Level vs Salary (Categorical vs. Numerical)
summary(salary_data$Seniority_Level)

average_salaries_by_seniority <- salary_data %>%
  group_by(Seniority_Level) %>%
  summarize(Average_Salary = mean(Salary, na.rm = TRUE))


hist(x = Seniority_Level, y = Salary, data = salary_data)

ggplot(salary_data, aes(x = Seniority_Level, y = Salary)) +
  geom_boxplot() +
  geom_point(data = average_salaries_by_seniority, aes(y = Average_Salary), 
             shape = 8, # Star shape
             color = "blue", size = 3) +
  labs(title = "Salary Distribution by Seniority Level",
       x = "Seniority Level", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


associate(Salary~Seniority_Level,data=salary_data) 


###############Regression - Seniority vs. Salary 

Salary_Seniority_model <- lm(Salary ~ Seniority_Level, data = salary_data); confint(Salary_Seniority_model, level = 0.95)
summary(Salary_Seniority_model)



#Education Level vs Salary (Categorical vs. Numerical)

## removing the one NA in Education.Level
cleaned_education <- salary_data %>%
  filter(!is.na(Education.Level))

## Boxplot
ggplot(cleaned_education, aes(x = Education.Level, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Education Level",
       x = "Education Level", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Histogram
ggplot(cleaned_education, aes(x = Salary_Category, fill = Education.Level)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribution of Education Across Salary Categories",x = "Salary Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), legend.text = element_text(size = 8), legend.position = "bottom")  


associate(Salary ~ Education.Level, data = cleaned_education,permutations=1500)

###############Regression - Education vs. Salary 

Salary_Education_model <- lm(Salary ~ Education.Level, data = cleaned_education)
summary(Salary_Education_model)




#Gender vs Salary
filtered_salary_data <- subset(salary_data, Gender != "Other")

ggplot(filtered_salary_data, aes(x = Gender, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Gender (Excluding 'Other')",
       x = "Gender", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

associate(Salary~Gender,data=salary_data,seed=2015,permutations=1500)


Salary_gender_model <- lm(Salary ~ Gender, data = salary_data)
Salary_gender_model
summary(Salary_gender_model)


names(salary_data)

#Cross Regression 
salary_model_all <- lm(Salary ~ Years.of.Experience + Education.Level + Specialization + Gender, data = salary_data)
summary(salary_model_all)




################################################Did not use ######################################################
#Age vs Salary
ggplot(salary_data, aes(x = Age_Bins, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Age Bin",
       x = "Age Bin", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


associate(Salary ~ Age_Bins, data = salary_data,seed=2015,permutations=1500)



#Department_Function vs Salary
ggplot(salary_data, aes(x = Department_Function, y = Salary)) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Department/Function",
       x = "Department/Function", y = "Salary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

associate(Salary~Department_Function,data=salary_data,seed=2015,permutations=1500)

