# Age (continuous)
summary(data.learn$age)
ggplot(data.learn, aes(age)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Age repartition")
boxplot(age ~ income, data = data.learn, main = "Age distribution depending on income", xlab = "Income", ylab = "Age")
# Children who win more than 50K (only two, 16 and 17 years old)
nb.children <- length(data.learn$age[data.learn$age < 18])
data.learn[data.learn$income == 1 & data.learn$age < 18,]
# See in variable "Education" people are considered children under 15 years old, so no children win more than 50K according to that
# TODO: Maybe add a variable "under15"

# Class of worker (nominal)
summary(data.learn$class.worker)
qplot(income, data = subset(data.learn, data.learn$class.worker != ' Not in universe'), fill = class.worker) + facet_grid (. ~ class.worker) + ggtitle("Number of people earning more or less than 50K in each class of worker") + theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.y = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())
# Very few with the class of worker value to "Not in universe" wins more than 50K (children and those who don't work, ..)
sum((data.learn$class.worker == ' Not in universe'))
sum((data.learn$class.worker == ' Not in universe' & data.learn$income == 1))
# We can see the classes with the highest percentage of people earning +50K are Self-employed-incorporated (34,7%) or Federal government(20.4%)
prop.table(table(data.learn$class.worker, data.learn$income), 1)

# Detailed industry recode (nominal)
summary(data.learn$ind.code)
length(unique(data.learn$ind.code))
# 0 does not correspond to a valid industry code, must be for children and people not working (equivalent to Not in universe?)
qplot(ind.code, data = subset(data.learn, data.learn$ind.code != '0'), fill = income) + ggtitle("Number of people earning more or less than 50K for each detailed industrial recode") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sum(data.learn$ind.code == '0')
# 99.1% of 0 win less than 50K 
prop.table(table(data.learn$ind.code, data.learn$income), 1)

# Detailed occupation recode (nominal)
summary(data.learn$occup.code)
length(unique(data.learn$occup.code))
# 0 does not correspond to a valid industry code, must be for children and people not working (equivalent to Not in universe ?)
qplot(occup.code, data = subset(data.learn, data.learn$occup.code != '0'), fill = income) + ggtitle("Number of people earning more or less than 50K for each detailed occupation recode") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
sum(data.learn$occup.code == '0')
# 99.1% of 0 win less than 50K 
prop.table(table(data.learn$occup.code, data.learn$income), 1)

# Education (nominal)
summary(data.learn$education)
unique(data.learn$education)
qplot(education, data = data.learn, fill = income) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Over 50% of Prof school degree and Doctorate degree(PhD EdD) earn more than 50K
# Masters degree 31.2%, Bachelor's degree 19.7%
prop.table(table(data.learn$education, data.learn$income), 1)
# People are considered as children if they are less than 15 years old
sort(unique(data.learn$age[data.learn$education == ' Children']))

# Wage per hour (continuous)
summary(data.learn$wage.per.hour)
# A lot of zeros : people not winning any money, stats will be done by removing the zeros
# We noticed though that the number of people having a wage per hour = 0 is a bit higher than the number of people winning less than 50K
# We can say that data is missing
length(data.learn$wage.per.hour[data.learn$wage.per.hour == 0])
summary(subset(data.learn, wage.per.hour != 0)$wage.per.hour)
ggplot(subset(data.learn, wage.per.hour != 0), aes(wage.per.hour)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Wage per hour repartition")
boxplot(data.learn$wage.per.hour, data = subset(data.learn, wage.per.hour != 0), main = "Wage per hour", ylab = "Wage per hour")
boxplot(wage.per.hour ~ income, data = subset(data.learn, wage.per.hour != 0), main = "Wage per hour depending on income", xlab = "Income", ylab = "Wage per hour")
# TODO : Processing will have to be done on data, maybe group wage per hour by categories
# 3 groups based on 1st and 3rd Quartile ? (after removing zeros) 
length(data.learn$wage.per.hour[data.learn$wage.per.hour < 584.8])
length(data.learn$wage.per.hour[data.learn$wage.per.hour < 1200 & data.learn$wage.per.hour >= 584.8])
length(data.learn$wage.per.hour[data.learn$wage.per.hour >= 1200 & data.learn$income == 1])

# Enroll in edu inst last week (nominal)
summary(data.learn$enroll.edu.inst.last.wk)
# "Not in universe" meaning they were not enrolled in an educational instance last week
qplot(enroll.edu.inst.last.wk, data = data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on if they were enrolled\n in an educational instance last week")
length(data.learn$income[data.learn$enroll.edu.inst.last.wk != ' Not in universe'] == 1)
length(data.learn$income[data.learn$enroll.edu.inst.last.wk == ' Not in universe'] == 1)
# Most people winning more than 50K were not enrolled in an educational instance last week
prop.table(table(data.learn$enroll.edu.inst.last.wk, data.learn$income), 1)

# Marital status (nominal)
summary(data.learn$marital.stat)
qplot(marital.stat, data = data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on marital status") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$marital.stat, data.learn$income), 1)
# People married-civilian with spouse present are more likely to win more than 50K

# Major industry code (nominal)
summary(data.learn$major.ind.code)
# Generalization of detailed industry code, major industry fields
# A lot of major industry code "Not in universe or children" (corresponds to value "0" in detailed industry recode)
qplot(major.ind.code, data = subset(data.learn, data.learn$major.ind.code != ' Not in universe or children'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on major industry code") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$major.ind.code, data.learn$income), 1)
length(data.learn$income[data.learn$major.ind.code != ' Not in universe'] == 1)
length(data.learn$income[data.learn$major.ind.code == ' Not in universe'] == 1)
# Same info than detailed industry recode, but detailed has too many categories
# so I will only use major industry code in model to avoid bias

# Major occupation code (nominal)
summary(data.learn$major.occup.code)
# Generalization of detailed occupation code, major occupation fields
# A lot of major occupation code "Not in universe" (corresponds to value '0' in detailed occupation recode)
qplot(major.occup.code, data = subset(data.learn, data.learn$major.occup.code != ' Not in universe'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on major occupation code") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$major.occup.code, data.learn$income), 1)
length(data.learn$income[data.learn$major.occup.code != ' Not in universe'] == 1)
length(data.learn$income[data.learn$major.occup.code == ' Not in universe'] == 1)
# Same info than detailed occupation recode, but detailed has too many categories
# so I will only use major occupation code in model to avoid bias

# Race (nominal)
summary(data.learn$race)
qplot(income, data = data.learn, fill = race) + facet_grid(. ~ race) + ggtitle("Number of people earning more or less than 50K depending on race") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$race, data.learn$income), 1)
# High proportions of Asian or Pacific Islander (73.7%) and White (67.3%) winning more than 50K

# Hispanic origin (nominal)
summary(data.learn$hisp.origin)
# All other probably means no hispanic origin
# NA and "Do not know" fields represents unknown hispanic origin for the individuals
# TODO: turn "Do not know" values into NAs
# Same information, factors to be merged
qplot(hisp.origin, data = subset(data.learn, data.learn$hisp.origin != ' All other' & data.learn$hisp.origin != ' NA' & data.learn$hisp.origin != ' Do not know'), 
      fill = income) + ggtitle("Number of people earning more or less than 50K depending on hispanic origin") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$hisp.origin, data.learn$income), 1)

# Sex (nominal)
summary(data.learn$sex)
# More women than man in the dataset
qplot(sex, data = data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on sex") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$sex, data.learn$income), 1)
# Men win a lot more than women (equal pay was way behind in 94/95) 10.2% VS 2.6% win more than 50K

# Member of labor union (nominal)
summary(data.learn$memb.labor.union)
qplot(memb.labor.union, data = subset(data.learn, data.learn$memb.labor.union != ' Not in universe'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on if the individual is a member of a labor union") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$memb.labor.union, data.learn$income), 1)

# Reason of unemployment (nominal)
summary(data.learn$reason.unemployment)
# "Not in universe" value corresponding to people employed or non applicable (children)
qplot(income, data = subset(data.learn, data.learn$reason.unemployment != ' Not in universe'), fill = reason.unemployment) + facet_grid (. ~ reason.unemployment) + ggtitle("Number of people earning more or less than 50K depending on reason of unemployment") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$reason.unemployment, data.learn$income), 1)

# Full or part time employment status (nominal)
summary(data.learn$ft.pt.employ.stat)
# Number of children in dataset (under 15)
nb.children <- nrow(data.learn[data.learn$education == ' Children',])
# Number of people in Armed Forces
nrow(data.learn[data.learn$ft.pt.employ.stat == ' Children or Armed Forces',]) - nb.children
qplot(ft.pt.employ.stat, data = data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on employment status") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$ft.pt.employ.stat, data.learn$income), 1)
# Full time schedules tend to earn more, as well as part-timers who are usually part-timers or who chose it for non-econ reasons

# Capital gains (continuous)
summary(data.learn$capital.gains)
# Too many zeros : people with no capital gains or no capital assets at all, stats will be done by removing the zeros
length(data.learn$capital.gains[data.learn$capital.gains == 0])
summary(subset(data.learn, capital.gains != 0)$capital.gains)
ggplot(subset(data.learn, capital.gains != 0), aes(capital.gains)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Capital gains repartition")
boxplot(capital.gains ~ income, data = subset(data.learn, capital.gains != 0), main = "Capital gains depending on income", xlab = "Income", ylab = "Capital gains")
# Among those who have capital assets, people with higher capital gains tend to win more than 50K
# TODO : Processing will have to be done on data, group capital gains in generalized categories based on visualization
# 3 groups based on 1st and 3rd Quartile ? (after removing zeros) 
length(data.learn$capital.gains[data.learn$capital.gains < 2964  & data.learn$income == 1])
length(data.learn$capital.gains[data.learn$capital.gains < 10520 & data.learn$capital.gains >= 2964 & data.learn$income == 1])
length(data.learn$capital.gains[data.learn$capital.gains >= 10520 & data.learn$income == 1])

# Capital losses (continuous)
summary(data.learn$capital.losses)
# Too many zeros : people with no capital losses or no capital assets at all, stats will be done by removing the zeros
length(data.learn$capital.losses[data.learn$capital.losses == 0])
summary(subset(data.learn, capital.losses != 0)$capital.losses)
ggplot(subset(data.learn, capital.losses != 0), aes(capital.losses)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Capital losses repartition")
boxplot(capital.losses ~ income, data = subset(data.learn, capital.losses != 0), main = "Capital losses depending on income", xlab = "Income", ylab = "Capital losses")
# Among those who have capital assets, people with higher capital losses tend to win more than 50K
# TODO : Processing will have to be done on data, group capital losses in generalized categories based on visualization

# Dividends from stocks (continuous)
summary(data.learn$div.stocks)
# Too many zeros : people with no stock dividends, stats will be done by removing the zeros
length(data.learn$div.stocks[data.learn$div.stocks == 0])
summary(subset(data.learn, div.stocks != 0)$div.stocks)
ggplot(subset(data.learn, div.stocks != 0), aes(div.stocks)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Dividends from stocks repartition")
boxplot(div.stocks ~ income, data = subset(data.learn, div.stocks != 0), main = "Dividends from stocks depending on income", xlab = "Income", ylab = "Dividends from stocks")
# Among those who have dividends from stocks, people with more dividends from stocks tend to win more than 50K
# TODO : Processing will have to be done on data, group number of dividends in stocks in generalized categories based on visualization

# Tax filer status (nominal)
summary(data.learn$tax.filer.stat)
# Nonfiler must correspond to people not winning any money or not winning enough per year to have to file taxes
qplot(income, data=data.learn, fill = tax.filer.stat) + facet_grid (. ~ tax.filer.stat) + ggtitle("Number of people earning more or less than 50K depending on tax filer status") + theme(axis.text.x = element_text(angle = 90, hjust = 1), strip.text.y = element_blank(), strip.background = element_blank(), strip.text.x = element_blank())
prop.table(table(data.learn$tax.filer.stat, data.learn$income), 1)
# Tax filer "joint both under 65" tend to earn more than the others

# Region of previous residence (nominal)
summary(data.learn$region.previous.residence)
# Lots of "Not in the universe" values. They can represent those who didn't move, so no previous residence
qplot(income, data=data.learn, fill = region.previous.residence) + facet_grid (. ~ region.previous.residence) + ggtitle("Number of people earning more or less than 50K depending on previous region of residence") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$region.previous.residence, data.learn$income), 1)

# State of previous residence (nominal)
summary(data.learn$state.previous.residence)
# Lots of "Not in the universe" values (same number as in region). They can represent those who didn't move, so no previous residence
qplot(state.previous.residence, data=data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on previous state of residence") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$state.previous.residence, data.learn$income), 1)
# Same info than region of previous residence, but with too many categories
# so I will only use region of previous residence in model to avoid bias

# Detailed household and family status (nominal)
summary(data.learn$household.family.stat)
qplot(household.family.stat, data=data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on household and family status") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$household.family.stat, data.learn$income), 1)
# Householder tend to earn more than the rest
# Too many categories

# Detailed household summary in household (nominal)
summary(data.learn$household.summary)
qplot(household.summary, data=data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on household summary") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$household.summary, data.learn$income), 1)
# Householder tend to earn more than the rest
# Same info than detailed household and family status, but with less categories
# so I will only use detailed household summary in household in model to avoid bias

# Instance weight (continuous)
# Indicates the number of people in the population that each record represents due to stratified sampling
# Should not be used in classifiers
summary(data.learn$instance.weight)
ggplot(data.learn, aes(instance.weight)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Instance weight repartition")

# Migration code-change in MSA (Metropolitan Statistical Areas) (nominal)
summary(data.learn$migration.code.change.msa)
# Lots of missing values " ?", no migration ?
qplot(migration.code.change.msa, data=subset(data.learn, data.learn$migration.code.change.msa != ' ?'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on migration code-change in MSA") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$migration.code.change.msa, data.learn$income), 1)

# Migration code-change in region (nominal)
summary(data.learn$migration.code.change.reg)
# Lots of missing values " ?" (same number as in migration code-change MSA), no migration ?
qplot(migration.code.change.reg, data=subset(data.learn, data.learn$migration.code.change.reg != ' ?'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on migration code-change in region") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$migration.code.change.reg, data.learn$income), 1)

# Migration code-move within region (nominal)
summary(data.learn$migration.code.move.reg)
# Lots of missing values " ?" (same number as in migration code-change MSA and region), no migration ?
qplot(migration.code.move.reg, data=subset(data.learn, data.learn$migration.code.move.reg != ' ?'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on migration \ncode-move within region") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$migration.code.move.reg, data.learn$income), 1)

# Live in this house 1 year ago (nominal)
summary(data.learn$live.house.1year.ago)
length(data.learn[data.learn$age == 1,])
# "Not in universe" value represents not only children under 1 year old
qplot(live.house.1year.ago, data=subset(data.learn, data.learn$live.house.1year.ago != ' Not in universe under 1 year old'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on if they lived in this house\n 1 year ago") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$live.house.1year.ago, data.learn$income), 1)
# Better odds of winning more if you lived in this house 1 year ago

# Migration previous residence in sunbelt (nominal)
summary(data.learn$sunbelt)
# Lots of missing values " ?" (same number as in migration code-change MSA and region, and migration code-move within region), no migration?
qplot(sunbelt, data=subset(data.learn, data.learn$sunbelt != ' ?'), fill = income) + ggtitle("Number of people earning more or less than 50K depending on migration\nprevious residence in sunbelt") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$sunbelt, data.learn$income), 1)

# Num persons worked for employer (continuous) (to be turned into nominal)
summary(data.learn$num.pers.worked.employer)
unique(factor(data.learn$num.pers.worked.employer))
# Only 7 possible values
# TODO: turn variable into categorical one
qplot(num.pers.worked.employer, data=data.learn, fill = income) + ggtitle("Number of people earning more or less than 50K depending on number\nof persons worked for employer") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$num.pers.worked.employer, data.learn$income), 1)
# The more persons worked for employer, the more the odds of earn more than 50K are high

# Family members if under 18 (nominal)
summary(data.learn$fam.if.under18)
length(data.learn$age[data.learn$age >= 18])
# "Not in universe" value represents those older than 18, although the number of not in universe is higher than the number of person older than 18
# meaning there are some values missing
# Only two persons earning more than 50K under 18 years old, the variable won't be relevant in the prediction of the income
# TODO: ignore it in the model 
qplot(fam.if.under18, data=subset(data.learn, data.learn$fam.if.under18 != ' Not in universe')) + ggtitle("Family members if under 18") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$fam.if.under18, data.learn$income), 1)

# Country of birth of father (nominal)
summary(data.learn$country.birth.father)
# Missing values " ?" -> NAs
qplot(country.birth.father, data=subset(data.learn, data.learn$country.birth.father != ' ?'), fill=income) + ggtitle("Number of people earning more or less than 50K depending on father's\ncountry of birth") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$country.birth.father, data.learn$income), 1)
# TODO: Too much detailed information, better to group countries based on regions
# Edit : var was ignored in classification due to NAs values

# Country of birth of mother (nominal)
summary(data.learn$country.birth.mother)
# Missing values " ?" -> NAs
qplot(country.birth.mother, data=subset(data.learn, data.learn$country.birth.mother != ' ?'), fill=income) + ggtitle("Number of people earning more or less than 50K depending on mother's\ncountry of birth") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$country.birth.mother, data.learn$income), 1)
# TODO: Too much detailed information, better to group countries based on regions
# Edit : var was ignored in classification due to NAs values

# Country of birth of self (nominal)
summary(data.learn$country.birth)
# Missing values " ?" -> NAs
qplot(country.birth, data=subset(data.learn, data.learn$country.birth != ' ?'), fill=income) + ggtitle("Number of people earning more or less than 50K depending on \ncountry of birth") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$country.birth, data.learn$income), 1)
# TODO: Too much detailed information, better to group countries based on regions
# Edit : var was ignored in classification due to NAs values

# Citizenship (nominal)
summary(data.learn$citizenship)
qplot(income, data=data.learn, fill=citizenship) + facet_grid(. ~ citizenship) + ggtitle("Number of people earning more or less than 50K depending on citizenship") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$citizenship, data.learn$income), 1)

# Own business or self employed (nominal)
summary(data.learn$own.business.self.employed)
# Lots of 0, represents those who don't have their own business or aren't self-emplyed
qplot(income, data=subset(data.learn, data.learn$own.business.self.employed != '0'), fill=own.business.self.employed) + facet_grid(. ~ own.business.self.employed) + ggtitle("Number of people earning more or less than 50K depending on own business or sel-employed") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$own.business.self.employed, data.learn$income), 1)

# Fill inc questionnaire for veteran's admin (nominal)
summary(data.learn$questionnaire.vet.admin)
# Lots of "Not in universe" values : probably those who are not veterans
qplot(questionnaire.vet.admin, data=subset(data.learn, data.learn$questionnaire.vet.admin != ' Not in universe'), fill=income) + ggtitle("Number of people earning more or less than 50K depending on if they fill inc \nquestionnaire for veteran's admin") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$questionnaire.vet.admin, data.learn$income), 1)

# Veterans benefits (nominal)
summary(data.learn$veterans.benefits)
# From the numbers, the factor '2' may means "Not in universe" (non veteran ?), although the number is not the same as in the previous variable
qplot(veterans.benefits, data=subset(data.learn, data.learn$veterans.benefits != '2'), fill=income) + ggtitle("Number of people earning more or less than 50K depending on veteran's benefits") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$veterans.benefits, data.learn$income), 1)

# Weeks worked in year (continuous)
summary(data.learn$weeks.worked.year)
ggplot(data.learn, aes(weeks.worked.year)) + geom_density(color = "blue", fill = "blue", alpha = 0.25) + ggtitle("Weeks worked in year repartition")
boxplot(weeks.worked.year ~ income, data = data.learn, main = "Weeks worked in year distribution depending on income", xlab = "Income", ylab = "Weeks worked in year")
# Frequency of weeks worked in year among thos who earn more than 50K
table(data.learn$weeks.worked.year[data.learn$income == '+50K'])/length(data.learn$weeks.worked.year[data.learn$income == 1])
# Expectedly, among people who earn more than 50K, 84% worked 52 weeks in the year

# Year (nominal)
summary(data.learn$year)
qplot(year, data=data.learn, fill=income) + ggtitle("Number of people earning more or less than 50K depending on year") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
prop.table(table(data.learn$year, data.learn$income),1)