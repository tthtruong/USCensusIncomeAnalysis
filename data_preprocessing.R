# Assign names to columns
col.names <- c("age", "class.worker", "ind.code", "occup.code", "education", "wage.per.hour", "enroll.edu.inst.last.wk", "marital.stat", "major.ind.code", "major.occup.code", "race", 
               "hisp.origin", "sex", "memb.labor.union", "reason.unemployment", "ft.pt.employ.stat", "capital.gains", "capital.losses", "div.stocks", "tax.filer.stat", "region.previous.residence", 
               "state.previous.residence", "household.family.stat", "household.summary", "instance.weight", "migration.code.change.msa", "migration.code.change.reg", "migration.code.move.reg", 
               "live.house.1year.ago", "sunbelt", "num.pers.worked.employer", "fam.if.under18", "country.birth.father", "country.birth.mother", "country.birth", "citizenship", 
               "own.business.self.employed", "questionnaire.vet.admin", "veterans.benefits", "weeks.worked.year", "year", "income")
colnames(data.learn) <- col.names
colnames(data.test) <- col.names

# Get infos on dataset
summary(data.learn)
str(data.learn)

# According to the metadata file, the following variables are to be considered as factors
data.learn$ind.code <- factor(data.learn$ind.code)
data.learn$occup.code <- factor(data.learn$occup.code)
data.learn$own.business.self.employed <- factor(data.learn$own.business.self.employed)
data.learn$veterans.benefits <- factor(data.learn$veterans.benefits)
data.learn$year <- factor(data.learn$year)
data.test$ind.code <- factor(data.test$ind.code)
data.test$occup.code <- factor(data.test$occup.code)
data.test$own.business.self.employed <- factor(data.test$own.business.self.employed)
data.test$veterans.benefits <- factor(data.test$veterans.benefits)
data.test$year <- factor(data.test$year)

# Change income to a numeric binary var
levels(data.learn$income) <- c(0, 1)
levels(data.test$income) <- c(0, 1)

# Percentage of people winning more than 50K
length(data.learn$income[data.learn$income == 1])
length(data.learn$income[data.learn$income == 1]) / length(data.learn$income) * 100
length(data.learn$income[data.learn$income == 0])

# Percentage of missing values in each columns
missing.values <- data.frame(matrix(vector(), nrow=ncol(data.learn), ncol=4, dimnames=list(c(), c("fake.NA", "real.NA", "quest.mark", "not.know"))))
# Value "Not in universe" means variable doesn't apply to the individual, so not really a missing value
missing.values$fake.NA <- sapply(1:ncol(data.learn), function(x) sum(data.learn[x] ==' NA', na.rm=T))/nrow(data.learn)
missing.values$quest.mark <- sapply(1:ncol(data.learn), function(x) sum(data.learn[x] ==' ?', na.rm=T))/nrow(data.learn)
missing.values$not.know <- sapply(1:ncol(data.learn), function(x) sum(data.learn[x] ==' Do not know', na.rm=T))/nrow(data.learn)
missing.values$real.NA <- sapply(1:ncol(data.learn), function(x) sum(is.na(data.learn[x])))/nrow(data.learn)
# No real NA
missing.values
rownames(missing.values) <- colnames(data.learn)
# Variables in which values are missing
as.data.frame(rowSums(missing.values)[rowSums(missing.values) > 0])
# TODO: Values need to be replaced with real NAs

# Replacing NA values by real NAs
data.learn$hisp.origin[data.learn$hisp.origin == ' Do not know'] <- NA
data.learn$hisp.origin[data.learn$hisp.origin == ' NA'] <- NA
data.learn$state.previous.residence[data.learn$state.previous.residence == ' ?'] <- NA
data.learn$migration.code.change.msa[data.learn$migration.code.change.msa == ' ?'] <- NA
data.learn$migration.code.change.reg[data.learn$migration.code.change.reg == ' ?'] <- NA
data.learn$migration.code.move.reg[data.learn$migration.code.move.reg == ' ?'] <- NA
data.learn$sunbelt[data.learn$sunbelt == ' ?'] <- NA
data.learn$country.birth.father[data.learn$country.birth.father == ' ?'] <- NA
data.learn$country.birth.mother[data.learn$country.birth.mother == ' ?'] <- NA
data.learn$country.birth[data.learn$country.birth == ' ?'] <- NA

data.test$hisp.origin[data.test$hisp.origin == ' Do not know'] <- NA
data.test$hisp.origin[data.test$hisp.origin == ' NA'] <- NA
data.test$state.previous.residence[data.test$state.previous.residence == ' ?'] <- NA
data.test$migration.code.change.msa[data.test$migration.code.change.msa == ' ?'] <- NA
data.test$migration.code.change.reg[data.test$migration.code.change.reg == ' ?'] <- NA
data.test$migration.code.move.reg[data.test$migration.code.move.reg == ' ?'] <- NA
data.test$sunbelt[data.test$sunbelt == ' ?'] <- NA
data.test$country.birth.father[data.test$country.birth.father == ' ?'] <- NA
data.test$country.birth.mother[data.test$country.birth.mother == ' ?'] <- NA
data.test$country.birth[data.test$country.birth == ' ?'] <- NA

# If test dataset doesn't have same levels than training dataset
for (i in 1:ncol(data.learn)) {
  levels(data.test[[i]]) <- levels(data.learn[[i]])
}
