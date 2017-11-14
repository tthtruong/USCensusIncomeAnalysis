# Datasets meant for classification (one without improvements, one with)
classif.learn <- data.learn
classif.test <- data.test
classif.improved.learn <- data.learn
classif.improved.test <- data.test

# Removing instance weigth for classification
classif.learn$instance.weight <- NULL
classif.test$instance.weight <- NULL
classif.improved.learn$instance.weight <- NULL
classif.improved.test$instance.weight <- NULL

# For the sake of classification, the variables containing NAs do not seem very relevant, we will remove them
# Maybe to improve the model, we could've thought to deal with the NAs differently
classif.learn$hisp.origin <- NULL
classif.test$hisp.origin <- NULL
classif.improved.learn$hisp.origin <- NULL
classif.improved.test$hisp.origin <- NULL
classif.learn$state.previous.residence <- NULL
classif.test$state.previous.residence <- NULL
classif.improved.learn$state.previous.residence <- NULL
classif.improved.test$state.previous.residence <- NULL
classif.learn$migration.code.change.msa <- NULL
classif.test$migration.code.change.msa <- NULL
classif.improved.learn$migration.code.change.msa <- NULL
classif.improved.test$migration.code.change.msa <- NULL
classif.learn$migration.code.change.reg <- NULL
classif.test$migration.code.change.reg <- NULL
classif.improved.learn$migration.code.change.reg <- NULL
classif.improved.test$migration.code.change.reg <- NULL
classif.learn$migration.code.move.reg <- NULL
classif.test$migration.code.move.reg <- NULL
classif.improved.learn$migration.code.move.reg <- NULL
classif.improved.test$migration.code.move.reg <- NULL
classif.learn$sunbelt <- NULL
classif.test$sunbelt <- NULL
classif.improved.learn$sunbelt <- NULL
classif.improved.test$sunbelt <- NULL
classif.learn$country.birth.father <- NULL
classif.test$country.birth.father <- NULL
classif.improved.learn$country.birth.father <- NULL
classif.improved.test$country.birth.father <- NULL
classif.learn$country.birth.mother <- NULL
classif.test$country.birth.mother <- NULL
classif.improved.learn$country.birth.mother <- NULL
classif.improved.test$country.birth.mother <- NULL
classif.learn$country.birth <- NULL
classif.test$country.birth <- NULL
classif.improved.learn$country.birth <- NULL
classif.improved.test$country.birth <- NULL


# IMPROVEMENTS
# Add "under15" variable
classif.improved.learn$under15 <- ifelse(classif.improved.learn$age < 15, 1, 0)
classif.improved.test$under15 <- ifelse(classif.improved.test$age < 15, 1, 0)

# Group variables values into general groups based on visualization and stats in univariate audit.
# Fonction categorizing continuous variable into 3 categories "Low", "Average", "High"
# Based on 1st and 3rd quartile
quartile.categorize.based <- function(dataset, variable, reference.dataset) {
  quantiles <- quantile(reference.dataset[[variable]][reference.dataset[[variable]] != 0])
  
  dataset[[variable]][reference.dataset[[variable]] < as.numeric(quantiles["25%"])] <- "Low"
  dataset[[variable]][reference.dataset[[variable]] >= as.numeric(quantiles["25%"]) 
                      & reference.dataset[[variable]] < as.numeric(quantiles["75%"])] <- "Average"
  dataset[[variable]][reference.dataset[[variable]] >= as.numeric(quantiles["75%"])] <- "High"
  dataset[[variable]] <- factor(dataset[[variable]])
  dataset[[variable]]
}

# Wage per hour
classif.improved.learn$wage.per.hour <- quartile.categorize.based(classif.improved.learn, "wage.per.hour", data.learn)
classif.improved.test$wage.per.hour <- quartile.categorize.based(classif.improved.test, "wage.per.hour", data.test)
# Capital gains
classif.improved.learn$capital.gains <- quartile.categorize.based(classif.improved.learn, "capital.gains", data.learn)
classif.improved.test$capital.gains <- quartile.categorize.based(classif.improved.test, "capital.gains", data.test)
# Capital losses
classif.improved.learn$capital.losses <- quartile.categorize.based(classif.improved.learn, "capital.losses", data.learn)
classif.improved.test$capital.losses <- quartile.categorize.based(classif.improved.test, "capital.losses", data.test)
# Dividends from stocks
classif.improved.learn$div.stocks <- quartile.categorize.based(classif.improved.learn, "div.stocks", data.learn)
classif.improved.test$div.stocks <- quartile.categorize.based(classif.improved.test, "div.stocks", data.test)

# Num persons worked for employer -> nominal
classif.improved.learn$num.pers.worked.employer <- factor(classif.improved.learn$num.pers.worked.employer)
classif.improved.test$num.pers.worked.employer <- factor(classif.improved.test$num.pers.worked.employer)

# Variables not to use in improved dataset
# Detailed industry recode
classif.improved.learn$ind.code <- NULL
classif.improved.test$ind.code <- NULL
# Detailed occupation recode
classif.improved.learn$occup.code <- NULL
classif.improved.test$occup.code <- NULL
# Detailed household and family status
classif.improved.learn$household.family.stat <- NULL
classif.improved.test$household.family.stat <- NULL