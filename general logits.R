general <- read.csv("3gen_short.csv", header = TRUE)

general.moral <- glm(moral~ Adult + Female + Question, data = general, family = "binomial")
summary(general.moral)

wald.test(b=coef(general.moral), Sigma = vcov(general.moral), Terms = 4:7)

str(general)

general2.soccon <- glm(soccon~ Adult*Question, data = general2, family = "binomial")
summary(general2.soccon)

general2.person <- glm(person~ Adult + Female + Question + Adult*Question, data = general2, family = "binomial")
summary(general2.person)

wald.test(b=coef(general2.soccon), Sigma = vcov(general2.soccon), Terms = 7)

m <- cbind(0, 1, 0, 0, 0, 0, -1)
wald.test(b=coef(general2.soccon), Sigma = vcov(general2.soccon), "L" = m)

## relevel
general2 <- read.csv("Lev3gen_short.csv", header = TRUE)

general2.moral <- glm(moral~ Adult + Female, data = general2, family = "binomial")
summary(general2.moral)

wald.test(b=coef(general.moral), Sigma = vcov(general.moral), Terms = 4:7)

general3 <- read.csv("Levgen_short.csv", header = TRUE)

general3.harmwelf <- glm(harmwelf~ Adult + Female, data = general3, family = "binomial")
summary(general3.harmwelf)

l_general3.harmwelf <- glmer(harmwelf~ (1|ID) + Question + Adult + Female, data = general3, family = "binomial", nAGQ = 8)
summary(l_general3.harmwelf)