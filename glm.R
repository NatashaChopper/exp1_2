logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

library(readxl)
setwd("~/")
exp1 <- readxl::read_excel("~/exp1_2/exp1.xlsx")

#sincere vs. sarcasme

glm1 <- glm(Answer~Type, data=exp1[exp1$Type %in% c("Sincere", "Sarcasme"),], family=binomial)
coef(glm1)
logit2prob(coef(glm1))
(intercept <- coef(glm1)[1])
(sincere <- coef(glm1)[2])
(logits_sincere <- intercept + 2 * sincere)
logit2prob(logits_sincere)

#sincere vs. sincere+sarcasme

glm2 <- glm(Answer~Type, data=exp1[exp1$Type %in% c("SincSarc", "Sincere"),], family=binomial)
coef(glm2)
logit2prob(coef(glm2))
(sinc <- coef(glm2)[1])
(sincsarc <- coef(glm2)[2])
(logits_sincsarc <- sinc + 2 * sincsarc)
logit2prob(logits_sincsarc)

#sarcasme vs. sincere+sarcasme

glm3 <- glm(Answer~Type, data=exp1[exp1$Type %in% c("Sarcasme", "SincSarc"),], family=binomial)
coef(glm3)
logit2prob(coef(glm3))
(sarcasme <- coef(glm3)[1])
(scsm <- coef(glm3)[2])
(logits_scsm <- sarcasme + 2 * scsm)
logit2prob(logits_scsm)


