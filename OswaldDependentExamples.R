library(metaplotr)
library(metafor)
library(robumeta)

head(FergusonBrannick2012)
# head(GenderDiff01)
# head(GenderDiff02)
# head(GenderDiff03)
# head(McLeod2007)

crosshairs(FergusonBrannick2012$pub_z, FergusonBrannick2012$dis_z, FergusonBrannick2012$pub_z_se,FergusonBrannick2012$dis_z_se, confint=.95,
           main_lab="Ferguson & Brannick (2012)", x_lab = "Publication effect", y_lab = "Dissertation effect", annotate=TRUE) 
crosshairs(Sweeney2015$inten_d, Sweeney2015$beh_d, Sweeney2015$inten_se,Sweeney2015$beh_se, confint=.95,
           main_lab="Sweeney (2015)", x_lab = "Intentions effect", y_lab = "Behaviors effect", annotate=TRUE) 

# reshape the data
k <- nrow(FergusonBrannick2012)
fb <- cbind(1:k,FergusonBrannick2012[,c(1,3,5)],rep("pub",k))
colnames(fb) <- c("id","yi","vi","mod","source")
fb2 <- cbind(1:k,FergusonBrannick2012[,c(2,4,5)],rep("diss",k))
colnames(fb2) <- c("id","yi","vi","mod","source")
fb <- rbind(fb,fb2)
dat <- fb[order(fb[,1]),]
# change the SEs to error variances (per the vi label)
# by squaring the values
dat[,3] <- dat[,3]^2

# make mod numeric where 0 = no and 1 = yes...won't use below
# make source numeric where 0 = pub and 1 = diss
dat[,4] <- as.numeric(dat[,4]) - 1
dat[,5] <- as.numeric(dat[,5]) - 1

# summarize the effects
round(c(summary(dat$yi), SD=sd(dat$yi)), 2)

# run RE without any moderators
res1 <- rma(yi, vi, data=dat, method="ML")
summary(res1)

# run RE with source as a moderator, again where 0 = pub and 1 = diss
res2 <- rma(yi, vi, mods = source, data=dat, method="ML")
summary(res2)

# compare the two models
anova(res1,res2)

# run RE taking study clustering into account
res3 <- rma.mv(yi, vi, random = ~ 1 | id, data=dat, method="ML")
summary(res3)

# run RE with study as a moderator and taking study clustering into account
res4 <- rma.mv(yi, vi, mods = source, random = ~ 1 | id, data=dat, method="ML")
summary(res4)

anova(res3,res4)

# compare models 1 and 3 using REML
res1 <- rma.mv(yi, vi, data=dat, method="REML")
res3 <- rma.mv(yi, vi, random = ~ 1 | id, data=dat, method="REML")
anova(res1,res3)

# how accurate is sigma-squared? not very according to the restricted LL profile
profile(res4, xlim=c(0.01,2), steps=100, log="x", cex=0, lwd=2)
# the CI on sigma-squared is also indicative
confint(res4)

# now let's look at robust estimation
robu(formula = yi ~ source, studynum = id, var.eff.size = vi, data=dat, rho = 0)

head(Sweeney2015)
# reshape the data
k <- nrow(Sweeney2015)
sw <- cbind(1:k,Sweeney2015[,c(1,2,3,4)],rep("inten",k))
colnames(sw) <- c("id","author","year", "yi","vi","source")
sw2 <- cbind(1:k,Sweeney2015[,c(1,2,5,6)],rep("beh",k))
colnames(sw2) <- c("id","author","year", "yi","vi","source")
sw <- rbind(sw,sw2)
dat2 <- sw[order(sw[,1]),]
# change the SEs to error variances (per the vi label)
# by squaring the values
dat2[,5] <- dat2[,5]^2

# make source numeric where 0 = intentions and 1 = behavior
dat2[,6] <- as.numeric(dat2[,6]) - 1

# summarize the effects
round(c(summary(dat2$yi), SD=sd(dat2$yi)), 2)

# run RE without any moderators
res1 <- rma(yi, vi, data=dat2, method="ML")
summary(res1)

# run RE with source as a moderator, again where 0 = intentions and 1 = behavior
res2 <- rma(yi, vi, mods = source, data=dat2, method="ML")
summary(res2)

# compare the two models
anova(res1,res2)

# run RE taking study clustering into account
res3 <- rma.mv(yi, vi, random = ~ 1 | id, data=dat2, method="ML")
summary(res3)

# compare models 1 and 3 using REML
res1 <- rma.mv(yi, vi, data=dat2, method="REML")
res3 <- rma.mv(yi, vi, random = ~ 1 | id, data=dat2, method="REML")
anova(res1,res3)

# run RE with study as a moderator and taking study clustering into account
res4 <- rma.mv(yi, vi, mods = source, random = ~ 1 | id, data=dat2, method="ML")
summary(res4)

anova(res3,res4)

# how accurate is sigma-squared? not very according to the restricted LL profile
profile(res4, xlim=c(0.01,2), steps=100, log="x", cex=0, lwd=2)
# the CI on sigma-squared is also indicative
confint(res4)

# now let's look at robust estimation
robu(formula = yi ~ source, studynum = id, var.eff.size = vi, data=dat2, rho = .5)

