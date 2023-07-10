# This file fits a linear mixed effects model for the relationship between MODIS
# LST and canopy temperature.

library(MuMIn)
library(MASS)
library(RLRsim)
library(lme4)
library(car)
library(lmerTest)
library(tidyverse)

# Read in data ----

ameriflux_modis <- read_csv("data_working/ameriflux_modis_joined.csv")
ameriflux_meta  <- read_csv("data_working/ameriflux_site_metadata.csv")

forest_igbps <- c("EBF", "ENF", "DBF", "MF")
forest_sites <- ameriflux_meta$SITE_ID[ameriflux_meta$IGBP %in% forest_igbps]

scale_cols <- c("T_CANOPY", "LST_Day_1km", "Lai", "abs_view_angle")

ameriflux_forest_unscaled <- ameriflux_modis %>% 
  left_join(
    ameriflux_meta %>% select(SITE_ID, IGBP),
    by=c("ID"="SITE_ID")
  ) %>%
  filter(IGBP %in% forest_igbps) %>%
  filter(QC_Day_bitmask == 0) %>%
  mutate(
    IGBP=recode(
      IGBP,
      "DBF"="Deciduous broadleaf forest",
      "ENF"="Evergreen needleleaf forest",
      "EBF"="Evergreen broadleaf forest",
      "MF"="Mixed forest"
    )
  ) %>%
  # Convert MODIS LST to deg C
  mutate(LST_Day_1km = LST_Day_1km - 273.15,
         abs_view_angle =abs(Day_view_angl)) %>%
  # Collect predictors and scale to make lme4 happy
  select(T_CANOPY, LST_Day_1km, abs_view_angle, Lai, Day_view_angl,
         ID, IGBP, Day_view_datetime) %>%
  drop_na()



ameriflux_forest <- ameriflux_forest_unscaled %>%
  mutate(
    across(any_of(scale_cols), scale)
  )

scale_constants <- lapply(
  scale_cols, 
  function(x)  {
    c(
      attr(ameriflux_forest[[x]], "scaled:center"),
      attr(ameriflux_forest[[x]], "scaled:scale")
    )
  }
)

# Descriptive statistics
ameriflux_forest %>%
  select(T_CANOPY, LST_Day_1km, abs_view_angle, Lai) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(mean=mean(value),
            sd=sd(value),
            min=min(value),
            max=max(value))

# Model fitting ----

## Random effect structure ----
# Full model with 3rd-order interaction
full_lm <- lm(
  T_CANOPY ~ LST_Day_1km*abs_view_angle*Lai,
  data=ameriflux_forest,
  na.action="na.fail"
)

# The only candidate random effect is an intercept because LAI should
# account for random effect due to slope.
lmer_ri <- lmer(
  T_CANOPY ~ LST_Day_1km*abs_view_angle*Lai +
    (1 | ID),
  data=ameriflux_forest,
  na.action="na.fail"
)

lmer_rs <- lmer(
  T_CANOPY ~ LST_Day_1km*abs_view_angle*Lai +
    (0 + LST_Day_1km | ID),
  data=ameriflux_forest,
  na.action="na.fail"
)

# Strong evidence in favor of a random intercept
rlrt_ri <- exactRLRT(lmer_ri)
rlrt_rs <- exactRLRT(lmer_rs)

## Fixed effect structure ----
full_lmer <- lmer(
  T_CANOPY ~ LST_Day_1km*abs_view_angle*Lai +
    (1 | ID),
  data=ameriflux_forest,
  na.action="na.fail"
)

lmer_dredge <- dredge(full_lmer)
lmer_dredge %>% head(5)

# Question: dropping the main effect of forest cover improves model performance
# according to AICc, but is it valid to do so? Does the fact that forest cover
# is a site-specific parameter change our thinking?
lmer_best <- lmer(
  T_CANOPY ~ abs_view_angle + LST_Day_1km +
    forest_cover_naip:LST_Day_1km + (1 | ID),
  data=ameriflux_forest,
  na.action="na.fail"
)

lmer_cover <- lmer(
  T_CANOPY ~ LST_Day_1km + abs_view_angle + forest_cover_naip +
    forest_cover_naip:LST_Day_1km +
    (1 | ID),
  data=ameriflux_forest,
  na.action="na.fail"
)

AICc(lmer_best)
AICc(lmer_cover)

# Model diagnostics ----

## Significance from lmerTest ----
summary(lmer_best)
summary(lmer_cover)

## Normality/homoscedasticity of residuals ----

# Normality of residuals - appears non-normal in the QQ plot
qqnorm(residuals(lmer_best), main = "QQ plot (residuals)", las = 1, pch = 16)
qqline(residuals(lmer_best))

# Shapiro test comes back sig
shapiro.test(sample(residuals(lmer_best), 5000))

# ... but the histogram looks pretty good?
plot(density(residuals(lmer_best)))
abline(v=0, lty="dotted", col="red", lwd=2)

# Plot of residuals vs. fitted - weird artifact?
plot(fitted(lmer_best), resid(lmer_best), xlab="Fitted and scaled response",
     ylab="Scaled residual")
abline(h=0, col="red", lty="dashed")

artifact_rows <- which((fitted(lmer_best) < 0) & (resid(lmer_best) < -1))
artifact_data <- ameriflux_forest[artifact_rows, ]

## Random effect goodness of fit ----
# Check that the random effects are normal - they super aren't lol
site_re <- ranef(lmer_best)$ID$`(Intercept)`
qqnorm(site_re, main = "QQ plot (R Ints)", las = 1, pch = 16)
qqline(site_re)
shapiro.test(site_re)

# Rescaling the model ----
# Following along with Ben Bolker's answer
# https://stackoverflow.com/questions/24268031/unscale-and-uncenter-glmer-parameters
# We do not unscale with the original models, but fit a dummy unscaled model to
# get the scaling parameters from the model matrix.
rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1]  <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}   

lmer_unscaled <- lmer(
  T_CANOPY ~ abs_view_angle + LST_Day_1km +
    forest_cover_naip:LST_Day_1km + (1 | ID),
  data=ameriflux_forest_unscaled,
  na.action="na.fail"
)
unscaled_mmtx <- getME(lmer_unscaled,"X")                                        
unscaled_mu <- colMeans(unscaled_mmtx)[-1]
unscaled_sd <- apply(unscaled_mmtx,2,sd)[-1]
y_mu <- mean(ameriflux_forest_unscaled$T_CANOPY)
y_sd <- sd(ameriflux_forest_unscaled$T_CANOPY)

# Get parameter estimates in original units
scaled_coefs <- rescale.coefs(fixef(lmer_best), mu=c(y_mu, unscaled_mu),
                              sigma=c(y_sd, unscaled_sd))
ci_coefs <- confint(lmer_best)
scaled_ci_lower <- rescale.coefs(ci_coefs[3:6, 1], mu=c(y_mu, unscaled_mu),
                          sigma=c(y_sd, unscaled_sd))
scaled_ci_upper <- rescale.coefs(ci_coefs[3:6, 2], mu=c(y_mu, unscaled_mu),
                                 sigma=c(y_sd, unscaled_sd))

scaled_sig_ci <- ci_coefs[1:2, ] * y_sd
scaled_resid_sig <- lmer_best@sigma * y_sd
scaled_ranef_sig <- sd(ranef(lmer_best)$ID[, 1]) * y_sd 
                   
scaled_resid <- (residuals(lmer_best) * y_sd)

scaled_ranefs <- ranef(lmer_best)$ID$`(Intercept)` * y_sd

scaled_response <- predict(lmer_best) * y_sd + y_mu

# Diagnostic plots ----
par(mfrow=c(1, 2))
qqnorm(residuals(lmer_best), main="", las = 1, pch = 16)
qqline(residuals(lmer_best))

plot(density(scaled_resid), xlab="Residual (deg C)", main="")
abline(v=0, col="red", lty="dotted", lwd=2)

par(mfrow=c(1, 1))
plot(scaled_response, residuals(lmer_best, type="pearson"),
     xlab="Response (deg C)", ylab="Scaled Residual")
abline(h=0, lty="dotted", col="red", lwd=2)
# Extra plots ----
theme_set(theme_bw())
ameriflux_forest %>%
  ggplot(aes(x=T_CANOPY, y=LST_Day_1km)) +
  #geom_point(aes(color=ID), alpha = 0.2) +
  stat_ellipse(geom="polygon", aes(fill=ID),
               alpha=0.2, show.legend = FALSE,
               level=0.95) + 
  geom_abline(slope=1, intercept=0, color="black", linetype="dashed") +
  coord_equal() +
  facet_wrap(~ IGBP) +
  guides(color=guide_legend(override.aes=list(alpha=1)))
