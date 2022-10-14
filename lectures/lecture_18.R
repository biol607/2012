## @knitr lecture18prep
library(contrast)
library(plyr)
library(ggplot2)
library(multcomp)

## @knitr load_anova_data
brainGene <- read.csv("./data/15q06DisordersAndGeneExpression.csv")

brainGene$group <- factor(brainGene$group, 
                          levels=c("control", "bipolar", "schizo"))

## @knitr brainGene_plot
bg <- ddply(brainGene, .(group), summarise, mean.Expression = mean(PLP1.expression), 
            se = sd(PLP1.expression) / length(PLP1.expression), 
            n=length(PLP1.expression))

bg$group <- factor(bg$group, levels=c("control", "bipolar", "schizo"))


ggplot(bg, aes(x=as.numeric(group), y=mean.Expression, 
               ymin=mean.Expression-se,
               ymax=mean.Expression+se)) + 
                 geom_pointrange(size=1.5) +
                 theme_bw() +
                 geom_line() + scale_x_discrete(labels=c("control", "bipolar", "schizo"))

## @knitr brainGene_plot_grouped
bg$Disorder <- factor(c("n", "y", "y"))
ggplot(bg, aes(x=as.numeric(group), y=mean.Expression, 
               ymin=mean.Expression-se,
               ymax=mean.Expression+se)) + 
                 geom_pointrange(mapping=aes(color=Disorder),size=1.5) +
                 theme_bw() +
                 geom_line() + scale_x_discrete(labels=c("control", "bipolar", "schizo")) +
                  scale_color_manual(values=c("black", "red", "red"), guide="none")                 
  

## @knitr brainGene_lm
bg.sub.lm <- lm(PLP1.expression ~ group, data=brainGene)

## @knitr brainGene_lm_summary
summary(bg.sub.lm)

## @knitr brainGene_lm_summary_noint

summary(lm(PLP1.expression ~ group -1, data=brainGene))


## @knitr brainGene_contrasts
contrasts(brainGene$group)

## @knitr brainGene_anova
anova(bg.sub.lm)


## @knitr brainGene_anova
anova(bg.sub.lm)

## @knitr contrast1
library(contrast)


## @knitr contrast1
library(contrast)

contrast(bg.sub.lm, list(group="control"), 
         list(group="schizo"))


## @knitr contrast2
contrast(bg.sub.lm, list(group="control"), 
         list(group=c("schizo", "bipolar")))


## @knitr ortho1
contrast_mat <- matrix(c(1, -0.5, -0.5,
                 0,   1,     -1), nrow=2, byrow=T)

colnames(contrast_mat) <- levels(brainGene$group)
rownames(contrast_mat) <- c("Control v. Disorders", "Bipolar v. Schizo")
contrast_mat

## @knitr ortho2
library(multcomp)
#
bg_orthogonal <- glht(bg.sub.lm, linfct=contrast_mat, 
                  test=adjusted("none"))
#
summary(bg_orthogonal)

## @knitr pairwise_glht
summary( glht(bg.sub.lm, linfct=mcp(group="Tukey")),
         test=adjusted("none"))

## @knitr pairwiseT
with( brainGene, pairwise.t.test(PLP1.expression, group, 
                                 p.adjust.method ="none") )

## @knitr pairwiseT_bonferroni
with( brainGene, pairwise.t.test(PLP1.expression, group, 
                                 p.adjust.method ="bonferroni") )

## @knitr pairwiseT_fdr
with( brainGene, pairwise.t.test(PLP1.expression, group, 
                                 p.adjust.method ="fdr") )

## @knitr Tukey
bg.sub.aov <- aov(PLP1.expression ~ group, data=brainGene)
TukeyHSD(bg.sub.aov)

## @knitr daphnia_load
daphnia <- read.csv("./data/15q13DaphniaResistance.csv")
daphnia$cyandensity <- factor(daphnia$cyandensity, levels=c("low", "med", "high"))

## @knitr daphnia_plot_1
qplot(cyandensity, resistance, data=daphnia) +theme_bw()

## @knitr daphnia_plot_2
#first use plyr to get means and SE
dsummary <- ddply(daphnia, .(cyandensity), summarise, 
                  mean_resistance = mean(resistance), 
                  se = sd(resistance) / sqrt(length(resistance)))
#
ggplot(dsummary, aes(x=cyandensity, y=mean_resistance, 
                     ymin=mean_resistance-se, 
                     ymax=mean_resistance+se)) +
                       geom_pointrange() + theme_bw()

## @knitr daphnia_anova
daphniaLM <- lm(resistance ~ cyandensity, data=daphnia)
anova(daphniaLM)

## @knitr daphnia_glht
summary( glht(daphniaLM, linfct=mcp(cyandensity="Tukey")), 
         test=adjusted("none"))
