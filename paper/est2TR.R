## ----"main-setup", include = FALSE--------------------------------------------
## knitr options
library(knitr)
opts_chunk$set(fig.height = 4,
               echo = FALSE,
               warning = FALSE,
               message = FALSE,
               cache = FALSE,
               eval = TRUE)

## printed digits
options(scipen = 100000)

## should sessionInfo be printed at the end?
Reproducibility <- TRUE

## ----"package"----------------------------------------------------------------
## packages
library(confMeta)
library(ggplot2)
library(twotrials)
library(kableExtra)
library(dplyr)

## ----"data"-------------------------------------------------------------------
## data from RESPIRE 1 (Table 3 in <https://doi.org/10.1183/13993003.02052-2017>)
cirr14r1 <- c(0.4, 0.91) # attention 97.5% CI!
cilogrr14r1 <- log(cirr14r1) # attention 97.5% CI!
logrr14r1 <- log(0.61)
selogrr14r1 <- (cilogrr14r1[2] - cilogrr14r1[1])/(2*qnorm(p = (1 + 0.975)/2))
cilogrr14r1 <- logrr14r1 + c(-1, 1)*selogrr14r1*qnorm(p = 0.975) # 95% CI
cirr14r1 <- exp(cilogrr14r1) # 95% CI
cirr28r1 <- c(0.64, 1.48) # attention 97.5% CI!
cilogrr28r1 <- log(cirr28r1) # attention 97.5% CI!
logrr28r1 <- log(0.98)
selogrr28r1 <- (cilogrr28r1[2] - cilogrr28r1[1])/(2*qnorm(p = (1 + 0.975)/2))
cilogrr28r1 <- logrr28r1 + c(-1, 1)*selogrr28r1*qnorm(p = 0.975) # 95% CI
cirr28r1 <- exp(cilogrr28r1) # 95% CI

## data from RESPIRE 2 (Table 3 in <https://doi.org/10.1183/13993003.02053-2017>)
cirr14r2 <- c(0.59, 1.17) # attention 95.1% CI!
cilogrr14r2 <- log(cirr14r2) # attention 95.1% CI!
logrr14r2 <- log(0.8313)
selogrr14r2 <- (cilogrr14r2[2] - cilogrr14r2[1])/(2*qnorm(p = (1 + 0.951)/2))
cilogrr14r2 <- logrr14r2 + c(-1, 1)*selogrr14r2*qnorm(p = 0.975) # 95% CI
cirr14r2 <- exp(cilogrr14r2) # 95% CI
cirr28r2 <- c(0.30, 1.02) # attention 99.9% CI!
cilogrr28r2 <- log(cirr28r2) # attention 99.9% CI!
logrr28r2 <- log(0.5493)
selogrr28r2 <- (cilogrr28r2[2] - cilogrr28r2[1])/(2*qnorm(p = (1 + 0.999)/2))
cilogrr28r2 <- logrr28r2 + c(-1, 1)*selogrr28r2*qnorm(p = 0.975) # 95% CI
cirr28r2 <- exp(cilogrr28r2) # 95% CI

## ## results for 14 days treatment group
## twotrials(null = 0, t1 = logrr14r1, t2 = logrr14r2, se1 = selogrr14r1,
##           se2 = selogrr14r2, alternative = "less", level = 0.95)
## ## results for 28 days treatment group
## twotrials(null = 0, t1 = logrr28r1, t2 = logrr28r2, se1 = selogrr28r1,
##           se2 = selogrr28r2, alternative = "less", level = 0.95)

## data from ORBIT 3 and ORBIT 4
## (p. 219 in <https://doi.org/10.1016/S2213-2600(18)30427-2>)
cihr3 <- c(0.71, 1.38) # 95% CI
ciloghr3 <- log(cihr3) # 95% CI
loghr3 <- log(0.99)
seloghr3 <- (ciloghr3[2] - ciloghr3[1])/(2*qnorm(p = 0.975))
cihr4 <- c(0.53, 0.97) # 95% CI
ciloghr4 <- log(cihr4) # 95% CI
loghr4 <- log(0.72)
seloghr4 <- (ciloghr4[2] - ciloghr4[1])/(2*qnorm(p = 0.975))
cilogrr3 <- c(0.65, 1.12) # 95% CI
cilogrr3 <- log(cilogrr3) # 95% CI
logrr3 <- log(0.85)
selogrr3 <- (cilogrr3[2] - cilogrr3[1])/(2*qnorm(p = 0.975))
cilogrr4 <- c(0.48, 0.82) # 95% CI
cilogrr4 <- log(cilogrr4) # 95% CI
logrr4 <- log(0.63)
selogrr4 <- (cilogrr4[2] - cilogrr4[1])/(2*qnorm(p = 0.975))

## ## results for primary endpoint (HR)
## twotrials(null = 0, t1 = loghr3, t2 = loghr4, se1 = seloghr3,
##           se2 = seloghr4, alternative = "less", level = 0.95)
## ## results for secondary endpoint (RR)
## twotrials(null = 0, t1 = logrr3, t2 = logrr4, se1 = selogrr3,
##           se2 = selogrr4, alternative = "less", level = 0.95)


## ----"respire-trials"---------------------------------------------------------
## RESPIRE trials
t1 <- logrr14r1
t2 <- logrr14r2
se1 <- selogrr14r1
se2 <- selogrr14r2
z1 <- t1/se1
z2 <- t2/se2
p1 <- pnorm(q = z1)
p2 <- pnorm(q = z2)
ci1 <- t1 + c(-1, 1)*se1*qnorm(p = 0.975)
ci2 <- t2 + c(-1, 1)*se2*qnorm(p = 0.975)
semeta <- 1/sqrt(1/se1^2 + 1/se2^2)
tmeta <- (t1/se1^2 + t2/se2^2)*semeta^2
cimeta <- tmeta + c(-1, 1)*qnorm(p = 0.975)*semeta
zmeta <- (tmeta - 0)/semeta
pmeta <- pnorm(q = zmeta)

## (1 - 2*alpha^2) CI
cimeta2 <- tmeta + c(-1, 1)*qnorm(p = 1 - 2*0.025^2)*semeta
cimeta3 <- tmeta + c(-1, 1)*qnorm(p = 1 - 2*0.05^2)*semeta


## ----"one-and-two-sided-pfs", fig.height = 2.5--------------------------------
pfunnorm. <- function(mu, t, se, alternative = "greater") {
    z <- (t - mu)/se
    if (alternative == "greater") {
        p <- 1 - pnorm(q = z)
    } else if (alternative == "less") {
        p <- pnorm(q = z)
    } else { # two-sided
        p <- 2*(1 - pnorm(q = abs(z)))
    }
    return(p)
}
pfunnorm <- Vectorize(FUN = pfunnorm.)

## illustration of p-value function, CI, median estimate, centrality function
museq <- seq(-3, 3, length.out = 500)
t <- 0
se <- 0.8
col <- adjustcolor(col = "#009E73", alpha.f = 0.95)
par(mfrow = c(1, 2), las = 1, "mar" = c(2.3, 4.1, 0.5, 1.5),
    "oma" = c(0, 0, 0, 0), xpd = TRUE)
pbks <- seq(0.2, 0.8, 0.2)
pfun1 <- pfunnorm(mu = museq, t = t, se = se)
alpha <- 0.05
ci <- t + c(-1, 1)*se*qnorm(p = 1 - alpha/2)
plot(museq, pfun1, type = "l", lwd = 1.5, xlab = "", ylim = c(0, 1),
     ylab = bquote(italic(p) * "-value (one-sided)"), xaxt = "n", yaxt = "n")
mtext(side = 1, text = bquote(mu), line = 1.5)
axis(side = 2, at = pbks)
segments(x0 = c(ci[1], t, ci[2]), y0 = c(alpha/2, 0.5, 1 - alpha/2), y1 = -0.02,
         lty = 2, col = col)
segments(x0 = c(ci[1], t, ci[2]), y0 = c(alpha/2, 0.5, 1 - alpha/2), x1 = -3.1,
         lty = 2, col = col)
## mtext(side = 2, text = c(alpha/2, 0.5, 1 - alpha/2), line = 0.25,
##       at = c(alpha/2, 0.5, 1 - alpha/2), col = col, cex = 0.9)
axis(side = 2, at = c(alpha/2, 0.5, 1 - alpha/2), col.ticks = col, col.axis = col,
     cex.axis = 0.8)
## axis(side = 1, at = c(ci[1], t, ci[2]), labels = rep("", 3), col.ticks = col,
##      col.axis = col, cex.axis = 0.8)
arrows(x0 = ci[1], x1 = ci[2], y0 = -0.1, code = 3, length = 0.075, col = col, angle = 90)
points(x = t, y = -0.1, col = col, pch = 20)

plot(museq, 2*pmin(pfun1, 1 - pfun1), type = "l", lwd = 1.5, xlab = "",
     ylim = c(0, 1),
     ylab = bquote(italic(p) * "-value (two-sided)"),
     xaxt = "n", yaxt = "n")
mtext(side = 1, text = bquote(mu), line = 1.5)
axis(side = 2, at = pbks)
segments(x0 = c(ci[1], t, ci[2]), y0 = c(alpha, 1, alpha), y1 = -0.02,
         lty = 2, col = col)
segments(x0 = c(ci[2], t), y0 = c(alpha, 1), x1 = -3.1, lty = 2, col = col)
## mtext(side = 2, text = c(alpha/2, 0.5, 1 - alpha/2), line = 0.25,
##       at = c(alpha/2, 0.5, 1 - alpha/2), col = col, cex = 0.9)
axis(side = 2, at = c(alpha, 1), col.ticks = col, col.axis = col,
     cex.axis = 0.8)
## axis(side = 1, at = c(ci[1], t, ci[2]), labels = rep("", 3), col.ticks = col,
##      col.axis = col, cex.axis = 0.8)
arrows(x0 = ci[1], x1 = ci[2], y0 = -0.1, code = 3, length = 0.075, col = col, angle = 90)
points(x = t, y = -0.1, col = col, pch = 20)


## ----"hypothetical-examples", fig.height = 5----------------------------------
c <- 2 # relative sample size of study 2 to study 1
nsmall <- 5 # small per-group sample size
nlarge <- 500 # big per-group sample size
settingsgrid <- data.frame(t1 = c(2.5, 3.5, 2.5, 3.5),
                           t2 = c(2.5, 1.5, 2.5, 1.5),
                           se1 = sqrt(2/c(nsmall, nsmall, nlarge, nlarge)),
                           se2 = sqrt(2/c(nsmall, nsmall, nlarge, nlarge)/c),
                           label = c("A) Identical and imprecise estimates",
                                     "B) Different and imprecise estimates",
                                     "C) Identical and precise estimates",
                                     "D) Different and precise estimates"))
museq <- seq(0, 5, length.out = 10000)
xlim <- c(0, 5)
input_p <- "greater"
alternative <- "greater"
alpha <- 0.05
null <- 0
resList <- lapply(X = seq(1, nrow(settingsgrid)), FUN = function(i) {
    res <- twotrials(null = null,
                     t1 = settingsgrid$t1[i], t2 = settingsgrid$t2[i],
                     se1 = settingsgrid$se1[i], se2 = settingsgrid$se2[i],
                     alternative = "greater", level = 1 - alpha)
    plotres <- plot(res, xlim = xlim, two.sided = TRUE, plot = FALSE)
    colnames(plotres)[1] <- "type"
    isummaries <- res$isummaries
    summaries <- res$summaries
    colnames(isummaries)[1] <- "type"
    colnames(summaries)[1] <- "type"
    list("p" = data.frame(plotres, settingsgrid[i,]),
         "summaries" = data.frame(rbind(isummaries, summaries[,1:5]), settingsgrid[i,]))
})
pDF <- do.call("rbind", lapply(X = resList, function(x) x$p))
summariesDF <- do.call("rbind", lapply(X = resList, function(x) x$summaries))
typelevels <- c("Trial 1", "Trial 2", "Two-trials rule", "Meta-analysis",
                "Tippett",  "Fisher", "Pearson", "Edgington")
summariesDF$type <- factor(summariesDF$type, levels = rev(typelevels))
pDF$type <- factor(pDF$type, levels = rev(typelevels))

lty <- c(rep("22", 2), rep("solid", 6))
names(lty) <- unique(summariesDF$type)
cols <- c("Trial 1" = "#CC79A7F2", "Trial 2" = "#888888F2",
          "Meta-analysis" = "#56B4E9F2", "Two-trials rule" = "#000000F2",
          "Tippett" = "#E69F00F2", "Edgington" = "#009E73F2",
          "Fisher" = "#F0E442F2", "Pearson" = "#0072B2F2")
ggplot(data = pDF, aes(x = mu, y = p, color = type, linetype = type)) +
    facet_wrap(~ label) +
    geom_hline(yintercept = alpha, lty = "longdash", alpha = 0.1) +
    geom_line() +
    geom_pointrange(data = summariesDF,
                    aes(xmin = lower, xmax = upper, x = est, y = 1.15),
                    position = position_dodge2(width = 0.2),
                    fatten = 0.5) +
    ## geom_vline(data = summariesDF, aes(xintercept = upper, color = type),
    ##            alpha = 0.2) +
    ## geom_vline(data = summariesDF, aes(xintercept = lower, color = type),
    ##            alpha = 0.2) +
    scale_linetype_manual(values = lty,
                          guide = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = cols,
                       guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks = c(seq(0.25, 1, 0.25), 0.05),
                        sec.axis = sec_axis(~ 1 - .,
                                           breaks = c(seq(0, 0.75, 0.25), 0.95),
                                           labels = scales::percent,
                                           name = "Confidence")) +
    labs(x = "Effect size", y = bquote(italic(p) * "-value"),
         color = NULL, linetype = NULL) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#00000005"))


## ----"RESPIRE-analysis", fig.height = 5---------------------------------------
## wrapper function to compute combined CIs, median estimates, and p-values
resFun <- function(t1, t2, se1, se2, labels, xlim, alternative, alpha, null,
                   lab1, lab2) {
    resList <- lapply(X = seq(1, length(t1)), FUN = function(i) {
        ## ordinary alpha
        res <- twotrials(null = null, t1 = t1[i], t2 = t2[i], se1 = se1[i],
                         se2 = se2[i], alternative = alternative,
                         level = 1 - alpha)
        ## for a lower alpha level to have CI decision at alpha^2
        res2 <- twotrials(null = null, t1 = t1[i], t2 = t2[i], se1 = se1[i],
                          se2 = se2[i], alternative = alternative,
                          level = 1 - 2*(alpha/2)^2)
        plotres <- plot(res, xlim = xlim, two.sided = TRUE, plot = FALSE)
        colnames(plotres)[1] <- "type"
        colnames(res$isummaries)[1] <- "type"
        colnames(res$summaries)[1] <- "type"
        res$isummaries$w2 <- res$isummaries$w1 <- NA
        summaries <- rbind(res$isummaries, res$summaries)
        summaries$lower2 <- c(NA, NA, res2$summaries$lower)
        summaries$upper2 <- c(NA, NA, res2$summaries$upper)
        summaries$labels <- labels[i]
        plotres$labels <- labels[i]
        res <- list("p" = plotres, "summaries" = summaries)
    })
    pDF <- do.call("rbind", lapply(X = resList, function(x) x$p))
    summariesDF <- do.call("rbind", lapply(X = resList, function(x) x$summaries))
    typelevels <- c("Trial 1", "Trial 2", "Two-trials rule", "Meta-analysis",
                    "Tippett",  "Fisher", "Pearson", "Edgington")
    typelabs <- c(lab1, lab2, "Two-trials rule", "Meta-analysis", "Tippett",
                  "Fisher", "Pearson", "Edgington")
    summariesDF$type <- factor(summariesDF$type, levels = rev(typelevels),
                               labels = rev(typelabs))
    pDF$type <- factor(pDF$type, levels = rev(typelevels), labels = rev(typelabs))
    list("summariesDF" = summariesDF, "pDF" = pDF)
}

## compute results for RESPIRE trials
RESPIREres <- resFun(t1 = c(logrr14r1, logrr28r1),
                     se1 = c(selogrr14r1, selogrr28r1),
                     t2 = c(logrr14r2, logrr28r2),
                     se2 = c(selogrr14r2, selogrr28r2),
                     labels = c("14-day treatment group",
                                "28-day treatment group"),
                     xlim = c(-1, 0.5),
                     alternative = "less", alpha = 0.05, null = 0,
                     lab1 = "RESPIRE 1", lab2 = "RESPIRE 2")
names(cols)[1:2] <- names(lty)[1:2] <- c("RESPIRE 1", "RESPIRE 2")

## ## save results as CSV
## RESPIREres$summariesDF |>
##         select(type, labels, lower, est, upper, p0, w1, w2) |>
##     rename("Type" = type, "Regimen" = labels, "Lower 95% CL" = lower,
##            "Point estimate" = est, "Upper 95% CL" = upper, "P-value" = p0,
##            "Weight RESPIRE 1" = w1, "Weight RESPIRE 2" = w2) |>
##     write.csv(file = "data/RESPIRE.csv", row.names = FALSE)

## plot results
ggplot(data = RESPIREres$pDF, aes(x = mu, y = p, color = type, linetype = type)) +
    facet_wrap(~ labels, ncol = 1) +
    geom_hline(yintercept = alpha, lty = "longdash", alpha = 0.1) +
    geom_line() +
    geom_pointrange(data = RESPIREres$summariesDF,
                    aes(xmin = lower2, xmax = upper2, x = est, y = 1.15),
                    position = position_dodge2(width = 0.25), fatten = 0,
                    linewidth = 0.3, alpha = 0.3) +
    geom_pointrange(data = RESPIREres$summariesDF,
                    aes(xmin = lower, xmax = upper, x = est, y = 1.15),
                    position = position_dodge2(width = 0.25),
                    fatten = 1.1) +
    ## geom_vline(data = RESPIREres$summariesDF,
    ##            aes(xintercept = upper, color = type), alpha = 0.2) +
    ## geom_vline(data = RESPIREres$summariesDF,
    ##            aes(xintercept = lower, color = type), alpha = 0.2) +
    scale_linetype_manual(values = lty, guide = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
    coord_cartesian(xlim = c(-1, 0.5), ylim = c(-0.02, 1.25)) +
    scale_x_continuous(breaks = round(seq(-0.9, 0.3, 0.3), 1)) +
    scale_y_continuous(breaks = c(seq(0.25, 1, 0.25), 0.05),
                       sec.axis = sec_axis(~ 1 - .,
                                           breaks = c(seq(0, 0.75, 0.25), 0.95),
                                           labels = scales::percent,
                                           name = "Confidence")) +
    annotate(geom = "segment", arrow = arrow(length = unit(1.2, 'mm'),
                                             type = "closed"),
             alpha = 0.6,
             x = c(-1, 1)*0.02, xend = c(-1, 1)*0.2,
             y = -0.06, yend = -0.06) +
    annotate(geom = "text", alpha = 0.8, label = "Benefit",
             x = -0.1, y = -0.025, size = 2.75) +
     annotate(geom = "text", alpha = 0.8, label = "Harm",
             x = 0.1, y = -0.025, size = 2.75) +
    labs(x = "Log rate ratio", y = bquote(italic(p) * "-value"),
         color = NULL, linetype = NULL) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#00000005"))


## ----"table-RESPIRE", results = "asis"----------------------------------------
formatNumberLaTeX. <- function(number, digits = 2) {
    if (is.na(number)) {
        return("$ $")
    }
    sprintfString <- paste0("%.", digits, "f")
    numberRound <- sprintf(sprintfString, number)
    if (number >= 0) {
        numberRound <- paste0("\\phantom{-}", numberRound)
    }
    paste0("$", numberRound, "$")
}
formatNumberLaTeX <- Vectorize(FUN = formatNumberLaTeX.)
formatCILaTeX <- function(lower, upper, digits = 2) {
    paste0(formatNumberLaTeX(number = lower, digits = digits),
           " to ",
           formatNumberLaTeX(number = upper, digits = digits))
}

dig <- 2
digPvals <- 5
RESPIREtab <- RESPIREres$summariesDF |>
    mutate(est = formatNumberLaTeX(number = est, digits = dig),
           w1 = formatNumberLaTeX(number = w1, digits = dig),
           ci = formatCILaTeX(lower = lower, upper = upper, digits = dig),
           ciwidth = formatNumberLaTeX(number = upper - lower, digits = dig),
           p0 = formatNumberLaTeX(number = p0, digits = digPvals)
           ) |>
    select(labels, type, est, w1, ci, ciwidth, p0)
kable(RESPIREtab |> select(-labels),
      format = "latex", booktabs = TRUE, align = "lcccccc",
      col.names = c("",
                    "\\textbf{Log rate ratio}",
                    "\\textbf{Weight RESPIRE 1}",
                    "\\textbf{95\\% CI}",
                    "\\textbf{CI width}",
                    "\\textbf{\\textit{P}-value (one-sided)}"),
      bold = TRUE, escape = FALSE) |>
    pack_rows("14-day treatment group", 1, 8, escape = FALSE, bold = FALSE,
              italic = TRUE) |>
    pack_rows("28-day treatment group", 9, 16, escape = FALSE, bold = FALSE,
              italic = TRUE) |>
    row_spec(c(2, 10), hline_after = TRUE) |>
    row_spec(seq(1, nrow(RESPIREtab), by = 2), background = "#F0F0F0")


## ----"ORBIT-analysis", fig.height = 5-----------------------------------------
## compute results for ORBIT trials
ORBITres <- resFun(t1 = c(loghr3, logrr3),
                   se1 = c(seloghr3, selogrr3),
                   t2 = c(loghr4, logrr4),
                   se2 = c(seloghr4, selogrr4),
                            # time to the first exacerbation
                   labels = c("Primary endpoint (log hazard ratio)",
                            # frequency of exacerbations
                              "Secondary endpoint (log rate ratio)"),
                   xlim = c(-0.8, 0.4),
                   alternative = "less", alpha = 0.05, null = 0,
                   lab1 = "ORBIT 3", lab2 = "ORBIT 4")
names(cols)[1:2] <- names(lty)[1:2] <- c("ORBIT 3", "ORBIT 4")

## ## save ORBIT results as CSV
## ORBITres$summariesDF |>
##         select(type, labels, lower, est, upper, p0, w1, w2) |>
##     rename(Type = type, Endpoint = labels, "Lower 95% CL" = lower,
##            "Point estimate" = est, "Upper 95% CL" = upper, "P-value" = p0,
##            "Weight ORBIT 3" = w1, "Weight ORBIT 4" = w2) |>
##     write.csv(file = "data/ORBIT.csv", row.names = FALSE)

## plot results
ggplot(data = ORBITres$pDF, aes(x = mu, y = p, color = type, linetype = type)) +
    facet_wrap(~ labels, ncol = 1) +
    geom_hline(yintercept = alpha, lty = "longdash", alpha = 0.1) +
    geom_line() +
    geom_pointrange(data = ORBITres$summariesDF,
                    aes(xmin = lower2, xmax = upper2, x = est, y = 1.15),
                    position = position_dodge2(width = 0.25), fatten = 0,
                    linewidth = 0.3, alpha = 0.3) +
    geom_pointrange(data = ORBITres$summariesDF,
                    aes(xmin = lower, xmax = upper, x = est, y = 1.15),
                    position = position_dodge2(width = 0.25),
                    fatten = 1.1) +
    ## geom_vline(data = ORBITres$summariesDF, aes(xintercept = upper, color = type),
    ##            alpha = 0.2) +
    ## geom_vline(data = ORBITres$summariesDF, aes(xintercept = lower, color = type),
    ##            alpha = 0.2) +
    scale_linetype_manual(values = lty, guide = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
    coord_cartesian(xlim = c(-0.8, 0.4), ylim = c(-0.02, 1.25)) +
    scale_x_continuous(breaks = seq(-0.75, 0.5, 0.25)) +
    scale_y_continuous(breaks = c(seq(0.25, 1, 0.25), 0.05),
                       sec.axis = sec_axis(~ 1 - .,
                                           breaks = c(seq(0, 0.75, 0.25), 0.95),
                                           labels = scales::percent,
                                           name = "Confidence")) +
    annotate(geom = "segment", arrow = arrow(length = unit(1.2, 'mm'),
                                             type = "closed"),
             alpha = 0.6,
             x = c(-1, 1)*0.02, xend = c(-1, 1)*0.2,
             y = -0.06, yend = -0.06) +
    annotate(geom = "text", alpha = 0.8, label = "Benefit",
             x = -0.1, y = -0.025, size = 2.75) +
     annotate(geom = "text", alpha = 0.8, label = "Harm",
             x = 0.1, y = -0.025, size = 2.75) +
    labs(x = "Effect size", y = bquote(italic(p) * "-value"),
         color = NULL, linetype = NULL) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#00000005"))


## ----"table-ORBIT", results = "asis"------------------------------------------
ORBITtab <- ORBITres$summariesDF |>
    mutate(est = formatNumberLaTeX(number = est, digits = dig),
           w1 = formatNumberLaTeX(number = w1, digits = dig),
           ci = formatCILaTeX(lower = lower, upper = upper, digits = dig),
           ciwidth = formatNumberLaTeX(number = upper - lower, digits = dig),
           p0 = formatNumberLaTeX(number = p0, digits = digPvals)
           ) |>
    select(labels, type, est, w1, ci, ciwidth, p0)
kable(ORBITtab |> select(-labels),
      format = "latex", booktabs = TRUE, align = "lcccccc",
      col.names = c("",
                    "\\textbf{Log rate ratio}",
                    "\\textbf{Weight ORBIT 3}",
                    "\\textbf{95\\% CI}",
                    "\\textbf{CI width}",
                    "\\textbf{\\textit{P}-value (one-sided)}"),
      bold = TRUE, escape = FALSE) |>
    pack_rows("\\textit{Primary endpoint (log hazard ratio)}", 1, 8,
              escape = FALSE, bold = FALSE, italic = TRUE) |>
    pack_rows("\\textit{Secondary endpoint (log rate ratio)}", 9, 16,
              escape = FALSE, bold = FALSE, italic = TRUE) |>
    row_spec(c(2, 10), hline_after = TRUE) |>
    row_spec(seq(1, nrow(RESPIREtab), by = 2), background = "#F0F0F0")


## ----"morethan2", fig.height = 3----------------------------------------------
## parameters
museq <- seq(-1, 0.5, length.out = 5000)
input_p <- "less"
alpha <- 0.05
null <- 0

## all RESPIRE trials
lower <- c(cilogrr14r1[1], cilogrr28r1[1],
           cilogrr14r2[1], cilogrr28r2[1])
upper <- c(cilogrr14r1[2], cilogrr28r1[2],
           cilogrr14r2[2], cilogrr28r2[2])
labels <- c("RESPIRE 1 14-day", "RESPIRE 1 28-day",
            "RESPIRE 2 14-day", "RESPIRE 2 28-day")
est <- c(logrr14r1, logrr28r1, logrr14r2, logrr28r2)
se <- c(selogrr14r1, selogrr28r1, selogrr14r2, selogrr28r2)
lower2 <- NA # est - se*qnorm(p = 1 - (alpha/2)^2)
upper2 <- NA # est + se*qnorm(p = 1 - (alpha/2)^2)

## compute combined p-value functions
funs <- list("Two-trials rule" = p_wilkinson,
             "Tippett" = p_tippett,
             "Pearson" = p_pearson,
             "Edgington" = p_edgington,
             "Fisher" = p_fisher,
             "Meta-analysis" = p_stouffer)
resList <- lapply(X = seq(1, length(funs)), FUN = function(i) {
    fun <- funs[[i]]
    if (names(funs)[i] != "Meta-analysis") {
        pvals <- fun(estimates = est, SEs = se, mu = museq, input_p = input_p)
        cm <- confMeta(estimates = est, SEs = se, fun = fun, input_p = input_p)
        pointest <- cm$p_max[1]
        lower1 <- cm$joint_cis[1]
        upper1 <- cm$joint_cis[2]
        ## HACK manually compute the 99.875% CIs
        ## some issues in confMeta
        ## cm2 <- confMeta(estimates = est, SEs = se, fun = fun, input_p = input_p,
        ##                 conf_level = 1 - 2*(alpha/2)^2)
        rootFun <- function(mu) {
            fun(estimates = est, SEs = se, mu = mu, input_p = input_p) -
                2*(alpha/2)^2
        }
        lower2 <- uniroot(f = rootFun, interval = c(-2, pointest))$root
        upper2 <- uniroot(f = rootFun, interval = c(pointest, 2))$root
    } else {
        semeta <- 1/sqrt(sum(1/se^2))
        pointest <- sum(est/se^2)*semeta^2
        pvals <- 2*(1 - pnorm(q = abs(pointest - museq)/semeta))
        ci1 <- pointest + c(-1, 1)*qnorm(p = 0.975)*semeta
        lower1 <- ci1[1]
        upper1 <- ci1[2]
        ci2 <- pointest + c(-1, 1)*qnorm(p = 1 - (alpha/2)^2)*semeta
        lower2 <- ci2[1]
        upper2 <- ci2[2]
    }
    summariesDF <- data.frame(method = names(funs)[i], lower = lower1,
                              estimate = pointest, upper = upper1,
                              lower2 = lower2, upper2 = upper2)
    pDF <- data.frame(method = names(funs)[i], mu = museq, p = pvals)
    return(list("summaries" = summariesDF, "p" = pDF))
})
summariesDF <- rbind(data.frame(method = labels, lower = lower, estimate = est,
                                upper = upper, lower2 = lower2, upper2 = upper2),
                     do.call("rbind",
                             lapply(X = resList, FUN = function(x) x$summaries)))
pDF <- rbind(do.call("rbind",
                     lapply(X = seq(1, length(est)), FUN = function(i) {
                         data.frame(method = labels[i], mu = museq,
                                    p = 2*(1 - pnorm(abs(est[i] - museq)/se[i])))
                     })),
             do.call("rbind", lapply(X = resList, FUN = function(x) x$p)))
typelevels <- c("RESPIRE 1 14-day", "RESPIRE 2 14-day",
                "RESPIRE 1 28-day", "RESPIRE 2 28-day",
                "Two-trials rule", "Meta-analysis", "Tippett", "Fisher",
                 "Pearson", "Edgington")
summariesDF$method <- factor(summariesDF$method, levels = rev(typelevels))
pDF$method <- factor(pDF$method, levels = rev(typelevels))

## plot them
cols <- c("RESPIRE 1 14-day" = "#CC79A7E6", "RESPIRE 2 14-day" = "#CC79A7E6",
          "RESPIRE 1 28-day" = "#888888E6", "RESPIRE 2 28-day" = "#888888E6",
          "Meta-analysis" = "#56B4E9F2", "Two-trials rule" = "#000000F2",
          "Tippett" = "#E69F00F2", "Edgington" = "#009E73F2",
          "Fisher" = "#F0E442F2", "Pearson" = "#0072B2F2")
lty <- c("dashed", "dotted", "dashed", "dotted", rep("solid", 6))
names(lty) <- names(cols)
ggplot(data = pDF, aes(x = mu, y = p, color = method, linetype = method)) +
    geom_hline(yintercept = alpha, lty = "longdash", alpha = 0.1) +
    geom_line() +
    geom_pointrange(data = summariesDF,
                    aes(xmin = lower2, xmax = upper2, x = estimate, y = 1.15),
                    position = position_dodge2(width = 0.25), fatten = 0,
                    linewidth = 0.3, alpha = 0.3) +
    geom_pointrange(data = summariesDF,
                    aes(xmin = lower, xmax = upper, x = estimate, y = 1.15),
                    position = position_dodge2(width = 0.25),
                    fatten = 1.1) +
    scale_linetype_manual(values = lty, guide = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = cols, guide = guide_legend(reverse = TRUE)) +
    coord_cartesian(xlim = range(museq), ylim = c(-0.02, 1.25)) +
    scale_x_continuous(breaks = round(seq(-0.9, 0.3, 0.3), 1)) +
    scale_y_continuous(breaks = c(seq(0.25, 1, 0.25), 0.05),
                       sec.axis = sec_axis(~ 1 - .,
                                           breaks = c(seq(0, 0.75, 0.25), 0.95),
                                           labels = scales::percent,
                                           name = "Confidence")) +
    annotate(geom = "segment", arrow = arrow(length = unit(1.2, 'mm'),
                                             type = "closed"),
             alpha = 0.6,
             x = c(-1, 1)*0.02, xend = c(-1, 1)*0.2,
             y = -0.06, yend = -0.06) +
    annotate(geom = "text", alpha = 0.8, label = "Benefit",
             x = -0.1, y = -0.025, size = 2.75) +
    annotate(geom = "text", alpha = 0.8, label = "Harm",
             x = 0.1, y = -0.025, size = 2.75) +
    labs(x = "Log rate ratio", y = bquote(italic(p) * "-value"),
         color = NULL, linetype = NULL) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#00000005"))

## ## does Edgington's median estimate generalize to more than 2 studies?
## ## No! not the same
## subset(summariesDF, method == "Edgington")$estimate # -0.328873
## sum(est/se)/sum(1/se) # -0.3238609


## ----"R-package-illustration", echo = TRUE, fig.height = 3.5, size = "scriptsize"----
library(twotrials) # load package

## combine logRR estimates from RESPIRE trials
results <- twotrials(null = 0, t1 = -0.4942, t2 = -0.1847, se1 = 0.1833,
                     se2 = 0.1738, alternative = "less", level = 0.95)
print(results, digits = 2) # print summary of results


## ----"R-package-illustration2", echo = TRUE, fig.height = 3.5, size = "scriptsize"----
plot(results, xlim = c(-1, 0.5), two.sided = TRUE) # plot p-value functions


## ----"standard-errors-comparison", fig.height = 5-----------------------------
seMA <- function(se1, se2, ...) {
    1/sqrt(1/se1^2 + 1/se2^2)
}
seE <- function(se1, se2, ...) {
    sqrt(2)/(1/se1 + 1/se2)
}

## first and second moment of a random variable
## Y = max(hat(theta)_1 - q*se1, hat(theta)_2 - q*se2)
EY <- function(se1, se2, q, t1, t2) {
    (t1 - se1*q)*
        pnorm((t1 - t2 + q*(se2 - se1))/sqrt(se1^2 + se2^2)) +
        (t2 - se2*q)*
        pnorm((t2 - t1 + q*(se1 - se2))/sqrt(se1^2 + se2^2)) +
        sqrt(se1^2 + se2^2)*
        dnorm((t1 - t2 + q*(se2 - se1))/sqrt(se1^2 + se2^2))
}
EY2 <- function(se1, se2, q, t1, t2) {
    (se1^2 + (t1 - se1*q)^2)*
        pnorm((t1 - t2 + q*(se2 - se1))/sqrt(se1^2 + se2^2)) +
        (se2^2 + (t2 - se2*q)^2)*
        pnorm((t2 - t1 + q*(se1 - se2))/sqrt(se1^2 + se2^2)) +
        (t1 + t2 - q*(se1 + se2))*sqrt(se1^2 + se2^2)*
        dnorm((t1 - t2 + q*(se2 - se1))/sqrt(se1^2 + se2^2))
}

## first and second moment of a random variable
## X = min(hat(theta)_1 + q*se1, hat(theta)_2 + q*se2)
EX <- function(se1, se2, q, t1, t2) {
    (t1 + se1*q)*
        pnorm((t2 - t1 + q*(se2 - se1))/sqrt(se1^2 + se2^2)) +
        (t2 + se2*q)*
        pnorm((t1 - t2 + q*(se1 - se2))/sqrt(se1^2 + se2^2)) -
        sqrt(se1^2 + se2^2)*
        dnorm((t2 - t1 + q*(se2 - se1))/sqrt(se1^2 + se2^2))
}
EX2 <- function(se1, se2, q, t1, t2) {
    (se1^2 + (t1 + se1*q)^2)*
        pnorm((t2 - t1 + q*(se2 - se1))/sqrt(se1^2 + se2^2)) +
        (se2^2 + (t2 + se2*q)^2)*
        pnorm((t1 - t2 + q*(se1 - se2))/sqrt(se1^2 + se2^2)) -
        (t1 + t2 + q*(se1 + se2))*sqrt(se1^2 + se2^2)*
        dnorm((t2 - t1 + q*(se2 - se1))/sqrt(se1^2 + se2^2))
}

se2TR <- function(se1, se2, t1, t2, alternative = "greater") {
    q <- qnorm(p = sqrt(0.5))
    if (alternative == "greater") {
        sqrt(EX2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EX(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    } else {
        sqrt(EY2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EY(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    }
}
seT <- function(se1, se2, t1, t2, alternative = "greater") {
    q <- qnorm(p = sqrt(0.5))
    if (alternative == "greater") {
        sqrt(EY2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EY(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    } else {
        sqrt(EX2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EX(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    }
}
seF <- function(se1, se2, t1, t2, alternative = "greater") {
    ## only approximately valid when t1 != t2
    q <- -qnorm(p = exp(-qchisq(p = 0.25, df = 4)))
    if (alternative == "greater") {
        sqrt(EY2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EY(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    } else {
        sqrt(EX2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EX(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    }
}
seP <- function(se1, se2, t1, t2, alternative = "greater") {
    ## only approximately valid when t1 != t2
    q <- -qnorm(p = exp(-qchisq(p = 0.25, df = 4)))
    if (alternative == "greater") {
        sqrt(EX2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EX(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    } else {
        sqrt(EY2(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2) -
             EY(se1 = se1, se2 = se2, q = q, t1 = t1, t2 = t2)^2)
    }
}


## se1 <- 0.05
## se2 <- 0.1
## theta1 <- 0.45
## theta2 <- 0.5

## ## computing standard errors with simulation
## nsim <- 10000
## set.seed(42)
## t1 <- rnorm(n = nsim, mean = theta1, sd = se1)
## t2 <- rnorm(n = nsim, mean = theta2, sd = se2)
## simres <- do.call("rbind", lapply(X = seq_len(nsim), FUN = function(i) {
##     res <- twotrials(t1 = t1[i], t2 = t2[i], se1 = se1, se2 = se2)
##     data.frame(sim = i, res$summaries[,c("method", "est")])
## }))

## simSE <- tapply(simres$est, simres$method, FUN = sd)
## ## comparing to analytical standard errors
## aSE <- sapply(X = list("Edgington" = seE, "Fisher" = seF,
##                        "Meta-analysis" = seMA, "Pearson" = seP, "Tippett" = seT,
##                        "Two-trials rule" = se2TR),
##               FUN = function(f) f(se1 = se1, se2 = se2, t1 = theta1, t2 = theta2))
## signif(cbind(simSE, aSE), 4)

## ## checking simplified formula for 2TR
## se <- 1
## theta <- 0.5
## q <- qnorm(sqrt(0.5))
## se2TR(se1 = se, se2 = se, t1 = theta, t2 = theta)
## ## first simplification
## sqrt(se^2 + (theta + se*q)^2 - 2*se*(theta + se*q)/sqrt(pi) -
##      (theta + se*(q - 1/sqrt(pi)))^2)
## ## next simplification
## sqrt(se^2 + theta^2 + 2*theta*se*q + se^2*q^2 - 2*se*theta/sqrt(pi) - 2*se^2*q/sqrt(pi) - theta^2 - 2*theta*se*(q - 1/sqrt(pi)) - se^2*(q - 1/sqrt(pi))^2)
## ## next simplification
## sqrt(se^2*(1 - 1/pi))

## evaluate SEs over a grid of parameter values
n1 <- 200
se1 <- sqrt(2/n1)
rse <- c(0.33, 0.5, 1, 1.5, 3) # relative standard error
se2 <- se1*rse
nsim <- 10000
t <- c(0, 0.5, 1)
grid <- expand.grid(se1 = se1, se2 = se2, t1 = t, t2 = t)

## estimate SEs with simulation
## set.seed(120396)
## library(parallel)
## simres <- mclapply(X = seq(1, nrow(grid)), FUN = function(i) {
##     t1 <- rnorm(n = nsim, mean = grid$t1[i], sd = grid$se1[i])
##     t2 <- rnorm(n = nsim, mean = grid$t2[i], sd = grid$se2[i])
##     simdat <- do.call("rbind", lapply(X = seq_len(nsim), FUN = function(j) {
##         res <- twotrials(t1 = t1[j], t2 = t2[j], se1 = grid$se1[i],
##                          se2 = grid$se2[i])
##         data.frame(sim = j, res$summaries[,c("method", "est")])
##     }))
##     res <- data.frame(grid[i,], simdat)
##     return(res)
## }, mc.cores = pmax(detectCores() - 4, 1))
## save(simres, file = "simulation-results.RData")
load("simulation-results.RData")
## simres |>
##     bind_rows() |>
##     group_by(method) |>
##     summarise(n = n(),
##               converged = mean(is.finite(est)))
simsummaries <- simres |>
    bind_rows() |>
    group_by(method, se1, se2, t1, t2) |>
    summarise(se = sd(est)) |>
    mutate(type = "simulation")

## compute analytical SEs
grid2 <- expand.grid(se1 = se1, se2 = seq(min(se2), max(se2), length.out = 100),
                     t1 = t, t2 = t)
summaries <- do.call("rbind", lapply(X = seq(1, nrow(grid2)), FUN = function(i) {
    aSE <- sapply(X = list("Edgington" = seE, "Fisher" = seF,
                           "Meta-analysis" = seMA, "Pearson" = seP, "Tippett" = seT,
                           "Two-trials rule" = se2TR),
                  FUN = function(f) f(se1 = grid2$se1[i], se2 = grid2$se2[i],
                                      t1 = grid2$t1[i], t2 = grid2$t2[i]))
    res <- data.frame(method = names(aSE), grid2[i,], se = aSE, type = "analytical")
    return(res)
})) |>
    rbind(simsummaries) |>
    mutate(method = factor(method, levels = typelevels))

cols <- c("Trial 1" = "#CC79A7F2", "Trial 2" = "#888888F2",
          "Meta-analysis" = "#56B4E9F2", "Two-trials rule" = "#000000F2",
          "Tippett" = "#E69F00F2", "Edgington" = "#009E73F2",
          "Fisher" = "#F0E442F2", "Pearson" = "#0072B2F2")
ggplot(filter(summaries, type == "analytical",
              method != "Pearson", method != "Fisher"),
       ## filter(summaries, type == "analytical"),
       aes(x = se2, y = se, color = method)) +
    facet_grid(t2 ~ t1,
               labeller = label_bquote(theta[2] == .(t2), theta[1] == .(t1))) +
    geom_hline(yintercept = se1, alpha = 0.3) +
    geom_line(aes(linetype = method), alpha = 0.8, linewidth = 0.8) +
    ## geom_point(data = filter(summaries, type == "simulation"),
    ##            aes(shape = method), alpha = 0.9) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c(2, 1, 3, 4, 5, 6)) +
    labs(x = bquote(sigma[2]), y = "Standard error of median estimate",
         color = "", shape = "", linetype = "") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top", legend.key.width = unit(0.9,"cm"),
          strip.background = element_rect(fill = "#00000005"))


## ----"fisher-pearson-edgington-approximation", fig.height = 3.5---------------
## when trials have small standard error
museq <- seq(0.2, 0.8, 0.001)
t1 <- 0.3
t2 <- 0.6
se1 <- 0.05
se2 <- 0.07
z1 <- (t1 - museq)/se1
z2 <- (t2 - museq)/se2
p1 <- 1 - pnorm(z1)
p2 <- 1 - pnorm(z2)
pfis <- 1 - pchisq(q = -2*(log(p1) + log(p2)), df = 4)
ppear <- pchisq(q = -2*(log(1 - p1) + log(1 - p2)), df = 4)
e <- p1 + p2
pedg <- ifelse(e <= 1, e^2/2, 1 - (2 - e)^2/2)

a1 <- 0.025
a2 <- 0.975
estpear1 <- min(c(t1, t2) - c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = a1, df = 4))))
estfis1 <- max(c(t1, t2) + c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = 1 - a1, df = 4))))
estedg1 <- min(c(t1, t2) + c(se1, se2)*qnorm(p = sqrt(2*a1)))
estpear2 <- min(c(t1, t2) - c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = a2, df = 4))))
estfis2 <- max(c(t1, t2) + c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = 1 - a2, df = 4))))
estedg2 <- max(c(t1, t2) - c(se1, se2)*qnorm(p = sqrt(2*(1 - a2))))

par("mar" = c(4.1, 4.1, 1.5, 4.1), "oma" = c(0, 0, 0, 0))
cols <- c("Edgington" = "#009E73F2", "Fisher" = "#F0E442F2",
          "Pearson" = "#0072B2F2")
matplot(museq, cbind(p1, p2), lty = 3, col = c(1, 1), type = "l",
        xlab = bquote(mu), ylab = bquote(italic(p) * "-value"), las = 1,
        lwd = 1.5)
matlines(museq, cbind(pedg, pfis, ppear), lty = 1, col = cols, lwd = 1.5)
legend("right", legend = c("Trial 1", "Trial 2", "Edgington", "Fisher", "Pearson"),
       lty = c(3, 3, 1, 1, 1), col = c(1, 1, cols), lwd = c(1.5, 1.5, 1.5, 1.5, 1.5),
       cex = 0.8)
abline(h = c(a1, a2), lty = 1, col = adjustcolor(col = 1, alpha.f = 0.3))
axis(side = 4, at = c(a1, a2), las = 1)
segments(x0 = c(estedg1, estfis1, estpear1, estedg2, estfis2, estpear2),
         y0 = rep(0, 6), y1 = c(rep(a1, 3), rep(a2, 3)), col = rep(cols, 2),
         lty = 2)
points(x = c(estedg1, estfis1, estpear1, estedg2, estfis2, estpear2),
       y = c(rep(a1, 3), rep(a2, 3)), col = rep(cols, 2), pch = 20, cex = 0.7)

## ## for alternative "less"
## p1 <- pnorm((t1 - museq)/se1)
## p2 <- pnorm((t2 - museq)/se2)
## pfis <- 1 - pchisq(q = -2*(log(p1) + log(p2)), df = 4)
## ppear <- pchisq(q = -2*(log(1 - p1) + log(1 - p2)), df = 4)
## e <- p1 + p2
## pedg <- ifelse(e <= 1, e^2/2, 1 - (2 - e)^2/2)
## estpear1 <- max(c(t1, t2) + c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = a1, df = 4))))
## estfis1 <- min(c(t1, t2) - c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = 1 - a1, df = 4))))
## estedg1 <- max(c(t1, t2) - c(se1, se2)*qnorm(p = sqrt(2*a1)))
## estpear2 <- max(c(t1, t2) + c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = a2, df = 4))))
## estfis2 <- min(c(t1, t2) - c(se1, se2)*qnorm(p = exp(-0.5*qchisq(p = 1 - a2, df = 4))))
## estedg2 <- min(c(t1, t2) + c(se1, se2)*qnorm(p = sqrt(2*(1 - a2))))
## matplot(museq, cbind(p1, p2), lty = 3, col = c(1, 1), type = "l",
##         xlab = bquote(mu), ylab = bquote(italic(p) * "-value"), las = 1,
##         lwd = 1.5)
## matlines(museq, cbind(pedg, pfis, ppear), lty = 1, col = cols, lwd = 1.5)
## legend("right", legend = c("Trial 1", "Trial 2", "Edgington", "Fisher", "Pearson"),
##        lty = c(3, 3, 1, 1, 1), col = c(1, 1, cols), lwd = c(1.5, 1.5, 1.5, 1.5, 1.5),
##        cex = 0.8)
## abline(h = c(a1, a2), lty = 1, col = adjustcolor(col = 1, alpha.f = 0.3))
## axis(side = 4, at = c(a1, a2), las = 1)
## segments(x0 = c(estedg1, estfis1, estpear1, estedg2, estfis2, estpear2),
##          y0 = rep(0, 6), y1 = c(rep(a1, 3), rep(a2, 3)), col = rep(cols, 2),
##          lty = 2)
## points(x = c(estedg1, estfis1, estpear1, estedg2, estfis2, estpear2),
##        y = c(rep(a1, 3), rep(a2, 3)), col = rep(cols, 2), pch = 20, cex = 0.7)



## ----"sessionInfo2", echo = Reproducibility, results = Reproducibility, size = "scriptsize"----
cat(paste(Sys.time(), Sys.timezone(), "\n"))
sessionInfo()

