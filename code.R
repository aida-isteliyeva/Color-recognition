library(lme4)
library(readxl)
#input of data into r
color <- read_excel("C:/Users/1/Downloads/Color.xlsx", sheet = "for r")

colornofil <- read_excel("C:/Users/1/Downloads/Color.xlsx", sheet = "for r wout fil")

#creating boxplots to show the data
boxplot(time ~ side, data = color, xlab = "Location of the color",
        ylab = "Response time")
boxplot(time ~ side, data = colornofil, xlab = "Location of the color",
        ylab = "Response time")

boxplot(time ~ names, data = color, xlab = "Use of color terms",
        ylab = "Response time")
boxplot(time ~ names, data = colornofil, xlab = "Use of color terms",
        ylab = "Response time")

boxplot(time ~ names*side, data = color, xlab = "Use of color terms and Location of the color",
        ylab = "Response time")
boxplot(time ~ names*side, data = colornofil, xlab = "Use of color terms and Location of the color",
        ylab = "Response time")

boxplot(time ~ creativity, data = color, xlab = "creative hobby or work",
        ylab = "Response time")
boxplot(time ~ creativity, data = colornofil, xlab = "creative hobby or work",
        ylab = "Response time")

boxplot(time ~ gen, data = color, xlab = "Gender",
        ylab = "Response time")
boxplot(time ~ gen, data = colornofil, xlab = "Gender",
        ylab = "Response time")

boxplot(time ~ creativity*side, data = color, xlab = "Creativity and location of the color",
        ylab = "Response time")
boxplot(time ~ creativity*side, data = colornofil, xlab = "Creativity and location of the color",
        ylab = "Response time")

boxplot(time ~ other, data = color, xlab = "Filler colors",
        ylab = "Response time")
boxplot(time ~ other, data = colornofil, xlab = "Filler colors",
        ylab = "Response time")

boxplot(time ~ side*names*creativity, data = colornofil, xlab = "Location of the color, color terms, creativity",
        ylab = "Response time")


#calculating the significance of factors

color.all = lmer (time ~ gen + creativity + names + side + other + (1|part), data = colornofil)
color.side = lmer (time ~ gen + creativity + names + other + (1|part), data = colornofil)
anova(color.side,color.all)

color.all = lmer (time ~ gen + creativity + names + side + other + (1|part), data = colornofil)
color.names = lmer (time ~ gen + creativity + side + other + (1|part), data = colornofil)
anova(color.names,color.all)

color.all = lmer (time ~ gen + creativity + names + side + other + (1|part), data = colornofil)
color.namesxside = lmer (time ~ gen + creativity + other + (1|part), data = colornofil)
anova(color.namesxside,color.all)

color.all = lmer (time ~ gen + creativity + names + side + other + (1|part), data = colornofil)
color.creatxside = lmer (time ~ gen + names + other + (1|part), data = colornofil)
anova(color.creatxside,color.all)

color.all = lmer (time ~ gen + creativity + names + side + other + (1|part), data = colornofil)
color.other = lmer (time ~ gen + creativity + names + side + (1|part), data = colornofil)
anova(color.other,color.all)


