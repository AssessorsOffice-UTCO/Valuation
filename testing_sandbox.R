# testing and playing with package functions


devtools::load_all()
devtools::document()
?RatioAnalysis()
df <- data.frame(BR=c(1,2,3,4,1,2,3,4,1,2,3,4),
           SQFT=c(1000,1500,1750,2500,
                  1100,1250,1550,2350,
                  1350,1600,2000,2300),
           ASSESSED=c(300000,350000,475000,560000,
                      349000,387000,421000,622000,
                      368000,402000,521000,600000),
           SOLD=c(295000,356000,470000,555000,
                  342000,380000,411000,630000,
                  363000,407000,501000,610000))
mod <- glm(SOLD ~ BR + SQFT,data=df)

RA <- RatioAnalysis(dat=df,model = mod)
