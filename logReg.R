data = read.csv("Z:/football.csv")
data = data.frame(data)

regression = glm(data$TARGET_5Yrs ~ data$Games.Played + data$Points.per.Game 
                  + data$Field.Goals.Made + data$Field.Goals.Attempts + data$X3.Points.Attempts.Percentage , family = binomial)
print(regression)

Name = data$Name
x1 = data$Games.Played
x2 = data$Points.per.Game
x3 = data$Field.Goals.Made
x4 = data$Field.Goals.Attempts
x5 = data$X3.Points.Attempts.Percentage
Target = data$TARGET_5Yrs

for(i in 1:length(x1)){
  y = -1.997140 + 0.029403*x1[i] +  -0.261170*x2[i] + 1.725113*x3[i] +  -0.365539*x4[i] +  -0.003926*x5[i]
  if(y < 0.5 )
    print(paste(Name[i] ," will be there for 5 years "))
  else
    print(paste(Name[i] ," WILL NOT be there for 5 years "))
}



#vizualisation 
plot(x1,Target, col = 'Blue', pch = 17, xlab = 'Games Played', ylab = 'In 5 years')
plot(x2,Target, col = 'Red', pch = 18, xlab = 'Points per Game', ylab = 'In 5 years')
plot(x3,Target, col = 'Green', pch = 13, xlab = 'Field.Goals.Made', ylab = 'In 5 years')
plot(x5,Target, col = 'cyan', pch = 15, xlab = '3.Points.Attempts.Percentage', ylab = 'In 5 years')

