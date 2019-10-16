# ggplot2 패키지를 사용한 그래프
# grammar of graph(그래프 그리기 문법)
# install.packages("ggplot2")
library(ggplot2)
search()

# ggplot2 패키지의 mpg 데이터 프레임 구조 확인
str(mpg)

# 자동차 배기량(displ)과 시내주행 연비(cty) 사이의 관계
summary(mpg$displ)
summary(mpg$cty)
# 1) 그래프를 그릴 데이터(데이터 프레임), 좌표축 설정
g <- ggplot(data = mpg, mapping = aes(x = displ, y = cty))

# 2) 그래프의 종류 선택
g <- g + geom_point() # 좌표축 + 그래프의 종류
g

# 3) 옵션 추가
g <- g + xlim(3, 6)
g

ggplot(mpg, aes(displ, cty)) +
  geom_point() +
  ylim(10, 30)


# 연습문제
# 1
ggplot(mpg, aes(cty, hwy)) +
  geom_point()

# 2
str(midwest)
ggplot(midwest, aes(poptotal, popasian)) +
  geom_point() +
  xlim(0, 250000) +
  ylim(0, 2500)


str(mpg)
table(mpg$cyl)

ggplot(mpg, aes(cyl, cty)) +
  geom_point()

ggplot(data = mpg, mapping = aes(x = displ, y = cty, 
                                 color = as.factor(cyl),
                                 shape = as.factor(drv))) +
  geom_point()
