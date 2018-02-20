#자동차 등록대수에 전기차 예상 점유율 2%의 값을 넣은것
SS <- read.csv('전기차_2퍼센트_수정.csv', header = T)

h <- SS
h$car <- log(h$car)
h$area <- log(h$area)

#fit <- data.frame(t(coef.lasso))

#write.csv(fit, '라쏘.csv')

h$evStation <- ((0.00098295)*h$car +	(0.014382052)*h$factory +	(0.007664823)*h$apt	+ (-0.008642522)*h$dong_office +	(0)*h$gu_office +	(0)*h$si_office +	(-0.752798919)*h$gun_office +	(0)*h$do_office	 + (0)*h$court	+ (0)*h$police	+ (0.009003527)*h$tourist	+ (0.066828936)*h$area + 2.628088432)

h$expEV <- round(exp(h$evStation))

write.csv(h , '전기충전소 설치 로그푼것.csv')
