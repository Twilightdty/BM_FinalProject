BM_FinalProject
================
Chris Deng
2023-12-10

``` r
# 加载必要的库
library(dplyr)
```

    ## 
    ## 载入程辑包：'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(caret)
```

    ## Warning: 程辑包'caret'是用R版本4.3.2 来建造的

    ## 载入需要的程辑包：ggplot2

    ## 载入需要的程辑包：lattice

``` r
# 加载数据
data <- read.csv("data/Project_2_data.csv")
```

``` r
# 数据处理
# 年龄（Age）保持不变
# 种族（Race）转换为哑变量
data <- data %>%
  mutate(Race = factor(Race, levels = c("White", "Black", "Other"))) %>%
  mutate(dummy_race = model.matrix(~ Race - 1, data))

# 婚姻状况（Marital Status）转换为哑变量，以单身（Single）作为参照
data <- data %>%
  mutate(MaritalStatus = factor(MaritalStatus, levels = c("Single", "Seperated", "Married", "Divorced", "Widowed"))) %>%
  mutate(dummy_marital = model.matrix(~ MaritalStatus - 1, data))

# T阶段（T Stage）T1, T2, T3, T4分别转换为1, 2, 3, 4
data$TStage <- as.numeric(sub("T", "", data$TStage))

# N阶段（N Stage）N1, N2, N3分别转换为1, 2, 3
data$NStage <- as.numeric(sub("N", "", data$NStage))

# 6th Stage转换为哑变量
data <- data %>%
  mutate(SixthStage = factor(SixthStage)) %>%
  mutate(dummy_sixthstage = model.matrix(~ SixthStage - 1, data))

data <- data %>%
  mutate(differentiate = factor(differentiate, levels = c("Well differentiated", "Moderately differentiated", "Poorly differentiated", "Undifferentiated"))) %>%
  mutate(dummy_differentiate = model.matrix(~ differentiate - 1, data))

# A阶段（A Stage）将Regional编码为0，Distant编码为1
data$AStage <- ifelse(data$AStage == "Regional", 0, 1)

# 肿瘤大小（Tumor Size）作为连续变量处理
# 雌激素状态（Estrogen Status）Negative编码为0，Positive编码为1
data$EstrogenStatus <- ifelse(data$EstrogenStatus == "Negative", 0, 1)

# 孕激素状态（Progesterone Status）Negative编码为0，Positive编码为1
data$ProgesteroneStatus <- ifelse(data$ProgesteroneStatus == "Negative", 0, 1)

# 区域节点检查（Regional Node Examined）和区域节点阳性（Regional Node Positive）保持不变

# 死亡状态（Status）Dead编码为1，Alive编码为0
data$Status <- ifelse(data$Status == "Dead", 1, 0)
```

Which factors (features) affect the risk significantly? Are there
interacting effects

``` r
model <- glm(Status ~ Age + dummy_race + dummy_marital + TStage + NStage + dummy_sixthstage + dummy_differentiate + Grade + AStage + TumorSize + EstrogenStatus + ProgesteroneStatus + RegionalNodeExamined + RegionalNodePositive, data = data, family = "binomial")

summary(model)
```

    ## 
    ## Call:
    ## glm(formula = Status ~ Age + dummy_race + dummy_marital + TStage + 
    ##     NStage + dummy_sixthstage + dummy_differentiate + Grade + 
    ##     AStage + TumorSize + EstrogenStatus + ProgesteroneStatus + 
    ##     RegionalNodeExamined + RegionalNodePositive, family = "binomial", 
    ##     data = data)
    ## 
    ## Coefficients: (5 not defined because of singularities)
    ##                                                             Estimate Std. Error
    ## (Intercept)                                               -4.3660968  0.9058253
    ## Age                                                        0.0242440  0.0056132
    ## dummy_raceRaceBlack                                        0.5092789  0.1618161
    ## dummy_raceRaceOther                                       -0.4146676  0.2022866
    ## dummy_raceRaceWhite                                               NA         NA
    ## dummy_maritalMaritalStatusDivorced                        -0.0216811  0.2209150
    ## dummy_maritalMaritalStatusMarried                         -0.2335364  0.1922854
    ## dummy_maritalMaritalStatusSeparated                        0.6527606  0.4087114
    ## dummy_maritalMaritalStatusSingle                          -0.0900601  0.2184996
    ## dummy_maritalMaritalStatusWidowed                                 NA         NA
    ## TStage                                                     0.2954178  0.1315494
    ## NStage                                                     0.6399050  0.2251952
    ## dummy_sixthstageSixthStageIIA                              0.5930218  0.5302285
    ## dummy_sixthstageSixthStageIIB                              0.8020851  0.4841996
    ## dummy_sixthstageSixthStageIIIA                             0.4782293  0.3124704
    ## dummy_sixthstageSixthStageIIIB                             0.7550575  0.4570561
    ## dummy_sixthstageSixthStageIIIC                                    NA         NA
    ## dummy_differentiatedifferentiateModerately differentiated  0.5375129  0.1840940
    ## dummy_differentiatedifferentiatePoorly differentiated      0.9274153  0.1926942
    ## dummy_differentiatedifferentiateUndifferentiated           1.9029277  0.5554682
    ## dummy_differentiatedifferentiateWell differentiated               NA         NA
    ## Grade                                                             NA         NA
    ## AStage                                                     0.0521960  0.2630044
    ## TumorSize                                                 -0.0002362  0.0035880
    ## EstrogenStatus                                            -0.7421047  0.1778956
    ## ProgesteroneStatus                                        -0.5854537  0.1276772
    ## RegionalNodeExamined                                      -0.0359495  0.0071838
    ## RegionalNodePositive                                       0.0791796  0.0153468
    ##                                                           z value Pr(>|z|)    
    ## (Intercept)                                                -4.820 1.44e-06 ***
    ## Age                                                         4.319 1.57e-05 ***
    ## dummy_raceRaceBlack                                         3.147 0.001648 ** 
    ## dummy_raceRaceOther                                        -2.050 0.040374 *  
    ## dummy_raceRaceWhite                                            NA       NA    
    ## dummy_maritalMaritalStatusDivorced                         -0.098 0.921819    
    ## dummy_maritalMaritalStatusMarried                          -1.215 0.224546    
    ## dummy_maritalMaritalStatusSeparated                         1.597 0.110239    
    ## dummy_maritalMaritalStatusSingle                           -0.412 0.680211    
    ## dummy_maritalMaritalStatusWidowed                              NA       NA    
    ## TStage                                                      2.246 0.024725 *  
    ## NStage                                                      2.842 0.004489 ** 
    ## dummy_sixthstageSixthStageIIA                               1.118 0.263385    
    ## dummy_sixthstageSixthStageIIB                               1.657 0.097617 .  
    ## dummy_sixthstageSixthStageIIIA                              1.530 0.125898    
    ## dummy_sixthstageSixthStageIIIB                              1.652 0.098534 .  
    ## dummy_sixthstageSixthStageIIIC                                 NA       NA    
    ## dummy_differentiatedifferentiateModerately differentiated   2.920 0.003503 ** 
    ## dummy_differentiatedifferentiatePoorly differentiated       4.813 1.49e-06 ***
    ## dummy_differentiatedifferentiateUndifferentiated            3.426 0.000613 ***
    ## dummy_differentiatedifferentiateWell differentiated            NA       NA    
    ## Grade                                                          NA       NA    
    ## AStage                                                      0.198 0.842685    
    ## TumorSize                                                  -0.066 0.947524    
    ## EstrogenStatus                                             -4.172 3.03e-05 ***
    ## ProgesteroneStatus                                         -4.585 4.53e-06 ***
    ## RegionalNodeExamined                                       -5.004 5.61e-07 ***
    ## RegionalNodePositive                                        5.159 2.48e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 3444.7  on 4023  degrees of freedom
    ## Residual deviance: 2952.1  on 4001  degrees of freedom
    ## AIC: 2998.1
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
# 使用模型进行预测
predictions <- predict(model, type = "response")
head(predictions)
```

    ##          1          2          3          4          5          6 
    ## 0.06866392 0.12517987 0.28300195 0.11282016 0.13402427 0.04857410

- Age,Race,Tstage,Nstage,differentiated,EstrogenStatus,ProgesteroneStatus,
  RegionalNodeExamined, RegionalNodePositive are significant.

- 找交互作用，differentiate和Grade肯定有, differentiate和Grade基本上等价
