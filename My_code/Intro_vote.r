path <- "E:\\Study\\Quantitative Social Science\\Data\\INTRO\\turnout.csv"
data <- read.csv(path)
print(summary(data))
View(data)

# Q1 - 总共14个样本年，year范围为1980到2008年，间隔为2

VRA <- 100 * data$total / (data$VAP + data$overseas)
VRE <- 100 * data$total / (data$VEP + data$overseas)

print(mean(VRA)) # 使用适龄人口算出的投票率
print(mean(VRE)) # 使用有投票权人口算出的投票率

# Q2 & Q3 - 容易看到用VAP算出来的投票率要低于用VEP算出来的，
# 这是因为VAP要高于VEP，适龄人口不一定都是有选举权的，
# 可以考虑对VEP加入加入犯有重罪和非公民的人数来进行验证：

VRE2 <- 100 * data$total / (data$VEP + data$oversea + data$felons + data$noncit)
print(mean(VRE2))

# 观察到加入两类人群后，用VAP算出的投票率反而高于VEP算出来的了，
# 这是因为两类人群并不一定都是适龄的，可能导致加上它们的VEP大于VAP

# Q4 - 2020是总统选举年，可以推知4的倍数为大选年，其余为中期选举

data$VRA <- VRA
data$VRE <- VRE

dt_big <- data[data$year %% 4 == 0, ]
dt_small <- data[data$year %% 4 == 2, ]

print(dt_big$VRE)
print(dt_small$VRE)

# 很明显大选年的投票率要高于中期选举年，接下来用t检验验证之

print(t.test(dt_big$VRE, dt_small$VRE, alternative = "greater", var.equal = FALSE))

# 但Q4问的是bias是否存在差异，因此先打印VRE和ANES的差

print(dt_big$ANES - dt_big$VRE)
print(dt_small$ANES - dt_small$VRE)

# 整体上似乎也是大选年的bias高于中期，
# 但存在一些例外，例如1982年和2002年，
# 同样使用单尾t检验来检验之

dif_big <- dt_big$ANES - dt_big$VRE
dif_small <- dt_small$ANES - dt_small$VRE
print(t.test(dif_big, dif_small, alternative = "greater", var.equal = FALSE))

# 没有通过检验（alpha=0.1），因此总体上bias上不存在显著差异

# Q5 - 同样用切片把data分成上下两半

dt_early <- data[1:7, ]
dt_late <- data[8:14, ]

# 先看看投票率是否有变化

print(dt_early$VRE)
print(dt_late$VRE)

# 直观上看，中期投票率无明显变化，但大选年投票率似乎在上升，
# 此处不对得票率变化趋势进行验证；
# 接下来计算bias是否存在时间上的差异

print(-dt_early$VRE + dt_early$ANES)
print(-dt_late$VRE + dt_late$ANES)

dif_early <- dt_early$ANES - dt_early$VRE
dif_late <- dt_late$ANES - dt_late$VRE

print(t.test(dif_early, dif_late, alternative = "greater", var.equal = FALSE))

# 没有通过检验，p值极高；
# 尝试一下比较大选年的VER和bias是否有时间趋势，
# 此处计算spearman相关系数，year和VER

print(cor.test(dt_big$year, dt_big$VRE))
print(cor.test(dt_big$year, dt_big$ANES - dt_big$VRE))

# 均未通过检验，但似乎VRE和year更相关一点

# Q6

data$osvoters[is.na(data$osvoters)] <- 0
VRA_adj <- (data$total - data$osvoters) / (data$VAP - data$felons - data$noncit)
VRA_adj <- VRA_adj * 100
data$VRA_adj <- VRA_adj
data$bias <- data$ANES - data$VRA_adj
View(data)

# 调整后的VRA比VRA要高1-5个百分点，也比VRE高0-2个百分点，
# 这说明国内选民的投票意愿要高于海外选民；
# 但即便只计算国内选民，我们看到投票率仍旧远低于ANES，
# 这意味着bias以较高水平存在;
# bias随时间变化的趋势不明显，体现的规律更接近周期性变化，
# 暗示了政治周期可能会影响选民对投票义务的认同。
