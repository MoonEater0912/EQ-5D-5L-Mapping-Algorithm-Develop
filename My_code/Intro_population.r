path1 <- "E:\\Study\\Quantitative Social Science\\Data\\INTRO\\Kenya.csv"
Kenya <- read.csv(path1)

path2 <- "E:\\Study\\Quantitative Social Science\\Data\\INTRO\\Sweden.csv"
Sweden <- read.csv(path2)

path3 <- "E:\\Study\\Quantitative Social Science\\Data\\INTRO\\World.csv"
World <- read.csv(path3)

# Q1 - 分别为三个表插入CBR列，然后编写一个函数分别计算各表两个period的总CBR

Kenya$CBR <- Kenya$births / (Kenya$py.men + Kenya$py.women)
Sweden$CBR <- Sweden$births / (Sweden$py.men + Sweden$py.women)
World$CBR <- World$births / (World$py.men + World$py.women)

cal_cbr <- function(data, period) {
    sum_birth <- sum(data[data$period == period, ]$births)
    sum_pymen <- sum(data[data$period == period, ]$py.men)
    sum_pywomen <- sum(data[data$period == period, ]$py.women)
    return(sum_birth / (sum_pymen + sum_pywomen))
}

print(cal_cbr(Kenya, "1950-1955"))
print(cal_cbr(Kenya, "2005-2010"))
print(cal_cbr(Sweden, "1950-1955"))
print(cal_cbr(Sweden, "2005-2010"))
print(cal_cbr(World, "1950-1955"))
print(cal_cbr(World, "2005-2010"))

# 20-30岁是不论哪个period都具有最高CBR的年龄段，对Sweden和World来说这一范围可以缩小到25-29；
# 从1955到2005出现了显著的CBR下降。

# Q2 & Q3 - 考虑编写函数在三个表中分别插入ASFR列，并编写函数计算TFR

insert_asfr <- function(data) {
    data$ASFR <- data$births / data$py.women
    return(data)
}

Kenya <- insert_asfr(Kenya)
Sweden <- insert_asfr(Sweden)
World <- insert_asfr(World)

cal_tfr <- function(data, period) {
    period_data <- data[data$period == period, ][4:10, ]
    return(5 * sum(period_data$ASFR))
    # 这里要乘5，因为ASFR中分母为person-year，需要乘上年数保持量纲
    # ASFR = 该年龄段女性预期生育子女数每人每年
    # TFR = 生育年龄期间女性预期生育子女数每人
}

print(cal_tfr(Kenya, "1950-1955"))
print(cal_tfr(Kenya, "2005-2010"))
print(cal_tfr(Sweden, "1950-1955"))
print(cal_tfr(Sweden, "2005-2010"))
print(cal_tfr(World, "1950-1955"))
print(cal_tfr(World, "2005-2010"))

# 从结果可以看到，不论哪个地区，从1955到2005的TFR都经历了下降；
# 类似于CBR的变化，Sweden的下降幅度较小，且基数也最小，World次之，Kenya最高。

# Q4 - 类似计算CBR地计算CDR

Kenya$CDR <- Kenya$deaths / (Kenya$py.men + Kenya$py.women)
Sweden$CDR <- Sweden$deaths / (Sweden$py.men + Sweden$py.women)
World$CDR <- World$deaths / (World$py.men + World$py.women)

cal_cdr <- function(data, period) {
    sum_death <- sum(data[data$period == period, ]$deaths)
    sum_pymen <- sum(data[data$period == period, ]$py.men)
    sum_pywomen <- sum(data[data$period == period, ]$py.women)
    return(sum_death / (sum_pymen + sum_pywomen))
}

print(cal_cdr(Kenya, "1950-1955"))
print(cal_cdr(Kenya, "2005-2010"))
print(cal_cdr(Sweden, "1950-1955"))
print(cal_cdr(Sweden, "2005-2010"))
print(cal_cdr(World, "1950-1955"))
print(cal_cdr(World, "2005-2010"))

# Kenya和World的总和死亡率经历了剧烈下降，不过Kenya仍旧保持在世界平均水平以上；
# 有趣的是Swede的死亡率未见下降，甚至在2005-2010年间死亡率超过了世界平均水平;
# 分析数据可知，这里存在一个“辛普森悖论”：
# 21世纪瑞典老龄化造成80+人口占比急剧上升，虽然相比于50年前，80+人口死亡率还略有下降，
# 但由于死亡率本身较高的人群占比上升了，这会导致总体死亡率也呈现出上升。

# Q5 - 需要计算的ASDR即前面算出来的CDR列，可以绘制一个柱状图观察分布

Kenya_cdr <- Kenya[Kenya$period == "2005-2010", ]$CDR
Sweden_cdr <- Sweden[Sweden$period == "2005-2010", ]$CDR
x_labels <- Kenya[Kenya$period == "2005-2010", ]$age

barplot(rbind(Kenya_cdr, Sweden_cdr),
    names.arg = x_labels,
    xlab = "Age", ylab = "CDR",
    main = "CDR by age groups in Sweden and Kenya",
    col = c("steelblue", "goldenrod"),
    legend = c("Kenya", "Sweden"),
    beside = TRUE
)

# 图像符合上文分析，即Kenya每个年龄段的死亡率均高于Sweden。

# Q6 - 预期结果应该是用Kenya的死亡率乘Sweden的人口分布，算出的总和CDR会远高于两国原本的CDR，
# 这里使用向量点乘进行计算：

conterfactual_cdr <- sum(Kenya_cdr * prop.table((Sweden[Sweden$period == "2005-2010", ]$deaths / Sweden_cdr)))
print(conterfactual_cdr)

# 计算出的反事实cdr既高于Kenya的0.0104，也高于Sweden的0.0099，
# 这是因为Sweden的人口结构比Kenya更老龄化，
# 而Kenya的每个年龄段死亡率都比Sweden高。
