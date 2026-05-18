---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

```r
# 1. 读取数据（你的activity.csv和Rmd文件在同一个文件夹，直接写文件名即可）
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

# 2. 把日期列转换成R能识别的标准日期格式
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# 3. 查看数据前几行，确认读取成功
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
# 4. 查看数据的基本信息
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# ------------------------------
# 任务1：计算每日总步数（忽略缺失值）
# ------------------------------
daily_steps <- activity %>%
  filter(!is.na(steps)) %>%  # 过滤掉steps为NA的行
  group_by(date) %>%         # 按日期分组
  summarise(total_steps = sum(steps))  # 按组求和，得到每天总步数
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# ------------------------------
# 任务2：绘制每日总步数的直方图（作业要求）
# ------------------------------
ggplot(daily_steps, aes(x = total_steps)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  labs(title = "每日总步数分布（忽略缺失值）",
       x = "每日总步数", y = "天数") +
  theme_minimal()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# ------------------------------
# 任务3：计算每日总步数的均值和中位数（作业要求）
# ------------------------------
mean_daily <- mean(daily_steps$total_steps)
median_daily <- median(daily_steps$total_steps)

# 输出结果，方便你写报告时参考
cat("✅ 每日总步数的平均值为：", mean_daily, "\n")
```

```
## ✅ 每日总步数的平均值为： 10766.19
```

```r
cat("✅ 每日总步数的中位数为：", median_daily, "\n")
```

```
## ✅ 每日总步数的中位数为： 10765
```

```r
# 计算每个5分钟间隔的平均步数
interval_avg <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# 绘制时间序列图
ggplot(interval_avg, aes(x=interval, y=avg_steps)) +
  geom_line(color="darkred") +
  labs(title="5分钟间隔平均步数", x="时间间隔", y="平均步数")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# 找出步数最多的间隔
max_step <- interval_avg[which.max(interval_avg$avg_steps),]
cat("步数最多的间隔：", max_step$interval, "，步数：", max_step$avg_steps)
```

```
## 步数最多的间隔： 835 ，步数： 206.1698
```

```r
# 统计缺失值数量
cat("总缺失值：", sum(is.na(activity$steps)), "\n")
```

```
## 总缺失值： 2304
```

```r
# 用【同时间段平均值】填补缺失值
interval_mean <- activity %>%
  group_by(interval) %>%
  summarise(mean_steps=mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
activity_new <- activity %>%
  left_join(interval_mean, by="interval") %>%
  mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
  select(-mean_steps)

# 验证：填补后缺失值=0
cat("填补后缺失值：", sum(is.na(activity_new$steps)))
```

```
## 填补后缺失值： 0
```

```r
# 计算填补后每日总步数
daily_new <- activity_new %>%
  group_by(date) %>%
  summarise(total=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# 画直方图
ggplot(daily_new, aes(x=total)) +
  geom_histogram(bins=20, fill="green", color="black") +
  labs(title="填补缺失值后每日步数", x="总步数", y="天数")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# 对比结果
cat("填补前均值：", mean(daily_steps$total_steps), "\n")
```

```
## 填补前均值： 10766.19
```

```r
cat("填补后均值：", mean(daily_new$total), "\n")
```

```
## 填补后均值： 10766.19
```

```r
cat("填补前中位数：", median(daily_steps$total_steps), "\n")
```

```
## 填补前中位数： 10765
```

```r
cat("填补后中位数：", median(daily_new$total))
```

```
## 填补后中位数： 10766.19
```

```r
# 标记工作日/周末
activity_new <- activity_new %>%
  mutate(week=weekdays(date),
         day_type=ifelse(week %in% c("Saturday","Sunday"),"周末","工作日"))

# 分组计算平均步数
week_compare <- activity_new %>%
  group_by(day_type, interval) %>%
 summarise(avg=mean(steps), .groups = "drop")

# 画对比图
ggplot(week_compare, aes(x=interval, y=avg)) +
  geom_line(color="blue") +
  facet_wrap(~day_type, nrow=2) +
  labs(title="工作日vs周末步数对比", x="间隔", y="平均步数")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


