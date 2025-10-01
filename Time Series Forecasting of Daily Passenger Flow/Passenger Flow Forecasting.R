# -----------------------------
# 安装和加载包
# -----------------------------
install.packages(c("tidyverse", "lubridate", "forecast", "Metrics", "purrr", "readxl"))
library(tidyverse)
library(lubridate)
library(forecast)
library(Metrics)
library(purrr)
library(readxl)

# -----------------------------
# 文件夹路径（修改为你自己的路径）
# -----------------------------
train_folder <- "C:/Users/Cynth/Desktop/RTS/data_training"
test_folder  <- "C:/Users/Cynth/Desktop/RTS/data_testing"
output_dir   <- "C:/Users/Cynth/Desktop/RTS/outputs"
plots_dir    <- file.path(output_dir, "plots")

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# -----------------------------
# 码头信息
# -----------------------------
docks <- c("Hong Kong International Airport",
           "Hong Kong-Zhuhai-Macao Bridge",
           "Lo Wu",
           "Lok Ma Chau Spur Line",
           "Shenzhen Bay")
short_names <- c("HKIA","HZMB","LoWu","LMC","SZBay")

train_files <- paste0(train_folder, "/", docks, ".xlsx")
test_files  <- paste0(test_folder, "/", short_names, "_real.xlsx")

# -----------------------------
# 核心函数：预测与评估
# -----------------------------
forecast_and_evaluate_file <- function(train_file, test_file, h_eval=8, h_forecast=30){
  
  dock_name <- tools::file_path_sans_ext(basename(train_file))
  
  # ---------- 读取训练数据 ----------
  train_data <- read_excel(train_file)
  names(train_data) <- trimws(names(train_data))
  
  if(!all(c("Date","Total") %in% names(train_data))) stop("列必须包含 Date 和 Total")
  
  train_data <- train_data %>% rename(date=Date, passengers=Total)
  train_data$passengers <- as.numeric(train_data$passengers)
  train_data$date <- lubridate::dmy(train_data$date)
  train_data <- train_data %>% filter(!is.na(date) & !is.na(passengers))
  
  ts_train <- ts(train_data$passengers, frequency=7)
  
  # ---------- 读取验证数据 ----------
  test_data <- read_excel(test_file)
  names(test_data) <- trimws(names(test_data))
  test_data <- test_data %>% rename(date=Date, passengers=Total)
  test_data$passengers <- as.numeric(test_data$passengers)
  test_data$date <- lubridate::dmy(test_data$date)
  test_data <- test_data %>% filter(!is.na(date) & !is.na(passengers))
  
  actual <- test_data$passengers
  
  # ---------- 四种模型预测 ----------
  fit_sarima <- auto.arima(ts_train)
  fit_ets    <- ets(ts_train)
  fit_hw     <- HoltWinters(ts_train)
  fit_naive  <- naive(ts_train, h=h_eval)
  
  fc_sarima <- forecast(fit_sarima, h=h_eval)
  fc_ets    <- forecast(fit_ets, h=h_eval)
  fc_hw     <- forecast(fit_hw, h=h_eval)
  fc_naive  <- forecast(fit_naive, h=h_eval)
  
  # ---------- 计算 MAPE ----------
  mape_values <- c(
    SARIMA=mape(actual, fc_sarima$mean),
    ETS=mape(actual, fc_ets$mean),
    HoltWinters=mape(actual, fc_hw$mean),
    Naive=mape(actual, fc_naive$mean)
  )
  
  # ---------- 短期预测可视化 ----------
  eval_df <- data.frame(
    day=1:h_eval,
    Actual=actual,
    SARIMA=as.numeric(fc_sarima$mean),
    ETS=as.numeric(fc_ets$mean),
    HoltWinters=as.numeric(fc_hw$mean),
    Naive=as.numeric(fc_naive$mean)
  )
  
  eval_long <- eval_df %>% pivot_longer(-day, names_to="Model", values_to="Passengers")
  
  p_eval <- ggplot(eval_long, aes(x=day, y=Passengers, color=Model)) +
    geom_line(size=1) + geom_point(size=2) +
    labs(title=paste("Short-term Forecast vs Actual:", dock_name),
         x="Day", y="Passengers") +
    theme_minimal()
  
  ggsave(filename=file.path(plots_dir, paste0(dock_name,"_shortterm.png")), plot=p_eval, width=8, height=5)
  
  # ---------- 选择最优模型 ----------
  best_model <- names(which.min(mape_values))
  
  # ---------- 全量数据拟合 ----------
  full_ts <- ts(c(train_data$passengers, actual), frequency=7)
  if(best_model=="SARIMA") best_fit <- auto.arima(full_ts)
  else if(best_model=="ETS") best_fit <- ets(full_ts)
  else if(best_model=="HoltWinters") best_fit <- HoltWinters(full_ts)
  else best_fit <- naive(full_ts, h=h_forecast)
  
  forecast_dates <- seq(from=max(test_data$date)+1, by="day", length.out=h_forecast)
  fc_final <- forecast(best_fit, h=h_forecast)
  
  forecast_df <- data.frame(
    date=forecast_dates,
    dock=dock_name,
    forecast=as.numeric(fc_final$mean)
  )
  
  # ---------- 30天预测可视化 ----------
  p_forecast <- ggplot(forecast_df, aes(x=date, y=forecast)) +
    geom_line(color="blue", size=1) +
    labs(title=paste("30-day Forecast:", dock_name),
         x="Date", y="Passengers") +
    theme_minimal()
  ggsave(filename=file.path(plots_dir, paste0(dock_name,"_30days.png")), plot=p_forecast, width=8, height=5)
  
  # 返回结果
  list(
    dock=dock_name,
    mape=mape_values,
    best_model=best_model,
    forecast_final=forecast_df
  )
}

# -----------------------------
# 批量处理 5 个码头
# -----------------------------
results <- map2(train_files, test_files, ~forecast_and_evaluate_file(.x,.y))

# ---------- 汇总 MAPE ----------
evaluation_summary <- map_dfr(results, function(res){
  data.frame(
    dock=res$dock,
    SARIMA=res$mape["SARIMA"],
    ETS=res$mape["ETS"],
    HoltWinters=res$mape["HoltWinters"],
    Naive=res$mape["Naive"],
    best_model=res$best_model
  )
})

write.csv(evaluation_summary, file.path(output_dir,"evaluation_summary.csv"), row.names=FALSE)

# ---------- 汇总 30天预测 ----------
final_forecasts <- map_dfr(results, "forecast_final")
write.csv(final_forecasts, file.path(output_dir,"forecast_30days.csv"), row.names=FALSE)

# ---------- 五个码头 MAPE 折线图 ----------
mape_long <- evaluation_summary %>% pivot_longer(cols=c("SARIMA","ETS","HoltWinters","Naive"),
                                                 names_to="Model", values_to="MAPE")
p_mape <- ggplot(mape_long, aes(x=dock, y=MAPE, color=Model, group=Model)) +
  geom_line(size=1) + geom_point(size=2) +
  labs(title="MAPE Comparison by Dock", x="Dock", y="MAPE") +
  theme_minimal()
ggsave(filename=file.path(plots_dir,"MAPE_comparison.png"), plot=p_mape, width=8, height=5)

# ---------- 所有码头 30天预测叠加图 ----------
p_all_forecast <- ggplot(final_forecasts, aes(x=date, y=forecast, color=dock)) +
  geom_line(size=1) +
  labs(title="30-day Forecast for All Docks", x="Date", y="Passengers") +
  theme_minimal()
ggsave(filename=file.path(plots_dir,"All_Docks_30days.png"), plot=p_all_forecast, width=10, height=6)

cat("✅ 所有预测和评估已完成，输出保存在：", output_dir, "\n")
