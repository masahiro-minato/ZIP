# データ読込
d <- read.csv('./input/data-ZIP.txt')

# 列名変更
colnames(d) <- c("性別_女性","酒_飲む","年齢","来店回数")
# 要素を数値⇒factor
d$性別 <- ifelse(d$性別==0, "男性", "女性")
d$酒 <- ifelse(d$酒==0, "飲まない", "飲む")
d$性別 <- as.factor(d$性別)
d$性別 <- factor(d$性別, levels=c("男性", "女性"))
d$酒 <- as.factor(d$酒)
d$酒 <-factor(d$酒, levels=c("飲まない", "飲む"))
# 元の数値データ列を削除
d <- d %>% 
  select(-性別_女性,-酒_飲む)

# MCMCサンプリング
Ret <- ZIP_calc (data = d, # 入力データ　tibble形式で応答変数を含める
                     value = "来店回数", # 応答変数
                     path_data_save = str_c("./CSV/ZIP-data", "_", lubridate::today(), ".csv"),
                     save_data = TRUE,
                     col_list = c(1:200), # pivot_longerでの列名
                     names = "ID", # pivot_longerにおけるnames
                     device = "CPU", # "CPU" or "GPU" or "CPU.rd"
                     opencl_ids = c(0, 0), # GPUのPlatform番号とDevice番号
                     path_ZIP = "./stan/ZIP_3_CPU.stan", # Stanファイルのアドレス
                     grainsize = 1, # 並列処理（reduce_sum）における粒度 1で最適化だが怪しい
                     chains = 4, 
                     parallel_chains = 4,
                     iter_warmup = 1000,
                     iter_sampling = 500,
                     thin = 1,
                     max_treedepth = 15,
                     refresh = 100,
                     path_fit_remove = "./CmdStan_files/", # サンプリング結果ファイルの移動先
                     path_fit_save = "./ZIP_fit/ZIP_CmdStan_飲食店来店回数.rds",
                     save_fit = TRUE,
                     N_col = 200, # qlの取得列数
                     N_row = 300, # ql,b12の取得行数
                     path_ql = str_c("./CSV/ZIP-ql", "_", lubridate::today(), ".csv"),
                     save_ql = TRUE,
                     path_b12 = str_c("./CSV/ZIP-b12_回数", "_", lubridate::today(), ".csv"),
                     path_b = str_c("./CSV/ZIP-b", "_", lubridate::today(), ".csv"),
                     save_b = TRUE,
                     rescale = TRUE, # 回帰係数の正規化を元に戻す/説明変数の元のスケールに戻す
                     sel_colname = c("酒", "性別"), # c("酒_飲む", "性別_女性"),
                     digits = 2, # グラフ表示の中央値の小数点以下桁数
                     prob = TRUE, # TRUEにて確率の回帰係数の降順でソートする/FALSEは回数　
                     path_graph = "./PDF/回帰係数_飲食店来店回数.pdf",
                     save_graph = TRUE,
                     dpi = 100,
                     width = 6, 
                     height = 4)
