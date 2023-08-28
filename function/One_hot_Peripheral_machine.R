One_hot_Peripheral_machine <- function(
    df = Metis_sheet3, 
    col_list = c("機種機番","顧客名","納品年月日","機種略","製造年月",
                 "ACV","ADF","フィニッシャー","LCT","バンク",
                 "インサーター","スライドソートトレイ","スタッカー",
                 "トリマー","デカーラ","紙折り","インナー1ビン"),
    date = "2022/12/01"){
  # 機器データ(df)の"納品年月日","製造年月"列のdata型への変換と周辺機列のOne_hot化を行う
  # date:稼働月算出の終日指定
  # 使用例 Excel_market_machine(Metis_sheet3)

  # 納品全台数情報である機器データから、必要列を抽出
  Metis_MF3_Count <<- 
    df %>% 
    select(all_of(col_list))
  
  # date型へ変換
  Metis_MF3_Count <-
    Date_conversion(df = "Metis_MF3_Count", col_list = c("納品年月日","製造年月"))
  
  # 周辺機列のOne_hot化
  Metis_MF3_by_model <- 
    Metis_MF3_Count %>% 
    mutate(
      COOK = case_when(
        ADF == "COOK-C" ~ 1,
        ADF == NA ~ 0,
        TRUE ~ 0),
      SINAI = case_when(
        ADF == "SINAI-H" ~ 1,
        ADF == NA ~ 0,
        TRUE ~ 0),
      AMUR_C_HY = case_when(
        フィニッシャー == "AMUR-C(HY)" ~ 1,
        フィニッシャー == NA ~ 0,
        TRUE ~ 0),
      AMUR_C_saddle = case_when(
        フィニッシャー == "AMUR-C中綴じ" ~ 1,
        フィニッシャー == NA ~ 0,
        TRUE ~ 0),
      VOLGA_E = case_when(
        フィニッシャー == "VOLGA-E" ~ 1,
        フィニッシャー == NA ~ 0,
        TRUE ~ 0),
      CANARIA_D = case_when(
        バンク == "CANARIA-D" ~ 1,
        バンク == NA ~ 0,
        TRUE ~ 0),
      GOREE_D = case_when(
        バンク == "GOREE-D" ~ 1,
        バンク == NA ~ 0,
        TRUE ~ 0),
      CUBA_C = case_when(
        バンク == "CUBA-C" ~ 1,
        バンク == NA ~ 0,
        TRUE ~ 0),
      # CANARIA_E = case_when(
      #   バンク == "CANARIA-E" ~ 1,
      #   バンク == NA ~ 0,
      #   TRUE ~ 0),
      THAMES_C = case_when(
        紙折り == "THAMES-C" ~ 1,
        紙折り == NA ~ 0,
        TRUE ~ 0)
    )
  
  Metis_MF3_by_model <- 
    Metis_MF3_by_model %>% 
    select(機種機番,顧客名,納品年月日,機種略,製造年月,
           COOK,SINAI,AMUR_C_HY,AMUR_C_saddle,VOLGA_E,THAMES_C,CANARIA_D,GOREE_D,CUBA_C) %>% 
    rename(Machine_numbers = 機種機番,
           Customer_name = 顧客名,
           Due_date = 納品年月日,
           Model_abbreviation = 機種略,
           Manufacturing_date = 製造年月) %>% 
    # rowwise() %>%
    # mutate(
    #   Elapsed_mf_months = 
    #     (length(seq(as.Date("2018/10/01"), as.Date(Manufacturing_date), "month"))),
    #   Working_months = 
    #     (length(seq(as.Date(Due_date), as.Date(date), "month")))
    # ) %>% 
    # ungroup() %>% 
    select(Machine_numbers,Customer_name,Model_abbreviation,Manufacturing_date,
           Due_date,everything())
           # Due_date,Elapsed_mf_months,Working_months,everything())
  
  return(Metis_MF3_by_model)
}

