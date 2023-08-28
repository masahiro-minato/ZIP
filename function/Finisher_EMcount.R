Metis_MF3_Finisher_EMcount <- function(
    df = Metis_MF3_distinct
    ){
  # AMUR-C,AMUR-C(HY),VOLGA-EのEMカウント
  # 機番毎のEM数の抽出
  Metis_MF3_EM_VOLGA_E_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
    filter(Peripheral_name == "VOLGA-E") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      VOLGA_E_EM_F = n()
    )
  
  print("VOLGA-E EM数")
  print(paste0("  ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部:",sum(Metis_MF3_EM_VOLGA_E_by_Machine_numbers$VOLGA_E_EM_F))) # 1205
  
  Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
    filter(Peripheral_name == "VOLGA-E") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      VOLGA_E_EM_P = n()
    )
  
  print(paste0("  ﾊﾟﾝﾁ部",sum(Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers$VOLGA_E_EM_P))) # 13
  
  Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
    filter(Peripheral_name == "VOLGA-E") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      VOLGA_E_EM_S = n()
    )
  
  print(paste0("  ｽﾃｰﾌﾟﾙ部:",sum(Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers$VOLGA_E_EM_S))) # 40
  
  # 結合
  Metis_MF3_EM_VOLGA_E_all_by_Machine_numbers <- 
    Metis_MF3_EM_VOLGA_E_by_Machine_numbers %>% 
    full_join(Metis_MF3_EM_VOLGA_E_P_by_Machine_numbers, by="Machine_numbers") %>% 
    full_join(Metis_MF3_EM_VOLGA_E_S_by_Machine_numbers, by="Machine_numbers") %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(
      VOLGA_E_EM = VOLGA_E_EM_F + VOLGA_E_EM_P + VOLGA_E_EM_S,
      Finisher = "VOLGA_E"
    )
  
  print(paste0("  合計:",sum(Metis_MF3_EM_VOLGA_E_all_by_Machine_numbers$VOLGA_E_EM))) #
  
  # AMUR-C(HY)
  Metis_MF3_EM_AMUR_CHY_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
    filter(Peripheral_name == "AMUR-C(HY)") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_CHY_EM_F = n()
    )
  print("AMUR-C(HY) EM数")
  print(paste0("  ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部:",sum(Metis_MF3_EM_AMUR_CHY_by_Machine_numbers$AMUR_CHY_EM_F))) # 176
  
  Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
    filter(Peripheral_name == "AMUR-C(HY)") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_CHY_EM_P = n()
    )
  print(paste0("  ﾊﾟﾝﾁ部:",sum(Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers$AMUR_CHY_EM_P))) # 7
  
  Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
    filter(Peripheral_name == "AMUR-C(HY)") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_CHY_EM_S = n()
    )
  
  print(paste0("  ｽﾃｰﾌﾟﾙ部:",sum(Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers$AMUR_CHY_EM_S))) # 9
  
  # 結合
  Metis_MF3_EM_AMUR_CHY_all_by_Machine_numbers <- 
    Metis_MF3_EM_AMUR_CHY_by_Machine_numbers %>% 
    full_join(Metis_MF3_EM_AMUR_CHY_P_by_Machine_numbers, by="Machine_numbers") %>% 
    full_join(Metis_MF3_EM_AMUR_CHY_S_by_Machine_numbers, by="Machine_numbers") %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(
      AMUR_CHY_EM = AMUR_CHY_EM_F + AMUR_CHY_EM_P + AMUR_CHY_EM_S,
      Finisher = "AMUR_CHY"
    )
  
  print(paste0("  合計:",sum(Metis_MF3_EM_AMUR_CHY_all_by_Machine_numbers$AMUR_CHY_EM))) 
  
  # AMUR-C中綴じ
  Metis_MF3_EM_AMUR_C_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部") %>%
    filter(Peripheral_name == "AMUR-C中綴じ") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_C_EM_F = n()
    )
  print("AMUR-C中綴じ EM数")
  print(paste0("  ﾌｨﾆｯｼｬｰ/ｿｰﾀｰ部:",sum(Metis_MF3_EM_AMUR_C_by_Machine_numbers$AMUR_C_EM_F))) # 518
  
  Metis_MF3_EM_AMUR_C_P_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ﾊﾟﾝﾁ部") %>%
    filter(Peripheral_name == "AMUR-C中綴じ") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_C_EM_P = n()
    )
  print(paste0("  ﾊﾟﾝﾁ部:",sum(Metis_MF3_EM_AMUR_C_P_by_Machine_numbers$AMUR_C_EM_P))) # 17
  
  Metis_MF3_EM_AMUR_C_S_by_Machine_numbers <- 
    df %>% 
    filter(Treatment_location == "ｽﾃｰﾌﾟﾙ部") %>%
    filter(Peripheral_name == "AMUR-C中綴じ") %>%
    group_by(Machine_numbers) %>% 
    summarise(
      AMUR_C_EM_S = n()
    )
  
  print(paste0("  ｽﾃｰﾌﾟﾙ部:",sum(Metis_MF3_EM_AMUR_C_S_by_Machine_numbers$AMUR_C_EM_S))) # 70
  
  # 結合
  Metis_MF3_EM_AMUR_C_all_by_Machine_numbers <- 
    Metis_MF3_EM_AMUR_C_by_Machine_numbers %>% 
    full_join(Metis_MF3_EM_AMUR_C_P_by_Machine_numbers, by="Machine_numbers") %>% 
    full_join(Metis_MF3_EM_AMUR_C_S_by_Machine_numbers, by="Machine_numbers") %>% 
    mutate_all(~replace(., is.na(.), 0)) %>% 
    mutate(
      AMUR_C_EM = AMUR_C_EM_F + AMUR_C_EM_P + AMUR_C_EM_S,
      Finisher = "AMUR_C"
    )
  
  print(paste0("  合計:",sum(Metis_MF3_EM_AMUR_C_all_by_Machine_numbers$AMUR_C_EM))) 
  
  # 全体結合
  Metis_MF3_EM_AMUR_by_Machine_numbers <-
    Metis_MF3_EM_AMUR_CHY_all_by_Machine_numbers %>% 
    full_join(Metis_MF3_EM_AMUR_C_all_by_Machine_numbers, by=c("Machine_numbers","Finisher"))
  
  Metis_MF3_EM_Finisher_by_Machine_numbers <<-
    Metis_MF3_EM_VOLGA_E_all_by_Machine_numbers %>% 
    full_join(Metis_MF3_EM_AMUR_by_Machine_numbers, by=c("Machine_numbers","Finisher")) %>% 
    select(Machine_numbers, Finisher, AMUR_C_EM, AMUR_CHY_EM, VOLGA_E_EM)
  
  return(Metis_MF3_EM_Finisher_by_Machine_numbers)
}
