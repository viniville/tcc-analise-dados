# ......::::::: Constants :::::::......
pathbase = "/home/vinicius/dev-projects/java/unisinos/tcc/tcc-app-leitura-agrup-arq-resultados/output-processing"


projects_names <- list("ant-design.csv","axios.csv","bitcoin.csv","bootstrap.csv","caffe.csv","code-server.csv","core.csv","django.csv","dubbo.csv",
                       "electron.csv","emscripten.csv","faceswap.csv","flask.csv","googletest.csv","hashcat.csv","hibernate-orm.csv","jq.csv","jquery.csv","json.csv",
                       "junit5.csv","keras.csv","libuv.csv","lombok.csv","Magisk.csv","mockito.csv","nest.csv","netdata.csv","netty.csv","obs-studio.csv","stb.csv",
                       "pandas.csv","protobuf.csv","react.csv","redis.csv","redux.csv","requests.csv","retrofit.csv","RxJava.csv","scrapy.csv","scrcpy.csv","socket.io.csv",
                       "spring-boot.csv","spring-framework.csv","terminal.csv","tesseract.csv","thefuck.csv","vue.csv","x64dbg.csv","youtube-dl.csv","zstd.csv")

columns_compare <- list(c("effort_git", "effort_svn"), 
                        c("percentageOfChange_git", "percentageOfChange_svn"),
                        c("accuracy_svn", "accuracy_git"),
                        c("precision_svn", "precision_git"),
                        c("recall_svn", "recall_git"),
                        c("measure_svn", "measure_git"))

# ......::::::: functions :::::::......
getresultwilcox <- function(dataframeprj, column1, column2) {
  wc = wilcox.test(dataframeprj[[column1]], dataframeprj[[column2]], alternative = "greater", paired=TRUE, conf.int=TRUE, conf.level= 0.95, exact=F, correct=F)
  return(c(wc[["p.value"]],  wc[["conf.int"]][1]))
}

getresultptest <- function(dataframeprj, column1, column2) {
  tt = t.test(dataframeprj[[column1]], dataframeprj[[column2]], alternative = "greater", paired=TRUE, conf.int=TRUE, conf.level= 0.95, exact=F, correct=F)
  return(c(tt[["statistic"]][["t"]], tt[["parameter"]][["df"]], tt[["p.value"]]))
}

sanitizesummaryvalue <- function(oldstr, value) {
  res = gsub(pattern = ":", replacement = "", x = value, ignore.case = TRUE)
  res = gsub(pattern = oldstr, replacement = "", x = res, ignore.case = TRUE)
  return(trimws(res))
}

getresultsummary <- function(svn, proj, summary) {
  #10 - colum effort_svn
  #19 - column effort_git
  col = 10
  colname = "svn"
  if (!svn) {
    col = 19
    colname = "git"
  }
  df <- data.frame(project = c(proj),
                   vcs = c(colname),
                   n = c(sanitizesummaryvalue("Length", summary[1,1])),
                   min = c(sanitizesummaryvalue("Min.", summary[1,col])),
                   c1st = c(sanitizesummaryvalue("1st Qu.", summary[2,col])),
                   med = c(sanitizesummaryvalue("Median", summary[3,col])),
                   mean = c(sanitizesummaryvalue("Mean", summary[4,col])),
                   c3rd = c(sanitizesummaryvalue("3rd Qu.", summary[5,col])),
                   max = c(sanitizesummaryvalue("Max.", summary[6,col])))
  return(df)
}

processa_dataframe_proj <- function(p_prj_name, p_df_prj) {
  summary_prj = summary(p_df_prj, mean, na.rm=TRUE)
  df_centralized = getresultsummary(TRUE, p_prj_name, summary_prj)
  df_distributed = getresultsummary(FALSE, p_prj_name, summary_prj)
  df_estat_descr_prj <- rbind(df_centralized, df_distributed) 
  # add St.D column in dataframe
  df_estat_descr_prj['std'] <- c(sd(p_df_prj$effort_svn), sd(p_df_prj$effort_git))

  #cria dataframe ja com linha para projeto atual
  df_tests_prj <- data.frame(project = c(p_prj_name))
  for (column in columns_compare) {
    # os nomes de coluna tem o sufixo _svn ou _git
    # neste ponto removemos o sufixo para pegar 
    # somente o nome do campo
    clns = strsplit(column[1], "_", fixed=T)
    cln = clns[[1]][1]
    # WILCOX
    # monta o nome da nova coluna wilcox_[coluna]_pvalue
    cnw_pvalue = paste("wilcox", cln, "pvalue", sep = "_")
    # monta o nome da nova coluna wilcox_[coluna]_inf
    cnw_inf = paste("wilcox", cln, "inf", sep = "_")
    # executa wilcox e insere resultados em novas colunas no dataframe
    wcr = getresultwilcox(p_df_prj, column[1], column[2])
    df_tests_prj[cnw_pvalue] <- c(wcr[1])
    df_tests_prj[cnw_inf] <- c(wcr[2])
    # PAIRED T-TEST
    # monta o nome da nova coluna t-test_[coluna]_t
    cnt_t = paste("t-test", cln, "t", sep = "_")
    # monta o nome da nova coluna t-test_[coluna]_df
    cnt_df = paste("t-test", cln, "df", sep = "_")
    # monta o nome da nova coluna t-test_[coluna]_pvalue
    cnt_pvalue = paste("t-test", cln, "pvalue", sep = "_")
    # executa t.test e insere resultados em novas colunas no dataframe
    ttr = getresultptest(p_df_prj, column[1], column[2])
    df_tests_prj[cnt_t] <- ttr[1]
    df_tests_prj[cnt_df] <- ttr[2]
    df_tests_prj[cnt_pvalue] <- ttr[3]
  }
  return(list(df_estat_descr_prj, df_tests_prj))
}
# ......::::::: END functions :::::::......


# ......::::::: STAR EXECUTION :::::::......
df_estat_descr = data.frame()
df_tests = data.frame()
df_geral = data.frame()
for (prj_file_name in projects_names) {
  prj_name = gsub(".csv", "", prj_file_name)
  out <- paste0("Process file: ", prj_file_name)  # Some output
  print(out)
  df_prj <- read.csv(file = paste(pathbase, prj_file_name, sep = "/"), header = TRUE, sep = ";")
  df_geral <- rbind(df_geral, df_prj)
  
  df_results_prj <- processa_dataframe_proj(prj_name, df_prj)
  df_estat_descr <- rbind(df_estat_descr, df_results_prj[[1]])
  df_tests <- rbind(df_tests, df_results_prj[[2]])
}

# processa o dataframe geral que contem os dados de todos arquivos acumulados
df_results_geral <- processa_dataframe_proj("Geral", df_geral)
df_estat_descr <- rbind(df_estat_descr, df_results_geral[[1]])
df_tests <- rbind(df_tests, df_results_geral[[2]])

write.csv(df_estat_descr, file = paste(pathbase, "result_estatistica_descritiva.csv", sep = "/"))
write.csv(df_tests, file = paste(pathbase, "result_testes_hipoteses.csv", sep = "/"))

