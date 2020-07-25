#' Baixa tabela de sentencas do tjpr
#'
#' @param id_comarca Pode ser encontrada no dataset comarca
#' @param data_inicial Data inicial no formato dd/mm/yyyy
#' @param data_final  Data final no formato dd/mm/yyyy
#' @param diretorio Diretório onde armazenar os htmls
#'
#' @return html
#' @export
#'
tjpr_baixar_dados_cjpg <- function(id_comarca, data_inicial, data_final, diretorio = "."){



  url <- "https://portal.tjpr.jus.br/pesquisa_sentenca/publico/sentenca.do?actionType=pesquisa"


  purrr::walk(id_comarca,purrr::possibly(~{


    body<- list(
      bundlePesquisa = "pesquisasentenca",
      pesquisaForward = "listaTabeladaSentenca",
      nomeComarca = "",
      idComarca = .x,
      idVara = "-1",
      numeroAutos = "",
      assunto = "",
      numAssunto = "",
      nomeparte = "",
      conteudo = "",
      juiz = "",
      numMatriculaJuiz = "",
      dataInicioFiltro = data_inicial,
      dataFimFiltro = data_final,
      Pesquisar = "Pesquisar",
      estiloPesquisa = "",
      pageSize = "20",
      pageNumber = 1,
      sortColumn = "dataPublicacao",
      sortOrder = "DESC"
    )

    n_pagina <- 1

   di <- stringr::str_replace_all(data_inicial,"/","_")
   df <- stringr::str_replace_all(data_final,"/","_")


repeat{


body$pageNumber <- n_pagina

r <-     httr::POST(url, body = body, encode = "form")

 conteudo <- r %>%
            httr::content() %>%
            xml2::xml_find_first("//div[@class='navLeft']") %>%
            xml2::xml_text()


 if (is.na(conteudo)){
  break
 }

 arquivo <- file.path(diretorio,paste0("id_comarca_",.x,"_pagina_",n_pagina,"_data_inicial_",di,"_datafinal_",df,".html"))

 writeBin(r$content,arquivo)

 n_pagina <- n_pagina+1

 }


  },NULL))
}
