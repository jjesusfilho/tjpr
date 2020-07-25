#' Extrai as urls das sentenças(pdfs) do tjpr
#'
#' @param arquivos Arquivos htmls
#' @param diretorio Informar se arquivos forem NULL
#'
#' @details Esta função itera sobre todos os htmls informados
#'    e busca na página do tjpr cada uma das urls dos pdfs.
#'    Isso implica que ela pode demorar. Sugere-se não buscar
#'    um número grande por vez.
#'
#' @return tibble
#' @export
#'
tjpr_extrair_dados_cjpg <-  function(arquivos = NULL, diretorio = "."){


  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="html$", full.names = TRUE)
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    x <- xml2::read_html(.x)

    ### Os links dos pdfs não vêm na primeira página. Esses links irão permitir acesso
    ### aos os links dos pdfs
    links <- x %>%
      xml2::xml_find_all("//td[@valign='middle']/img") %>%
      xml2::xml_attr("onclick") %>%
      stringr::str_extract("tjpr.url.crypto\\X+Sentenca") %>%
      paste0("https://portal.tjpr.jus.br/pesquisa_sentenca/publico/ajax_concursos.do?",.) %>%
      unique()

    ### Realiza 20 requisições por vez para obter as urls dos pdfs.
    pdfs <- purrr::map_chr(links,purrr::possibly(~{
      httr::GET(.x) %>%
        httr::content() %>%
        xml2::xml_find_first("//a") %>%
        xml2::xml_attr("href") %>%
        paste0("https://portal.tjpr.jus.br",.)



    },NA_character_))

    data <- x %>%
      xml2::xml_find_all("//td[@align='center']//a") %>%
      xml2::xml_text(trim=T) %>%
      as.Date()

    assunto <- x %>%
      xml2::xml_find_all("//table[2]//td[@valign='top'][1]") %>%
      xml2::xml_text()

    processo <- x %>%
      xml2::xml_find_all("//table[2]//td[@valign='top'][2]") %>%
      xml2::xml_text() %>%
      stringr::str_remove_all("\\D")


    juiz <- x %>%
      xml2::xml_find_all("//table[2]//td[@valign='top'][3]") %>%
      xml2::xml_text()

    tibble::tibble(processo = processo, assunto = assunto, juiz = juiz, pdf = pdfs)


  },NULL))

}
