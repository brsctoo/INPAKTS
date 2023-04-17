#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #Criando arquivo para armazenar os caches
  shinyOptions(
    cache = cachem::cache_disk(
    # dir = NULL (default) será usado e criado um diretório temporário.
    dir = "./cache",
    destroy_on_finalize =T))#,
    #Tempo, em segundos, que os arquivos em cache ficarão armazenados
    #max_age = 60))

  ## Configurando a página inicial -------
 #w <- waiter::Waiter$new(
    # html = h1(
    #   p(),
    #   h1(strong("Protótipo:"), align = "center"),
    #   linebreaks(0.001),
    #   h3(strong(HTML("INPAKTS: Plataforma de gestão e monitoramento do impacto <br/>
    #                    de intervenções e eventos externos em Séries Temporais    <br/>
    #                    na saúde materno-infantil, da mulher e da criança")), align = "center"),
    #   # linebreaks(0.001),
    #   # h3(strong("de intervenções e eventos externos em Séries Temporais"), align = "center"),
    #   # linebreaks(0.001),
    #   # h3(strong("na saúde materno-infantil, da mulher e da criança"), align = "center"),
    #   p(),
    #   linebreaks(5),
    #   h3(strong(em("Sem dados, você possui apenas uma opinião")), align = "right")),
    #Url da imagem exibida na página inicial
    #img(src = "pagina_inicial.png")
    #image ="www/pagina_inicial.png"
    #Url da imagem exibida na página inicial
    #image = "https://images.pexels.com/photos/2664417/pexels-photo-2664417.jpeg?auto=compress&cs=tinysrgb&w=1260&h=750&dpr=2"
  #)$show()

  ### Fixando o tempo que a tela inicial aparecerá -------
  #Sys.sleep(15)
  #w$hide()


  # Your application server logic

  opcoes_usuario <- reactiveValues()
  mod_inicio_server("inicio_ui_1",opcoes_usuario)

 # mod_testando_server("testando_1",opcoes_usuario)
  mod_analise_geografica_server("analise_geografica_1",opcoes_usuario)
  mod_intervencao_sinasc_server("intervencao_sinasc_1",opcoes_usuario)
  mod_intervencao_sim_neonatal_server("intervencao_sim_neonatal_1",opcoes_usuario)
  mod_intervencao_sim_materno_server("intervencao_sim_materno_1",opcoes_usuario)
  mod_analise_geografica_server("analise_geografica_1", opcoes_usuario)
  mod_intervencao_sinasc_server("intervencao_sinasc_1", opcoes_usuario)
  mod_intervencao_sim_neonatal_server("intervencao_sim_neonatal_1", opcoes_usuario)
  mod_intervencao_sim_materno_server("intervencao_sim_materno_1", opcoes_usuario)
  mod_sobre_server("sobre_ui_1")
  mod_descritiva_sinasc_server("descritiva_sinasc_1",opcoes_usuario)
  mod_descritiva_sim_neonatal_server("descritiva_sim_neonatal_1", opcoes_usuario)
  mod_descritiva_sim_materna_server("descritiva_sim_materna_1", opcoes_usuario)
  mod_descritiva_sif_gestante_server("descritiva_sif_gestante_1", opcoes_usuario)
  mod_descritiva_sif_congenita_server("descritiva_sif_congenita_1", opcoes_usuario)
  mod_intervencao_sif_gestante_server("intervencao_sif_gestante_1",opcoes_usuario)
  mod_intervencao_sif_congenita_server("intervencao_sif_congenita_1",opcoes_usuario)
}
