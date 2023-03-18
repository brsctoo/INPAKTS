
# Cookie warning ------
cookie_box <- div(
  class = "alert alert-info", style = "margin-bottom: 0",
  "A página foi atualizada no dia 18 de junho de 2022 às 13h00min, acesse o site do programa de mestrado em",
  tags$a(
    href = "http://pbe.uem.br/",
    "PBE-UEM"
  ), ".",
  HTML('<a href="#" class="close" data-dismiss="alert" aria-label="close">&check;</a>'),
  img(src="www/imagem1.ico", align = "center",height = "35px", width = "35px")

)
