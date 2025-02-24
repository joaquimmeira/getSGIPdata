# getSGIPdata

## Descrição

O **getSGIPdata** é um pacote R que facilita a obtenção de informações sobre partidos políticos no Brasil por meio da API do SGIP (Sistema de Gestão de Informações Partidárias) do TSE.

## Instalação

```r
# Instalar via remotes (caso ainda não tenha instalado o remotes)
install.packages("remotes")

# Instalar o pacote diretamente do GitHub
remotes::install_github("adelino-dev/getSGIPdata")
```

## Uso

Antes de utilizar o pacote, carregue-o no R:

```r
library(getSGIPdata)
```

### 1. Obter informações sobre partidos

A função `get_parties_info()` permite buscar dados sobre partidos políticos em um ou mais estados do Brasil.

#### Exemplo de uso:

```r
parties_info <- get_parties_info(
  states = c("SP", "RJ"),
  ivigencia = "01/01/2020",
  fvigencia = "31/12/2023"
)

head(parties_info)
```

**Parâmetros:**

- `states`: Vetor de caracteres com siglas dos estados (ex.: "SP", "RJ"). Se `NULL`, busca dados de todos os estados.
- `ivigencia`: Data inicial da pesquisa no formato "dd/mm/yyyy". Padrão: data atual.
- `fvigencia`: Data final da pesquisa no formato "dd/mm/yyyy". Padrão: data atual.

### 2. Obter membros de um partido

A função `get_party_members()` retorna os membros de um determinado partido.

#### Exemplo de uso:

```r
members <- get_party_members("447881")
print(members)
```

**Parâmetro:**

- `id_orgao_partidario`: ID numérico de um partido específico. Se `NULL`, retorna informações de todos os partidos.

## Funções internas

O pacote inclui funções internas para validação e obtenção de dados:

- `.validate_date()`: Valida e formata datas, garantindo que estejam no formato "dd/mm/yyyy" e dentro dos limites permitidos.
- `.validate_states()`: Verifica se as siglas dos estados informados são válidas. Caso contrário, gera um erro. Se `NULL` for fornecido, retorna todas as siglas válidas com um aviso.
- `.validate_id_orgao_partidario()`: Valida o ID de uma organização partidária, verificando se é válido com base em uma lista predefinida.
- `.fetch_parties_info()`: Obtém informações de partidos políticos da API do TSE, com base nos estados e nas datas fornecidas.
- `.fetch_party_members()`: Obtém os membros de um partido político a partir da API do TSE, usando o ID do partido.
- `.request_party_data()`: Envia uma solicitação à API do TSE para obter dados sobre um partido específico, dado seu ID.
- `.parse_members()`: Converte a resposta da API sobre membros de um partido em um tibble estruturado, incluindo detalhes como sigla, UF e município.


## Dependências

O pacote depende das seguintes bibliotecas:

- `lubridate`
- `magrittr`
- `stringr`
- `purrr`
- `httr2`
- `glue`
- `tibble`
- `dplyr`
- `tidyr`


## Licença

Este projeto está licenciado sob a MIT License - veja o arquivo [LICENSE](LICENSE) para mais detalhes.

---

**Autores:**

- [adelino-dev](https://github.com/adelino-dev)
- [SamuelBRodrigues](https://github.com/SamuelBRodrigues)
- [BaruqueRodrigues](https://github.com/BaruqueRodrigues)

