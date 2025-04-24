# ⚡️ Simulador de EMG para Identificação de Padrões na Esclerose Lateral Amiotrófica (ELA)

## 🌐 [🔗 Acesse a Aplicação Web](https://bit.ly/24-04-25)

📚 **Disciplina:** PGEB39 – Processamento de Sinais Biomédicos  
🗓️ **Período:** 1º Semestre de 2025  
🧑‍🏫 **Orientador:** Prof. Drº João Batista Destro Filho  

💼 **Apresentação:** Seminário em 25/04/2025  
🚀 **Desenvolvido por alunos do Programa de Pós-graduação em Engenharia Biomédica (PPGEB‑UFU)**  

👥 **Integrantes do Grupo 2:** Fernando | Francisco | João | Ysabel  

---

## 🎯 Objetivo do Projeto

Esta aplicação web (WebApp) em Shiny tem como propósito capacitar profissionais da saúde a identificar e reconhecer padrões característicos do sinal de Eletromiografia (EMG) relacionados à Esclerose Lateral Amiotrófica (ELA). É uma ferramenta educativa que simula dados EMG com diferentes padrões patológicos para treinamento diagnóstico inicial e compreensão da aplicação da Transformada de Fourier.

---

## 🚧 Importante: Aviso de Desenvolvimento 🚧

- 🗓️ Este projeto foi desenvolvido em um prazo extremamente curto para a apresentação no seminário da disciplina PGEB39, que ocorrerá no dia **25/04/2025**.
- 🚨 Por conta disso, a aplicação pode conter erros críticos, bugs e inconsistências que ainda não foram revisados por profissionais da área médica.
- ⚠️ A aplicação NÃO foi validada clinicamente e serve apenas como demonstração acadêmica do uso da Transformada de Fourier com dados simulados.

---

## 💻 Tecnologias e Ferramentas Utilizadas

- **R (Sim, orgulhosamente desenvolvido totalmente em R, que não é uma linguagem morta ainda!)** 🚀✨
- **Shiny:** framework R para desenvolvimento de aplicações web interativas.
- **Bibliotecas adicionais:** `shinydashboard`, `plotly`, `dplyr`, `signal`, `DT`, `shinyjs`, `shinycssloaders`
- **Transformada de Fourier (FFT):** Implementada para análise no domínio da frequência dos sinais EMG simulados.

---

## 📈 Sobre o Código

Este repositório inclui:

- **Simulação de EMG:** Diversos padrões típicos da ELA podem ser configurados, como fasciculações, fibrilações, ondas agudas positivas, potenciais polifásicos, descargas repetitivas complexas (DRC) e recrutamento reduzido.
- **Análise de Fourier:** Permite análise dos sinais simulados através da Transformada de Fourier, destacando componentes de frequência importantes.
- **Exportação de Dados:** Dados simulados podem ser exportados em CSV.

O código é totalmente aberto e livre para ser utilizado, modificado e aprimorado.

---

## ⚙️ Funcionalidades

### Simulação de EMG
- Configuração detalhada de cada padrão (amplitude, duração, intervalo)
- Variabilidade natural através de desvios padrão configuráveis
- Visualização em tempo real com marcadores
- Exportação de dados simulados em CSV

### Análise de Fourier
- Importação de dados EMG (simulados ou reais)
- Pré-processamento com janelamento (Hanning, Hamming, Blackman)
- Cálculo de métricas espectrais:
  - Frequência média ponderada
  - Frequência mediana
  - Potência total
  - Detecção de picos significativos
  

---


## 🔍 Análise de Frequência com FFT (Transformada de Fourier)

A Transformada Rápida de Fourier (FFT - *Fast Fourier Transform*) permite converter o sinal EMG do domínio do tempo (gráfico azul) para o domínio da frequência (gráfico vermelho), facilitando a identificação de padrões rítmicos, frequências dominantes e alterações características de doenças neuromusculares, como a Esclerose Lateral Amiotrófica (ELA).

### 🧠 O que é a FFT?
A FFT é um algoritmo que decompõe um sinal em suas **componentes senoidais básicas**, mostrando **quais frequências estão presentes** e com qual intensidade. Isso permite examinar padrões que não são visíveis diretamente no tempo.

🔗 Saiba mais: [Fast Fourier Transform (NTi Audio)](https://www.nti-audio.com/en/support/know-how/fast-fourier-transform-fft)

---

### ⚙️ Pré-processamento aplicado no simulador

Antes de aplicar a FFT ao sinal EMG, nosso sistema realiza duas etapas fundamentais para garantir maior precisão:

1. **Remoção do componente DC (offset):**
   - Subtrai-se a média do sinal, eliminando deslocamentos verticais que distorcem o espectro.
   - Implementado via: `signal <- signal - mean(signal)`

2. **Aplicação de janelamento (windowing):**
   - Utilizamos funções de janela (ex: Hanning, Hamming, Blackman) para suavizar as bordas do sinal.
   - Isso reduz artefatos espectrais (leakage) e melhora a resolução das frequências.

🔬 Referência prática: [BIOPAC – EMG Frequency Signal Analysis](https://www.biopac.com/application-note/emg-electromyogram-frequency-signal-analysis/emg-frequency-signal-analysis/)

---

### 📊 Como interpretar os gráficos

- **Gráfico Azul (Sinal Original):** Mostra o EMG no tempo, com amplitudes em milivolts (mV).
- **Gráfico Vermelho (FFT):** Exibe o espectro de frequência do sinal (em Hz), onde:
  - **Picos agudos** indicam frequências dominantes.
  - **Frequências mais baixas** podem refletir recrutamento de unidades motoras lentas (comum em ELA).
  - **Espectros achatados ou com menos picos** podem indicar **sincronização anormal** de unidades motoras.

---

### 📐 Métricas calculadas automaticamente

| Métrica                       | Interpretação na ELA (Hipótese)                                                                 |
|------------------------------|--------------------------------------------------------------------------------------------------|
| **Frequência Média Ponderada** | Pode ser reduzida na ELA, refletindo menor atividade de fibras rápidas.                         |
| **Frequência Mediana**         | Divide o espectro ao meio. Valores mais baixos sugerem menor complexidade de recrutamento.     |
| **Potência Total do Sinal**   | Reduzida na ELA, indicando perda de unidades motoras e menor ativação muscular.                 |
| **Picos Significativos**      | Menor número de picos pode refletir recrutamento reduzido ou dessincronizado.                   |

📘 Leitura complementar:  
- [Median and Mean Frequency in EMG – BIOPAC](https://www.biopac.com/application/emg-electromyography/advanced-feature/median-and-mean-frequency-analysis/)

---

### ℹ️ Notas Técnicas

- **Simetria do espectro:** Como o sinal EMG é real, sua FFT é simétrica. Só mostramos a metade positiva.
- **Resolução espectral (Δf):** Quanto mais longo o sinal no tempo, melhor a separação entre frequências.  
  Fórmula: `Δf = Fs / N`, onde `Fs` é a frequência de amostragem e `N` o número de amostras.
- **Magnitude:** A análise usa `abs(FFT)` (amplitude), mas também é comum usar `abs(FFT)^2` (potência) dependendo do objetivo.

---

## 📊 Explicação Consolidada dos Resultados da Análise de Fourier (FFT)

Esta seção esclarece os resultados obtidos após aplicar a Transformada de Fourier Rápida (**FFT - Fast Fourier Transform**) ao sinal EMG simulado. A análise FFT transforma o sinal do domínio do tempo (**gráfico azul original**) para o domínio da frequência (**gráfico vermelho da FFT**), permitindo examinar características que não são claramente visíveis na visualização temporal.

### 🔹 O que é a Transformada de Fourier (FFT)?

A **Transformada de Fourier** é uma técnica matemática que decompõe um sinal complexo no tempo (gráfico azul) em componentes individuais de frequência (gráfico vermelho). A versão utilizada é a **FFT**, uma implementação rápida e eficiente que facilita a análise de grandes sinais.

### 🔹 Frequência e Magnitude

- **Frequência** representa quantas oscilações completas (ciclos completos: subida e descida da onda) ocorrem em um segundo (medido em Hertz - Hz).
- **Magnitude** é a intensidade ou contribuição de cada frequência específica ao sinal, observada no gráfico vermelho após a FFT.

---

## 🧮 Como foi calculado o FFT no Simulador

Na aplicação Shiny, **a Transformada Rápida de Fourier (FFT)** foi implementada usando a função `fft()` da **base do R** (`package:stats`), sem necessidade de bibliotecas externas adicionais.

### 📦 Biblioteca utilizada:
- **`fft()`** do próprio R base (não precisa instalar nada adicional)

### 🔗 Referência oficial da função `fft()` do R

> 📌 **Função utilizada para calcular a Transformada de Fourier (base do R)**

- **Documentação oficial do R (base):**  
  📄 [`fft()` - R Documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html)  
  https://stat.ethz.ch/R-manual/R-devel/library/stats/html/fft.html

---

---

### ⚙️ Etapas do Cálculo da FFT no Código:

A função principal usada é `calculate_fft()` e segue este fluxo:

1. **Remoção do Componente DC (offset)**:
   Elimina o valor médio do sinal para evitar distorções no espectro:
   ```r
   signal <- signal - mean(signal)
   ```

2. **Aplicação de Janelamento** (windowing):
   Aplica uma função de janela (ex: Hanning, Hamming, Blackman) para suavizar bordas:
   ```r
   windowed_signal <- apply_window(signal, window_type)
   ```

3. **Cálculo da FFT**:
   A Transformada de Fourier propriamente dita:
   ```r
   fft_result <- fft(windowed_signal)
   ```

4. **Cálculo da Magnitude do Espectro**:
   Considerando apenas a metade positiva do espectro (sinal real):
   ```r
   magnitude <- abs(fft_result[1:(n/2+1)]) / n * 2
   magnitude[1] <- magnitude[1] / 2  # Corrige a magnitude DC
   ```

5. **Geração do Vetor de Frequências**:
   Relaciona cada ponto do espectro à sua frequência correspondente:
   ```r
   freq <- seq(0, sampling_rate/2, length.out = length(magnitude))
   ```

---

### 📈 Saída da Função `calculate_fft()`

A função retorna uma **lista com os seguintes elementos**:
- `freq`: vetor com as frequências em Hertz (Hz)
- `magnitude`: vetor da magnitude espectral associada a cada frequência

Esses vetores são usados para:
- Gerar o **gráfico vermelho** do espectro de frequência
- Calcular métricas como **frequência média ponderada**, **frequência mediana**, **potência total** e **número de picos significativos**

--- 


### 📌 Resultados e Interpretações Específicas:

#### 1. **Frequência Média Ponderada** (Gráfico vermelho)

- **O que é:** Média ponderada das frequências do sinal, dando mais importância às frequências com magnitudes maiores.
- **Cálculo:**
  ```R
  mean_freq <- sum(freq * magnitude) / sum(magnitude)
  ```
- **Interpretação (Hipótese em ELA):** Frequências médias mais baixas indicam predominância de fibras musculares lentas ou perda de fibras rápidas, comum em ELA avançada.
- **Valor Exemplo:** `292.86 Hz` (SUPOSTAMENTE alto, sugere atividade muscular vigorosa ou anormal).

#### 2. **Frequência Mediana** (Gráfico vermelho)

- **O que é:** Frequência que divide o espectro em duas partes iguais de energia.
- **Cálculo:**
  ```R
  cumulative_power <- cumsum(magnitude)
  median_freq_idx <- which(cumulative_power >= cumulative_power[length(cumulative_power)]/2)[1]
  median_freq <- freq[median_freq_idx]
  ```
- **Interpretação (Hipótese em ELA):** Valores reduzidos refletem recrutamento motor limitado e diversidade reduzida das fibras musculares ativas.
- **Valor Exemplo:** `122.80 Hz` (SUPOSTAMENTE dentro da faixa normal, carece de comparação com controle).

#### 3. **Potência Total do Sinal** (Gráfico vermelho)

- **O que é:** Energia total do sinal, soma dos quadrados das magnitudes.
- **Cálculo:**
  ```R
  total_power <- sum(magnitude^2)
  ```
- **Interpretação (Hipótese em ELA):** Potência reduzida indica menor ativação muscular, consistente com a perda de unidades motoras na ELA.
- **Valor Exemplo:** `0.03764 mV²` (atividade muscular SUPOSTAMENTE moderada-baixa, carece de comparação com controle).

#### 4. **Número de Picos Significativos** (Gráfico vermelho)

- **O que é:** Frequências dominantes detectadas no espectro.
- **Cálculo:**
  ```R
  peaks <- which(diff(sign(diff(magnitude))) == -2) + 1
  significant_peaks <- peaks[magnitude[peaks] > 0.005]
  ```
- **Interpretação (Hipótese em ELA):** Pacientes com ELA geralmente têm menos picos ou picos dispersos devido à dessincronização motora.
- **Valor Exemplo:** `10 picos` (SUPOSTAMENTE normal, carece de comparação com controle).

---

### 🔍 Exemplos dos Picos Detectados (Gráfico vermelho)

Os picos ordenados por magnitude mostram as frequências mais importantes:

- Pico 1: `6.70 Hz` (Magnitude: `0.02966`)
- Pico 2: `7.30 Hz` (Magnitude: `0.02510`)

Frequências baixas sugerem atividade muscular reduzida ou dessincronizada, característica em condições como ELA.

---

### ⚠️ Comparação Clínica Necessária

Resultados devem ser comparados com indivíduos saudáveis ou outros pacientes com ELA para uma interpretação correta. Valores isolados têm significado limitado sem contexto clínico.

---

### 📘 Resumo e Utilidade da FFT

A aplicação da **FFT** (gráfico vermelho) revela informações importantes sobre a atividade muscular não evidentes no domínio temporal (gráfico azul). As frequências, magnitudes e potência extraídas ajudam na identificação e caracterização de padrões relacionados à ELA, sendo valiosas no diagnóstico e acompanhamento clínico.

> **Nota:** As interpretações clínicas são educacionais e devem ser validadas por estudos clínicos reais.


  

## 🚀 Futuro do Projeto

Este simulador pode ser o ponto de partida para uma aplicação mais robusta, capaz de utilizar dados reais para auxiliar profissionais de saúde no diagnóstico e acompanhamento da ELA. Com orientação e validação por especialistas, o potencial de contribuição clínica dessa ferramenta pode ser bastante significativo.

Estamos totalmente disponíveis e interessados em ajudar quem desejar continuar o desenvolvimento desta aplicação.

- 📧 **Contato:** franciscodnlneto@gmail.com  
- 📱 **WhatsApp:** +55 (31) 9 9900-5794

---

## ⚖️ Licença e Autoria

Este projeto é uma criação colaborativa do Grupo 2 (Fernando, Francisco, João e Ysabel). Todos os integrantes possuem coautoria e compartilham propriedade intelectual sobre o código desenvolvido.

---
## ⚠️ Limitações

- Os dados são 100% simulados e não substituem dados reais de EMG
- As interpretações clínicas são hipotéticas e educacionais
- O simulador não realiza diagnósticos
- Não validado clinicamente para uso diagnóstico real
- Interface totalmente em português (sem suporte multilíngue)
---


## 💻 Requisitos de Sistema

- R (versão 4.0.0 ou superior)
- RStudio (opcional, mas recomendado)
- Conexão com internet para instalar pacotes

## 🚀 Como Executar Localmente

1. Clone o repositório:
```bash
git clone https://github.com/franciscodnlneto/ppgeb39.git

---


🔗 **Acesse o repositório:** [GitHub - franciscodnlneto/ppgeb39](https://github.com/franciscodnlneto/ppgeb39)

---

Feito com 💙 e ☕️ pelos alunos da disciplina PGEB39 do PPGEB-UFU.
