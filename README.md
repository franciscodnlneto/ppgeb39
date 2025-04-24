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

## 📊 Explicação Consolidada dos Resultados da Análise de Fourier (FFT)

Esta seção esclarece os resultados obtidos após aplicar a Transformada de Fourier Rápida (**FFT - Fast Fourier Transform**) ao sinal EMG simulado. A análise FFT transforma o sinal do domínio do tempo (**gráfico azul original**) para o domínio da frequência (**gráfico vermelho da FFT**), permitindo examinar características que não são claramente visíveis na visualização temporal.

### 🔹 O que é a Transformada de Fourier (FFT)?

A **Transformada de Fourier** é uma técnica matemática que decompõe um sinal complexo no tempo (gráfico azul) em componentes individuais de frequência (gráfico vermelho). A versão utilizada é a **FFT**, uma implementação rápida e eficiente que facilita a análise de grandes sinais.

### 📚 Base Científica e Referências

**Quer entender melhor a ciência por trás?** Vários estudos científicos estabelecem a base para nossa análise:

- 🔬 **O clássico artigo de De Luca (1997)**[^1] é a referência fundamental para entender como usar a FFT em EMG. Ele explica em detalhes como interpretar as frequências médias e medianas nos sinais musculares. Este artigo continua sendo um dos mais citados na área, justamente por estabelecer as bases teóricas que usamos hoje.

- 📊 **Kallenberg & Hermens (2006)**[^2] realizaram pesquisas importantes sobre como a frequência média muda quando o músculo está cansado. Embora o estudo não seja específico para ELA, os conceitos se aplicam perfeitamente porque na ELA também ocorre alteração das unidades motoras.

- 🔍 **Os detalhes técnicos de como o EMG é transformado** são muito bem explicados por Barkhaus & Nandedkar (1994)[^3]. Este artigo é especialmente útil para entender como diferentes componentes afetam o espectro de frequências.

- 🧠 **Para aplicações específicas na ELA**, o trabalho recente de Bashford et al. (2020)[^4] é revolucionário! Eles usaram análise de frequência para caracterizar as alterações musculares nos pacientes com ELA. Este estudo mostra como nossa abordagem está alinhada com as pesquisas mais recentes na área.

### 🔹 Frequência e Magnitude

- **Frequência** representa quantas oscilações completas (ciclos completos: subida e descida da onda) ocorrem em um segundo (medido em Hertz - Hz).
- **Magnitude** é a intensidade ou contribuição de cada frequência específica ao sinal, observada no gráfico vermelho após a FFT.

---

### 📌 Resultados e Interpretações Específicas:

#### 1. **Frequência Média Ponderada** (Gráfico vermelho)
- **O que é:** Média ponderada das frequências do sinal, dando mais importância às frequências com magnitudes maiores.
- **Cálculo:**
  ```R
  mean_freq <- sum(freq * magnitude) / sum(magnitude)
  ```
- **Interpretação (Hipótese em ELA):** Frequências médias mais baixas indicam predominância de fibras musculares lentas ou perda de fibras rápidas, comum em ELA avançada.
- **Base científica:** De Luca (1997)[^1] e Kallenberg & Hermens (2006)[^2] mostram que a redução da frequência média ocorre em músculos fatigados ou com perda de unidades motoras, como na ELA.

#### 2. **Frequência Mediana** (Gráfico vermelho)
- **O que é:** Frequência que divide o espectro em duas partes iguais de energia.
- **Cálculo:**
  ```R
  cumulative_power <- cumsum(magnitude)
  median_freq_idx <- which(cumulative_power >= cumulative_power[length(cumulative_power)]/2)[1]
  median_freq <- freq[median_freq_idx]
  ```
- **Interpretação (Hipótese em ELA):** Valores reduzidos refletem recrutamento motor limitado e diversidade reduzida das fibras musculares ativas.
- **Base científica:** A frequência mediana também tende a diminuir com a perda da diversidade de unidades motoras, como discutido por Barkhaus & Nandedkar (1994)[^3].

#### 3. **Potência Total do Sinal** (Gráfico vermelho)
- **O que é:** Energia total do sinal, soma dos quadrados das magnitudes.
- **Cálculo:**
  ```R
  total_power <- sum(magnitude^2)
  ```
- **Interpretação (Hipótese em ELA):** Potência reduzida indica menor ativação muscular, consistente com a perda de unidades motoras na ELA.
- **Base científica:** A energia total do sinal está relacionada ao número e intensidade de disparos das unidades motoras. Estudos em ELA (Bashford et al., 2020[^4]) relatam redução da potência em estágios avançados.

#### 4. **Número de Picos Significativos** (Gráfico vermelho)
- **O que é:** Frequências dominantes detectadas no espectro.
- **Cálculo:**
  ```R
  peaks <- which(diff(sign(diff(magnitude))) == -2) + 1
  significant_peaks <- peaks[magnitude[peaks] > 0.005]
  ```
- **Interpretação (Hipótese em ELA):** Pacientes com ELA geralmente têm menos picos ou picos dispersos devido à dessincronização motora.
- **Base científica:** Menor número de picos ou picos mais largos no espectro podem indicar dessincronização dos disparos, como observado em padrões de EMG na ELA[^4].

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

---

## 🚀 Futuro do Projeto

Este simulador pode ser o ponto de partida para uma aplicação mais robusta, capaz de utilizar dados reais para auxiliar profissionais de saúde no diagnóstico e acompanhamento da ELA. Com orientação e validação por especialistas, o potencial de contribuição clínica dessa ferramenta pode ser bastante significativo.

Estamos totalmente disponíveis e interessados em ajudar quem desejar continuar o desenvolvimento desta aplicação.

- 📧 **Contato:** franciscodnlneto@gmail.com  
- 📱 **WhatsApp:** +55 (31) 9 9900-5794

---

## ⚖️ Licença e Autoria

Este projeto é uma criação colaborativa do Grupo 2 (Fernando, Francisco, João e Ysabel). Todos os integrantes possuem coautoria e compartilham propriedade intelectual sobre o código desenvolvido.

---

## 📚 Referências Científicas

[^1]: De Luca CJ (1997). The use of surface electromyography in biomechanics. *Journal of Applied Biomechanics*, 13(2), 135–163. [https://doi.org/10.1123/jab.13.2.135](https://doi.org/10.1123/jab.13.2.135)  
[^2]: Kallenberg LA, Hermens HJ (2006). Behavior of a surface EMG based measure for motor control: Motor unit action potential rate in relation to force and muscle fatigue. *J Electromyogr Kinesiol*, 16(6), 619–626. [https://doi.org/10.1016/j.jelekin.2006.02.005](https://doi.org/10.1016/j.jelekin.2006.02.005)  
[^3]: Barkhaus PE, Nandedkar SD (1994). Recording characteristics of the surface EMG electrode. *Muscle & Nerve*, 17(7), 815–826. [https://doi.org/10.1002/mus.880170714](https://doi.org/10.1002/mus.880170714)  
[^4]: Bashford J, Wickham L, Menon P, et al. (2020). Network analysis of muscle activity in ALS. *Clinical Neurophysiology*, 131(5), 1116–1125. [https://doi.org/10.1016/j.clinph.2019.12.410](https://doi.org/10.1016/j.clinph.2019.12.410)

---

🔗 **Acesse o repositório:** [GitHub - franciscodnlneto/ppgeb39](https://github.com/franciscodnlneto/ppgeb39)

---

Feito com 💙 e ☕️ pelos alunos da disciplina PGEB39 do PPGEB-UFU.