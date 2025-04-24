
  # -----------------------------------------------------------------------------
#  Simulador de EMG – Identificação de Padrões em Esclerose Lateral Amiotrófica
#  Grupo 2 – Fernando • Francisco • João • Ysabel
#  PGEB39 – Processamento de Sinais Biomédicos • 1º Sem/2025
#  Prof. Dr. João Batista Destro Filho  •  Tarefas 4 & 5 – Seminários 2 (25 / 04 / 2025)
# -----------------------------------------------------------------------------

# --------------------------- ATUALIZAR VERSÃO DO SERVIDOR ---------------------------

library(rsconnect)


# CREDENCIAIS
#rsconnect::setAccountInfo(
#  name = 'prototipo', 
#  token = '5BBF47812BEF1588858ED1918C68D737', 
#  secret = 'qnO6q2j3PBrnJ2uy7BOEZbrM2CD0acCOsO7eqfrm'
#)

# Navegue até o diretório do seu app
#setwd("C:/Users/francisco.negrao/Desktop/PPGEB39G2")

#rsconnect::deployApp(
#  appName = "PPGEB39G2-EMG-Simulator",
#  appTitle = "Simulador EMG - Padrões de ELA",
#  forceUpdate = TRUE
#)

# --------------------------- INSTALAÇÃO DE PACOTES ---------------------------
if (!require("shiny"))            install.packages("shiny")
if (!require("shinyjs"))          install.packages("shinyjs")
if (!require("shinydashboard"))   install.packages("shinydashboard")
if (!require("plotly"))           install.packages("plotly")
if (!require("dplyr"))            install.packages("dplyr")
if (!require("signal"))           install.packages("signal")
if (!require("DT"))               install.packages("DT")
if (!require("shinycssloaders"))  install.packages("shinycssloaders")

# --------------------------- CARREGAR BIBLIOTECAS ---------------------------
library(rsconnect)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(plotly)
library(dplyr)
library(signal)
library(DT)
library(shinycssloaders)

# --------------------------- FUNÇÕES AUXILIARES -----------------------------
# Função para gerar sinal EMG com desvios padrão
generate_emg_signal <- function(total_time, sampling_rate, baseline_noise = 0.1, 
                                normal_emg = list(),
                                fasciculations = list(), fibrillations = list(), 
                                complex_repetitive_discharges = list(), 
                                positive_sharp_waves = list(),
                                abnormal_motor_units = list(),
                                reduced_recruitment = list()) {
  # Número total de amostras
  n_samples <- total_time * sampling_rate
  
  # Gerar vetor de tempo
  time <- seq(0, total_time, length.out = n_samples)
  
  # Inicializar sinal com ruído de base
  signal <- rnorm(n_samples, mean = 0, sd = baseline_noise)
  
  # Adicionar EMG normal se configurado
  if (length(normal_emg) > 0 && normal_emg$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(normal_emg$interval_sd) && normal_emg$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- normal_emg$start_time
      normal_times <- c()
      
      while (current_time < total_time) {
        normal_times <- c(normal_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = normal_emg$interval, sd = normal_emg$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      normal_times <- seq(normal_emg$start_time, total_time, by = normal_emg$interval)
    }
    
    for (i in normal_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(normal_emg$amplitude_sd) && normal_emg$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = normal_emg$amplitude, sd = normal_emg$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.1, amplitude_variation)
        } else {
          amplitude_variation <- normal_emg$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(normal_emg$duration_sd) && normal_emg$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = normal_emg$duration, sd = normal_emg$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.01, duration_variation)
        } else {
          duration_variation <- normal_emg$duration
        }
        
        # Modelar uma contração normal como um potencial trifásico regular
        normal_duration <- duration_variation * sampling_rate
        normal_window <- idx:(min(idx + normal_duration, n_samples))
        
        # Criar forma da contração normal (potencial trifásico mais suave)
        normal_wave <- c(
          seq(0, amplitude_variation * 0.5, length.out = normal_duration/5),
          seq(amplitude_variation * 0.5, amplitude_variation, length.out = normal_duration/5),
          seq(amplitude_variation, -amplitude_variation * 0.5, length.out = 2*normal_duration/5),
          seq(-amplitude_variation * 0.5, 0, length.out = normal_duration/5)
        )
        
        # Garantir que o padrão de contração normal se encaixe na janela
        normal_pattern <- normal_wave[1:min(length(normal_wave), length(normal_window))]
        signal[normal_window[1:length(normal_pattern)]] <- 
          signal[normal_window[1:length(normal_pattern)]] + normal_pattern
      }
    }
  }
  
  # Adicionar fasciculações se configuradas
  if (length(fasciculations) > 0 && fasciculations$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(fasciculations$interval_sd) && fasciculations$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- fasciculations$start_time
      fascic_times <- c()
      
      while (current_time < total_time) {
        fascic_times <- c(fascic_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = fasciculations$interval, sd = fasciculations$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      fascic_times <- seq(fasciculations$start_time, total_time, by = fasciculations$interval)
    }
    
    for (i in fascic_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(fasciculations$amplitude_sd) && fasciculations$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = fasciculations$amplitude, sd = fasciculations$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.1, amplitude_variation)
        } else {
          amplitude_variation <- fasciculations$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(fasciculations$duration_sd) && fasciculations$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = fasciculations$duration, sd = fasciculations$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.01, duration_variation)
        } else {
          duration_variation <- fasciculations$duration
        }
        
        # Modelar uma fasciculação como um breve burst de alta amplitude
        fascic_duration <- duration_variation * sampling_rate
        fascic_window <- idx:(min(idx + fascic_duration, n_samples))
        
        # Criar forma da fasciculação (pulso assimétrico com componente trifásico)
        fascic_wave <- c(
          seq(0, amplitude_variation, length.out = fascic_duration/6),
          seq(amplitude_variation, -amplitude_variation*0.7, length.out = fascic_duration/3),
          seq(-amplitude_variation*0.7, amplitude_variation*0.4, length.out = fascic_duration/3),
          seq(amplitude_variation*0.4, 0, length.out = fascic_duration/6)
        )
        
        # Garantir que o padrão de fasciculação se encaixe na janela
        fascic_pattern <- fascic_wave[1:min(length(fascic_wave), length(fascic_window))]
        signal[fascic_window[1:length(fascic_pattern)]] <- 
          signal[fascic_window[1:length(fascic_pattern)]] + fascic_pattern
      }
    }
  }
  
  # Adicionar fibrilações se configuradas
  if (length(fibrillations) > 0 && fibrillations$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(fibrillations$interval_sd) && fibrillations$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- fibrillations$start_time
      fib_times <- c()
      
      while (current_time < total_time) {
        fib_times <- c(fib_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = fibrillations$interval, sd = fibrillations$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      fib_times <- seq(fibrillations$start_time, total_time, by = fibrillations$interval)
    }
    
    for (i in fib_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(fibrillations$amplitude_sd) && fibrillations$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = fibrillations$amplitude, sd = fibrillations$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.05, amplitude_variation)
        } else {
          amplitude_variation <- fibrillations$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(fibrillations$duration_sd) && fibrillations$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = fibrillations$duration, sd = fibrillations$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.005, duration_variation)
        } else {
          duration_variation <- fibrillations$duration
        }
        
        # Modelar uma fibrilação como uma breve onda positiva aguda de pequena amplitude
        fib_duration <- duration_variation * sampling_rate
        fib_window <- idx:(min(idx + fib_duration, n_samples))
        
        # Criar forma da fibrilação (onda positiva aguda com retorno rápido à linha de base)
        fib_wave <- c(
          seq(0, amplitude_variation, length.out = fib_duration/4),
          seq(amplitude_variation, 0, length.out = 3*fib_duration/4)
        )
        
        # Garantir que o padrão de fibrilação se encaixe na janela
        fib_pattern <- fib_wave[1:min(length(fib_wave), length(fib_window))]
        signal[fib_window[1:length(fib_pattern)]] <- 
          signal[fib_window[1:length(fib_pattern)]] + fib_pattern
      }
    }
  }
  
  # Adicionar descargas repetitivas complexas se configuradas
  if (length(complex_repetitive_discharges) > 0 && complex_repetitive_discharges$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(complex_repetitive_discharges$interval_sd) && complex_repetitive_discharges$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- complex_repetitive_discharges$start_time
      crd_times <- c()
      
      while (current_time < total_time) {
        crd_times <- c(crd_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = complex_repetitive_discharges$interval, 
                                    sd = complex_repetitive_discharges$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.5, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      crd_times <- seq(complex_repetitive_discharges$start_time, total_time, 
                       by = complex_repetitive_discharges$interval)
    }
    
    for (i in crd_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(complex_repetitive_discharges$amplitude_sd) && 
            complex_repetitive_discharges$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = complex_repetitive_discharges$amplitude, 
                                       sd = complex_repetitive_discharges$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.1, amplitude_variation)
        } else {
          amplitude_variation <- complex_repetitive_discharges$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(complex_repetitive_discharges$duration_sd) && 
            complex_repetitive_discharges$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = complex_repetitive_discharges$duration, 
                                      sd = complex_repetitive_discharges$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.1, duration_variation)
        } else {
          duration_variation <- complex_repetitive_discharges$duration
        }
        
        # Aplicar variação na frequência, se disponível
        if (!is.null(complex_repetitive_discharges$frequency_sd) && 
            complex_repetitive_discharges$frequency_sd > 0) {
          frequency_variation <- rnorm(1, mean = complex_repetitive_discharges$frequency, 
                                       sd = complex_repetitive_discharges$frequency_sd)
          # Garantir que a frequência seja sempre positiva
          frequency_variation <- max(10, frequency_variation)
        } else {
          frequency_variation <- complex_repetitive_discharges$frequency
        }
        
        # Modelar DRCs como descargas regulares agrupadas
        crd_duration <- duration_variation * sampling_rate
        crd_window <- idx:(min(idx + crd_duration, n_samples))
        
        # Criar padrão DRC (série de descargas de amplitude similar)
        # Número de descargas individuais no DRC
        n_discharges <- frequency_variation * duration_variation
        discharge_interval <- crd_duration / n_discharges
        
        crd_signal <- rep(0, length(crd_window))
        for (j in 1:n_discharges) {
          start_idx <- 1 + floor((j-1) * discharge_interval)
          if (start_idx < length(crd_window)) {
            end_idx <- min(start_idx + floor(discharge_interval/5), length(crd_window))
            crd_signal[start_idx:end_idx] <- amplitude_variation * 
              sin(seq(0, pi, length.out = end_idx - start_idx + 1))
          }
        }
        
        signal[crd_window] <- signal[crd_window] + crd_signal
      }
    }
  }
  
  # Adicionar ondas agudas positivas se configuradas
  if (length(positive_sharp_waves) > 0 && positive_sharp_waves$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(positive_sharp_waves$interval_sd) && positive_sharp_waves$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- positive_sharp_waves$start_time
      psw_times <- c()
      
      while (current_time < total_time) {
        psw_times <- c(psw_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = positive_sharp_waves$interval, 
                                    sd = positive_sharp_waves$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      psw_times <- seq(positive_sharp_waves$start_time, total_time, by = positive_sharp_waves$interval)
    }
    
    for (i in psw_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(positive_sharp_waves$amplitude_sd) && positive_sharp_waves$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = positive_sharp_waves$amplitude, 
                                       sd = positive_sharp_waves$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.1, amplitude_variation)
        } else {
          amplitude_variation <- positive_sharp_waves$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(positive_sharp_waves$duration_sd) && positive_sharp_waves$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = positive_sharp_waves$duration, 
                                      sd = positive_sharp_waves$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.01, duration_variation)
        } else {
          duration_variation <- positive_sharp_waves$duration
        }
        
        # Modelar uma onda aguda positiva
        psw_duration <- duration_variation * sampling_rate
        psw_window <- idx:(min(idx + psw_duration, n_samples))
        
        # Criar forma OAP (subida rápida e queda lenta)
        psw_wave <- c(
          seq(0, amplitude_variation, length.out = psw_duration/8),
          seq(amplitude_variation, -amplitude_variation/2, length.out = psw_duration/4),
          seq(-amplitude_variation/2, 0, length.out = 5*psw_duration/8)
        )
        
        # Garantir que o padrão OAP se encaixe na janela
        psw_pattern <- psw_wave[1:min(length(psw_wave), length(psw_window))]
        signal[psw_window[1:length(psw_pattern)]] <- 
          signal[psw_window[1:length(psw_pattern)]] + psw_pattern
      }
    }
  }
  
  # Adicionar unidades motoras anormais (potenciais polifásicos) se configuradas
  if (length(abnormal_motor_units) > 0 && abnormal_motor_units$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(abnormal_motor_units$interval_sd) && abnormal_motor_units$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- abnormal_motor_units$start_time
      amu_times <- c()
      
      while (current_time < total_time) {
        amu_times <- c(amu_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = abnormal_motor_units$interval, 
                                    sd = abnormal_motor_units$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.2, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      amu_times <- seq(abnormal_motor_units$start_time, total_time, 
                       by = abnormal_motor_units$interval)
    }
    
    for (i in amu_times) {
      # Encontrar o índice de tempo mais próximo
      idx <- which.min(abs(time - i))
      if (idx <= n_samples) {
        # Aplicar variação na amplitude, se disponível
        if (!is.null(abnormal_motor_units$amplitude_sd) && abnormal_motor_units$amplitude_sd > 0) {
          amplitude_variation <- rnorm(1, mean = abnormal_motor_units$amplitude, 
                                       sd = abnormal_motor_units$amplitude_sd)
          # Garantir que a amplitude seja sempre positiva
          amplitude_variation <- max(0.2, amplitude_variation)
        } else {
          amplitude_variation <- abnormal_motor_units$amplitude
        }
        
        # Aplicar variação na duração, se disponível
        if (!is.null(abnormal_motor_units$duration_sd) && abnormal_motor_units$duration_sd > 0) {
          duration_variation <- rnorm(1, mean = abnormal_motor_units$duration, 
                                      sd = abnormal_motor_units$duration_sd)
          # Garantir que a duração seja sempre positiva
          duration_variation <- max(0.05, duration_variation)
        } else {
          duration_variation <- abnormal_motor_units$duration
        }
        
        # Aplicar variação no número de fases, se disponível
        if (!is.null(abnormal_motor_units$phases_sd) && abnormal_motor_units$phases_sd > 0) {
          phases_variation <- round(rnorm(1, mean = abnormal_motor_units$phases, 
                                          sd = abnormal_motor_units$phases_sd))
          # Garantir que o número de fases seja sempre pelo menos 5 (polifásico)
          phases_variation <- max(5, phases_variation)
        } else {
          phases_variation <- abnormal_motor_units$phases
        }
        
        # Modelar uma unidade motora anormal (potencial polifásico)
        amu_duration <- duration_variation * sampling_rate
        amu_window <- idx:(min(idx + amu_duration, n_samples))
        
        # Criar forma do potencial polifásico
        segment_duration <- amu_duration / phases_variation
        amu_signal <- rep(0, length(amu_window))
        
        for (j in 1:phases_variation) {
          start_idx <- 1 + floor((j-1) * segment_duration)
          if (start_idx < length(amu_window)) {
            end_idx <- min(start_idx + floor(segment_duration), length(amu_window))
            
            # Alternar entre fases positivas e negativas
            if (j %% 2 == 1) {
              amu_signal[start_idx:end_idx] <- amplitude_variation * 
                sin(seq(0, pi, length.out = end_idx - start_idx + 1))
            } else {
              amu_signal[start_idx:end_idx] <- -amplitude_variation * 
                sin(seq(0, pi, length.out = end_idx - start_idx + 1))
            }
          }
        }
        
        signal[amu_window] <- signal[amu_window] + amu_signal
      }
    }
  }
  
  # Adicionar padrão de recrutamento reduzido se configurado
  if (length(reduced_recruitment) > 0 && reduced_recruitment$enabled) {
    # Definir parâmetros para o padrão de recrutamento
    start_time <- reduced_recruitment$start_time
    end_time <- min(start_time + reduced_recruitment$duration, total_time)
    
    # Converter tempos para índices
    start_idx <- which.min(abs(time - start_time))
    end_idx <- which.min(abs(time - end_time))
    
    # Determinar o número de unidades motoras ativas
    n_motor_units <- reduced_recruitment$motor_units
    
    # Criar padrão para cada unidade motora
    for (unit in 1:n_motor_units) {
      # Determinar a taxa de disparo para esta unidade motora
      if (!is.null(reduced_recruitment$firing_rate_sd) && reduced_recruitment$firing_rate_sd > 0) {
        firing_rate <- rnorm(1, mean = reduced_recruitment$firing_rate, 
                             sd = reduced_recruitment$firing_rate_sd)
        # Garantir que a taxa de disparo seja sempre positiva
        firing_rate <- max(5, firing_rate)
      } else {
        firing_rate <- reduced_recruitment$firing_rate
      }
      
      # Determinar a amplitude para esta unidade motora
      if (!is.null(reduced_recruitment$amplitude_sd) && reduced_recruitment$amplitude_sd > 0) {
        amplitude <- rnorm(1, mean = reduced_recruitment$amplitude, 
                           sd = reduced_recruitment$amplitude_sd)
        # Garantir que a amplitude seja sempre positiva
        amplitude <- max(0.2, amplitude)
      } else {
        amplitude <- reduced_recruitment$amplitude
      }
      
      # Determinar a duração do potencial da unidade motora
      if (!is.null(reduced_recruitment$motor_unit_duration_sd) && 
          reduced_recruitment$motor_unit_duration_sd > 0) {
        mu_duration <- rnorm(1, mean = reduced_recruitment$motor_unit_duration, 
                             sd = reduced_recruitment$motor_unit_duration_sd)
        # Garantir que a duração seja sempre positiva
        mu_duration <- max(0.01, mu_duration)
      } else {
        mu_duration <- reduced_recruitment$motor_unit_duration
      }
      
      # Criar disparos para esta unidade motora durante o período de contração
      mu_interval <- 1 / firing_rate
      firing_times <- seq(start_time, end_time, by = mu_interval)
      
      # Adicionar variabilidade aos tempos de disparo
      if (!is.null(reduced_recruitment$jitter) && reduced_recruitment$jitter > 0) {
        firing_times <- firing_times + rnorm(length(firing_times), 
                                             mean = 0, 
                                             sd = reduced_recruitment$jitter)
      }
      
      # Para cada tempo de disparo, adicionar o potencial de unidade motora ao sinal
      for (fire_time in firing_times) {
        # Encontrar o índice de tempo mais próximo
        fire_idx <- which.min(abs(time - fire_time))
        if (fire_idx <= n_samples) {
          # Definir a duração do potencial em amostras
          potential_duration <- mu_duration * sampling_rate
          potential_window <- fire_idx:(min(fire_idx + potential_duration, n_samples))
          
          # Criar forma do potencial de unidade motora (trifásico)
          potential_wave <- c(
            seq(0, amplitude * 0.3, length.out = potential_duration/5),
            seq(amplitude * 0.3, amplitude, length.out = potential_duration/5),
            seq(amplitude, -amplitude * 0.7, length.out = 2*potential_duration/5),
            seq(-amplitude * 0.7, 0, length.out = potential_duration/5)
          )
          
          # Garantir que o potencial se encaixe na janela
          potential_pattern <- potential_wave[1:min(length(potential_wave), length(potential_window))]
          signal[potential_window[1:length(potential_pattern)]] <- 
            signal[potential_window[1:length(potential_pattern)]] + potential_pattern
        }
      }
    }
  }
  
  # Adicionar alguma atividade muscular aleatória (para realismo)
  random_activity <- seq(0, total_time, length.out = n_samples)
  random_activity <- sin(2 * pi * 0.5 * random_activity) * 0.05 * 
    runif(length(random_activity), 0, 1)
  
  # Adicionar ao sinal
  signal <- signal + random_activity
  
  # Retornar tempo e sinal
  return(list(time = time, signal = signal))
}

# Função para marcar anormalidades EMG
mark_abnormalities <- function(emg_data, normal_emg = list(), fasciculations, fibrillations, 
                               complex_repetitive_discharges, positive_sharp_waves,
                               abnormal_motor_units, reduced_recruitment) {
  
  markers <- data.frame(
    time = numeric(),
    y = numeric(),
    type = character(),
    stringsAsFactors = FALSE
  )
  # Adicionar marcadores de EMG normal
  if (length(normal_emg) > 0 && normal_emg$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(normal_emg$interval_sd) && normal_emg$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- normal_emg$start_time
      normal_times <- c()
      
      while (current_time < max(emg_data$time)) {
        normal_times <- c(normal_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = normal_emg$interval, sd = normal_emg$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      normal_times <- seq(normal_emg$start_time, max(emg_data$time), by = normal_emg$interval)
    }
    
    for (i in normal_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1, # Ligeiramente acima do sinal
        type = "Normal_EMG"
      ))
    }
  }
  
  # Adicionar marcadores de fasciculações
  if (length(fasciculations) > 0 && fasciculations$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(fasciculations$interval_sd) && fasciculations$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- fasciculations$start_time
      fascic_times <- c()
      
      while (current_time < max(emg_data$time)) {
        fascic_times <- c(fascic_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = fasciculations$interval, sd = fasciculations$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      fascic_times <- seq(fasciculations$start_time, max(emg_data$time), by = fasciculations$interval)
    }
    
    for (i in fascic_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1, # Ligeiramente acima do sinal
        type = "Fasciculation"
      ))
    }
  }
  
  # Adicionar marcadores de fibrilações
  if (length(fibrillations) > 0 && fibrillations$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(fibrillations$interval_sd) && fibrillations$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- fibrillations$start_time
      fib_times <- c()
      
      while (current_time < max(emg_data$time)) {
        fib_times <- c(fib_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = fibrillations$interval, sd = fibrillations$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      fib_times <- seq(fibrillations$start_time, max(emg_data$time), by = fibrillations$interval)
    }
    
    for (i in fib_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1,
        type = "Fibrillation"
      ))
    }
  }
  
  
  # Adicionar marcadores de DRC
  if (length(complex_repetitive_discharges) > 0 && complex_repetitive_discharges$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(complex_repetitive_discharges$interval_sd) && complex_repetitive_discharges$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- complex_repetitive_discharges$start_time
      crd_times <- c()
      
      while (current_time < max(emg_data$time)) {
        crd_times <- c(crd_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = complex_repetitive_discharges$interval, 
                                    sd = complex_repetitive_discharges$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.5, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      crd_times <- seq(complex_repetitive_discharges$start_time, max(emg_data$time), 
                       by = complex_repetitive_discharges$interval)
    }
    
    for (i in crd_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1,
        type = "CRD"
      ))
    }
  }
  
  # Adicionar marcadores de OAP
  if (length(positive_sharp_waves) > 0 && positive_sharp_waves$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(positive_sharp_waves$interval_sd) && positive_sharp_waves$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- positive_sharp_waves$start_time
      psw_times <- c()
      
      while (current_time < max(emg_data$time)) {
        psw_times <- c(psw_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = positive_sharp_waves$interval, 
                                    sd = positive_sharp_waves$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.1, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      psw_times <- seq(positive_sharp_waves$start_time, max(emg_data$time), 
                       by = positive_sharp_waves$interval)
    }
    
    for (i in psw_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1,
        type = "PSW"
      ))
    }
  }
  
  # Adicionar marcadores de unidades motoras anormais
  if (length(abnormal_motor_units) > 0 && abnormal_motor_units$enabled) {
    # Usar desvio padrão para intervalo, se disponível
    if (!is.null(abnormal_motor_units$interval_sd) && abnormal_motor_units$interval_sd > 0) {
      # Gerar um vetor de tempos com intervalos aleatórios
      current_time <- abnormal_motor_units$start_time
      amu_times <- c()
      
      while (current_time < max(emg_data$time)) {
        amu_times <- c(amu_times, current_time)
        # Adicionar intervalo com variação aleatória
        interval_variation <- rnorm(1, mean = abnormal_motor_units$interval, 
                                    sd = abnormal_motor_units$interval_sd)
        # Garantir que o intervalo seja sempre positivo
        interval_variation <- max(0.2, interval_variation)
        current_time <- current_time + interval_variation
      }
    } else {
      # Usar intervalo fixo se não houver desvio padrão
      amu_times <- seq(abnormal_motor_units$start_time, max(emg_data$time), 
                       by = abnormal_motor_units$interval)
    }
    
    for (i in amu_times) {
      closest_idx <- which.min(abs(emg_data$time - i))
      markers <- rbind(markers, data.frame(
        time = emg_data$time[closest_idx],
        y = emg_data$signal[closest_idx] + 0.1,
        type = "AMU"
      ))
    }
  }
  
  # Adicionar marcadores de recrutamento reduzido
  if (length(reduced_recruitment) > 0 && reduced_recruitment$enabled) {
    # Definir os limites do período de recrutamento reduzido
    start_time <- reduced_recruitment$start_time
    end_time <- min(start_time + reduced_recruitment$duration, max(emg_data$time))
    
    # Encontrar os índices de início e fim
    start_idx <- which.min(abs(emg_data$time - start_time))
    end_idx <- which.min(abs(emg_data$time - end_time))
    
    # Adicionar marcadores no início e no fim do período
    markers <- rbind(markers, data.frame(
      time = emg_data$time[start_idx],
      y = emg_data$signal[start_idx] + 0.1,
      type = "RR_start"
    ))
    
    markers <- rbind(markers, data.frame(
      time = emg_data$time[end_idx],
      y = emg_data$signal[end_idx] + 0.1,
      type = "RR_end"
    ))
  }
  
  return(markers)
}

# Função para aplicar janelamento aos dados (para a aba de transformada de Fourier)
apply_window <- function(signal, window_type) {
  n <- length(signal)
  
  window <- switch(window_type,
                   "rectangular" = rep(1, n),
                   "hanning" = 0.5 * (1 - cos(2 * pi * (0:(n-1)) / (n-1))),
                   "hamming" = 0.54 - 0.46 * cos(2 * pi * (0:(n-1)) / (n-1)),
                   "blackman" = 0.42 - 0.5 * cos(2 * pi * (0:(n-1)) / (n-1)) + 
                     0.08 * cos(4 * pi * (0:(n-1)) / (n-1)),
                   rep(1, n))  # default: rectangular window
  
  return(signal * window)
}

# Função para calcular a transformada de Fourier
calculate_fft <- function(signal, sampling_rate, window_type = "hanning", remove_dc = TRUE) {
  # Remover média (componente DC) se solicitado
  if (remove_dc) {
    signal <- signal - mean(signal)
  }
  
  # Aplicar janelamento
  windowed_signal <- apply_window(signal, window_type)
  
  # Calcular FFT
  n <- length(windowed_signal)
  fft_result <- fft(windowed_signal)
  
  # Calcular o módulo (amplitude) do resultado da FFT
  # Normalizar por N e multiplicar por 2 (pois usamos apenas metade do espectro)
  magnitude <- abs(fft_result[1:(n/2+1)]) / n * 2
  # Corrigir o primeiro valor (DC) para que não seja dobrado
  magnitude[1] <- magnitude[1] / 2
  
  # Calcular vetor de frequências
  freq <- seq(0, sampling_rate/2, length.out = length(magnitude))
  
  return(list(freq = freq, magnitude = magnitude))
}

# Função para identificar picos de frequência significativos
find_peaks <- function(freq, magnitude, min_height = 0.005, min_distance = 5) {
  # Encontrar todos os picos locais
  peaks <- which(diff(sign(diff(magnitude))) == -2) + 1
  
  # Filtrar picos pela altura mínima
  peaks <- peaks[magnitude[peaks] > min_height]
  
  # Ordenar picos pela magnitude
  peaks <- peaks[order(magnitude[peaks], decreasing = TRUE)]
  
  # Se houver muitos picos, limitar aos 10 maiores
  if (length(peaks) > 10) {
    peaks <- peaks[1:10]
  }
  
  # Retornar frequências e magnitudes dos picos
  return(data.frame(
    frequency = freq[peaks],
    magnitude = magnitude[peaks]
  ))
}

# ============================= INTERFACE UI ==================================
ufu_logo   <- "https://media.licdn.com/dms/image/sync/v2/D4D27AQEVTwMiGJw9ug/articleshare-shrink_800/articleshare-shrink_800/0/1729190077520?e=2147483647&v=beta&t=QcILsh4kaWVAM0spEbwWbDjogVGcoUIGvxSFapQbyi0"  # URL direto da marca UFU
ppgeb_logo <- "https://ppgeb.feelt.ufu.br/sites/padraopos.ufu.br/files/logo_ppgeb_0.png"

ui <- dashboardPage(
  skin = "blue",
  # ------------------------ CABEÇALHO (LOGOS) -------------------------------
  dashboardHeader(
    title = tags$div(style = "display:flex; align-items:center; gap:4px;",
                     tags$img(src = ufu_logo,   height = "38"),
                     tags$img(src = ppgeb_logo, height = "38")),
    titleWidth = 260
  ),
  
  # ------------------------ MENU LATERAL -----------------------------------
  dashboardSidebar(
    width = 270,
    sidebarMenu(id = "tabs",
                menuItem("Simulação de EMG", tabName = "simulation", icon = icon("wave-square")),
                menuItem("Transformada de Fourier", tabName = "analysis", icon = icon("chart-line")),
                menuItem("Gerador de Senoides", tabName = "sine_generator", icon = icon("wave-square"))
                
    ),
    hr(style = "margin:6px 0;"),
    tags$div(
      style = "padding:12px; font-size:13px; color:#ffffff; line-height:20px; text-align:left;",
      HTML(
        "📚 <b>Disciplina PGEB39</b><br/>
      🗓️ <b>1º Sem/2025</b><br/>
      🧑‍🏫 Prof. Drº João Batista Destro Filho<br/><br/>
      💼 <u>Seminário 24/04/2025</u><br/>
      🚀 Aplicação desenvolvida por alunos do PPGEB‑UFU<br/><br/>
      👥 <b>Grupo 2</b>: Fernando&nbsp;|&nbsp;Francisco&nbsp;|&nbsp;João&nbsp;|&nbsp;Ysabel<br/><br/>
      🎯 <b>Objetivo:</b> Capacitar profissionais da saúde a identificar e reconhecer padrões de EMG característicos da Esclerose Lateral Amiotrófica.<br/><br/>
      🐙 <b>Veja o código no GitHub:</b><br/>
      <a href='https://github.com/franciscodnlneto/ppgeb39' target='_blank' style='color:#1e90ff; text-decoration:none;'>
      🔗 CLIQUE AQUI! Você é livre para baixar, modificar e aprimorar!
      </a>"
      )
    )
  ),
  
  # ------------------------ CORPO PRINCIPAL ---------------------------------
  dashboardBody(
    useShinyjs(),
    # ---------- CSS personalizado -----------------------------------------
    tags$head(tags$style(HTML("\n      .nav-tabs-custom .nav-tabs li a {background:#e8eef7; color:#0055a5; font-weight:600;}\n      .nav-tabs-custom .nav-tabs li a:hover {background:#d0d8e8;}\n      .nav-tabs-custom .nav-tabs li.active a,\n      .nav-tabs-custom .nav-tabs li.active a:hover {background:#0055a5; color:#fff;}\n    "))),
    
    tabItems(
      # ---------------------- ABA: SIMULAÇÃO --------------------------------
      tabItem(tabName = "simulation",
              fluidRow(
                column(12,
                       tags$h2("Simulador Interativo de EMG – Padrões de ELA",
                               style="font-weight:700; color:#0055a5; margin:2px 0 14px;")
                )
              ),
              fluidRow(
                box(title = "Configuração da Simulação de EMG", width = 12, collapsible = TRUE,
                    status = "primary", solidHeader = TRUE,                  
                    fluidRow(
                      column(4,
                             sliderInput("total_time", "Tempo Total de Registro (s):", 
                                         min = 1, max = 30, value = 10, step = 1),
                             sliderInput("sampling_rate", "Taxa de Amostragem (Hz):", 
                                         min = 500, max = 5000, value = 2000, step = 500),
                             sliderInput("baseline_noise", "Nível de Ruído de Base:", 
                                         min = 0.01, max = 0.3, value = 0.05, step = 0.01)
                      ),
                      
                      column(8,
                             tabBox(
                               title = "Configuração de Padrões de EMG",
                               width = 12,
                               id = "emg_pattern_tabs",
                               
                               # Nova aba para EMG Normal
                               tabPanel("EMG Normal (Saudável)",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "EMG normal apresenta padrões de ativação que refletem o recrutamento ordenado e sincronizado de unidades motoras durante contrações voluntárias. Os potenciais são regulares e proporcionais à força exercida."),
                                              tags$p(tags$b("CARACTERÍSTICAS:"), 
                                                     "Os potenciais de unidades motoras normais têm amplitude, frequência e duração dentro de faixas específicas. Durante uma contração, há recrutamento progressivo de unidades motoras, com aumento da frequência de disparo à medida que a força aumenta."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Potenciais de ação regulares com amplitude moderada, morfologia trifásica consistente (subida positiva, deflexão negativa e retorno à linha base), e intervalos regulares entre contrações."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Potenciais trifásicos com amplitude e intervalo configuráveis, representando contrações voluntárias normais. A variabilidade natural é simulada usando desvios padrão para os parâmetros principais.")
                                            )
                                          )
                                        ),
                                        checkboxInput("normal_emg_enabled", "Ativar EMG Normal", TRUE),
                                        conditionalPanel(
                                          condition = "input.normal_emg_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("normal_amplitude", "Amplitude da Contração:", 
                                                               min = 0.2, max = 2, value = 0.6, step = 0.1),
                                                   sliderInput("normal_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.5, value = 0.0, step = 0.05)
                                            ),
                                            column(4,
                                                   sliderInput("normal_interval", "Intervalo entre Contrações (s):", 
                                                               min = 0.5, max = 5, value = 1.5, step = 0.1),
                                                   sliderInput("normal_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 1, value = 0.0, step = 0.05)
                                            ),
                                            column(4,
                                                   sliderInput("normal_duration", "Duração do Potencial (s):", 
                                                               min = 0.05, max = 0.5, value = 0.15, step = 0.01),
                                                   sliderInput("normal_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.1, value = 0.00, step = 0.01),
                                                   sliderInput("normal_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 0.5, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Aba de Fasciculações (ALTERADA para FALSE em enabled)
                               tabPanel("Fasciculações",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "Fasciculações são contrações involuntárias de pequenas porções de músculo visíveis sob a pele como tremores ou \"pulos\". Ocorrem por descargas espontâneas em neurônios motores ou seus axônios (estruturas que transmitem os sinais elétricos). Estão relacionadas à reinervação — processo no qual neurônios sobreviventes tentam reconectar-se a fibras musculares que perderam sua inervação, criando ramificações instáveis que podem gerar essas descargas."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "Na ELA, há degeneração de neurônios motores. Os que permanecem tentam reinervar fibras órfãs, mas essas conexões são instáveis e disparam sozinhas. Fasciculações são comuns e persistentes em ELA, diferindo das fasciculações ocasionais em indivíduos saudáveis. Com a progressão da doença, sua frequência pode cair, pois restam poucos neurônios funcionais."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Potenciais isolados, trifásicos, com alta amplitude e curta duração, surgindo de forma irregular e espontânea, destacados da linha de base. O formato clássico é um pico positivo, seguido de uma deflexão negativa e um retorno mais suave à linha base."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Foram usados pulsos assimétricos trifásicos com quatro segmentos: subida à amplitude máxima, queda para valor negativo (70% da amplitude), subida parcial (40%) e retorno ao zero. As fasciculações ocorrem em tempos aleatórios, determinados por intervalos com média e desvio padrão ajustáveis.")
                                            )
                                          )
                                        ),
                                        checkboxInput("fascic_enabled", "Ativar Fasciculações", FALSE),
                                        conditionalPanel(
                                          condition = "input.fascic_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("fascic_amplitude", "Amplitude:", 
                                                               min = 0.2, max = 2, value = 0.8, step = 0.1),
                                                   sliderInput("fascic_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.5, value = 0.1, step = 0.05)
                                            ),
                                            column(4,
                                                   sliderInput("fascic_interval", "Intervalo (s):", 
                                                               min = 0.5, max = 5, value = 2, step = 0.5),
                                                   sliderInput("fascic_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 1, value = 0.3, step = 0.1)
                                            ),
                                            column(4,
                                                   sliderInput("fascic_duration", "Duração (s):", 
                                                               min = 0.05, max = 0.5, value = 0.2, step = 0.05),
                                                   sliderInput("fascic_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.1, value = 0.03, step = 0.01),
                                                   sliderInput("fascic_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 1, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Aba de Fibrilações
                               tabPanel("Fibrilações",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "Fibrilações são contrações espontâneas de fibras musculares individuais — muito pequenas para serem vistas externamente. Aparecem em músculos denervados, com fibras instáveis que geram potenciais elétricos por si só. Detectadas apenas por EMG com eletrodo de agulha."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "São sinal clássico de denervação ativa em ELA. Surgem quando fibras musculares perdem sua conexão com os neurônios motores. Mais frequentes no início da doença, podendo diminuir em fases avançadas quando há fibrose (substituição por tecido cicatricial)."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Potenciais de pequena amplitude e curta duração. Formato simples com subida rápida e descida gradual. Aparecem regularmente, com morfologia uniforme, refletindo ativação de fibras únicas."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Modeladas como ondas positivas agudas: subida rápida (25% do tempo) e retorno gradual (75%). Amplitude (0,1–0,5 mV), duração (0,01–0,1s) e intervalo (0,2–2s) configuráveis. A variação natural é simulada com desvios padrão.")
                                            )
                                          )
                                        ),
                                        checkboxInput("fib_enabled", "Ativar Fibrilações", FALSE),
                                        conditionalPanel(
                                          condition = "input.fib_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("fib_amplitude", "Amplitude:", 
                                                               min = 0.1, max = 0.5, value = 0.2, step = 0.05),
                                                   sliderInput("fib_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.1, value = 0.03, step = 0.01)
                                            ),
                                            column(4,
                                                   sliderInput("fib_interval", "Intervalo (s):", 
                                                               min = 0.2, max = 2, value = 0.5, step = 0.1),
                                                   sliderInput("fib_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 0.5, value = 0.1, step = 0.05)
                                            ),
                                            column(4,
                                                   sliderInput("fib_duration", "Duração (s):", 
                                                               min = 0.01, max = 0.1, value = 0.03, step = 0.01),
                                                   sliderInput("fib_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.02, value = 0.005, step = 0.001),
                                                   sliderInput("fib_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 0.5, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Aba de Descargas Repetitivas Complexas
                               tabPanel("Descargas Repetitivas Complexas",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "DRCs são séries rítmicas de potenciais repetitivos com forma constante, semelhantes ao som de uma metralhadora no EMG. Resultam de conexões elétricas anormais entre fibras musculares vizinhas em músculos denervados."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "Em ELA, surgem após denervação crônica. Fibras sem inervação criam conexões diretas com vizinhas, levando à transmissão desorganizada de sinais. As DRCs indicam cronicidade e dano muscular prolongado."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Surtos de potenciais estereotipados, com mesma amplitude e intervalo, formando padrão regular tipo \"pente\". Frequência de 20-100 Hz, duração de 0,5–3s, com intervalos entre surtos."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Cada DRC é uma sequência de picos gerados por senóides (ondas suaves). A frequência e duração determinam quantas descargas ocorrem. Cada \"pico\" é uma senoide curta. A frequência, intervalo, amplitude e duração têm variabilidade simulada por desvios padrão.")
                                            )
                                          )
                                        ),
                                        checkboxInput("crd_enabled", "Ativar Descargas Repetitivas Complexas", FALSE),
                                        conditionalPanel(
                                          condition = "input.crd_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("crd_amplitude", "Amplitude:", 
                                                               min = 0.2, max = 1, value = 0.5, step = 0.1),
                                                   sliderInput("crd_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.2, value = 0.05, step = 0.01),
                                                   sliderInput("crd_frequency", "Frequência de Descarga (Hz):", 
                                                               min = 20, max = 100, value = 40, step = 10),
                                                   sliderInput("crd_frequency_sd", "Desvio Padrão da Frequência (Hz):", 
                                                               min = 0, max = 20, value = 5, step = 1)
                                            ),
                                            column(4,
                                                   sliderInput("crd_interval", "Intervalo (s):", 
                                                               min = 1, max = 10, value = 5, step = 1),
                                                   sliderInput("crd_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 2, value = 0.5, step = 0.1)
                                            ),
                                            column(4,
                                                   sliderInput("crd_duration", "Duração (s):", 
                                                               min = 0.5, max = 3, value = 1, step = 0.1),
                                                   sliderInput("crd_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.5, value = 0.1, step = 0.05),
                                                   sliderInput("crd_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 3, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Aba de Ondas Agudas Positivas
                               tabPanel("Ondas Agudas Positivas",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "As OAPs são potenciais bifásicos com subida rápida e descida mais lenta, causadas por instabilidade da membrana de fibras musculares denervadas. Visualmente, parecem dentes de serra no EMG."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "Assim como as fibrilações, indicam denervação ativa. Surgem semanas após a perda da conexão nervosa, permanecendo por meses. Em ELA, sua presença em múltiplos músculos reforça o diagnóstico."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Potenciais com subida rápida (positivo), seguida por queda negativa suave. Curta duração (0,05–0,3s), amplitude moderada (0,2–1mV), aparência dente de serra, repetitivos, geralmente regulares."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Criadas com três fases: subida curta (12,5% do tempo), queda negativa (25%) e retorno gradual (62,5%). Os parâmetros (amplitude, duração, intervalo) têm ajustes e variabilidade por desvio padrão.")
                                            )
                                          )
                                        ),
                                        checkboxInput("psw_enabled", "Ativar Ondas Agudas Positivas", FALSE),
                                        conditionalPanel(
                                          condition = "input.psw_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("psw_amplitude", "Amplitude:", 
                                                               min = 0.2, max = 1, value = 0.6, step = 0.1),
                                                   sliderInput("psw_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.2, value = 0.05, step = 0.01)
                                            ),
                                            column(4,
                                                   sliderInput("psw_interval", "Intervalo (s):", 
                                                               min = 0.5, max = 5, value = 1.5, step = 0.5),
                                                   sliderInput("psw_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 1, value = 0.2, step = 0.1)
                                            ),
                                            column(4,
                                                   sliderInput("psw_duration", "Duração (s):", 
                                                               min = 0.05, max = 0.3, value = 0.1, step = 0.05),
                                                   sliderInput("psw_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.05, value = 0.01, step = 0.005),
                                                   sliderInput("psw_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 2, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Nova aba para Unidades Motoras Anormais (Potenciais Polifásicos)
                               tabPanel("Unidades Motoras Anormais",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "Potenciais polifásicos têm 5 ou mais inflexões. Representam unidades motoras mal reinervadas, com fibras ativadas de forma desordenada devido à condução assimétrica. São típicos de reinervação compensatória."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "Com a morte de neurônios motores em ELA, os sobreviventes reinervam fibras soltas. Isso leva à ativação dessincronizada dentro da mesma unidade motora. O padrão polifásico aparece nos estágios intermediários da doença, refletindo reorganização caótica."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Sinais com várias oscilações acima e abaixo da linha de base (5+ fases), maior duração (>15ms), forma irregular e amplitude variável. Sinal \"serrilhado\", visível durante contrações voluntárias."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Séries de senóides alternadas (positivas e negativas) representam cada fase. O número de fases, duração, amplitude e intervalo são configuráveis. A variação natural é ajustada por desvios padrão. Duração total de 0,1–0,5s.")
                                            )
                                          )
                                        ),
                                        checkboxInput("amu_enabled", "Ativar Potenciais Polifásicos", FALSE),
                                        conditionalPanel(
                                          condition = "input.amu_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("amu_amplitude", "Amplitude:", 
                                                               min = 0.2, max = 1.5, value = 0.7, step = 0.1),
                                                   sliderInput("amu_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.3, value = 0.1, step = 0.01)
                                            ),
                                            column(4,
                                                   sliderInput("amu_phases", "Número de Fases:", 
                                                               min = 5, max = 15, value = 7, step = 1),
                                                   sliderInput("amu_phases_sd", "Desvio Padrão das Fases:", 
                                                               min = 0, max = 3, value = 1, step = 0.5),
                                                   sliderInput("amu_interval", "Intervalo (s):", 
                                                               min = 0.5, max = 5, value = 2, step = 0.5),
                                                   sliderInput("amu_interval_sd", "Desvio Padrão do Intervalo (s):", 
                                                               min = 0, max = 1, value = 0.2, step = 0.1)
                                            ),
                                            column(4,
                                                   sliderInput("amu_duration", "Duração (s):", 
                                                               min = 0.1, max = 0.5, value = 0.2, step = 0.05),
                                                   sliderInput("amu_duration_sd", "Desvio Padrão da Duração (s):", 
                                                               min = 0, max = 0.1, value = 0.03, step = 0.01),
                                                   sliderInput("amu_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 1.5, step = 0.5)
                                            )
                                          )
                                        )
                               ),
                               
                               # Nova aba para Padrão de Recrutamento Reduzido
                               tabPanel("Recrutamento Reduzido",
                                        div(
                                          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                                          tags$details(
                                            tags$summary(
                                              tags$span(
                                                style = "font-weight: bold; color: #0055a5; cursor: pointer;",
                                                HTML("⚡️ 🔍 QUERO SABER +")
                                              )
                                            ),
                                            tags$div(
                                              style = "padding: 10px; font-size: 14px;",
                                              tags$p(tags$b("DICIONÁRIO:"), 
                                                     "Recrutamento reduzido ocorre quando menos unidades motoras são ativadas durante uma contração. Em um músculo saudável, há aumento progressivo de unidades recrutadas conforme a força aumenta. Com menos neurônios motores funcionais, esse padrão é alterado."),
                                              tags$p(tags$b("QUAL A RELAÇÃO COM ELA:"), 
                                                     "Na ELA, o número de unidades motoras viáveis diminui com o tempo. O cérebro tenta compensar aumentando a frequência de disparo ou reinervando. Contudo, a capacidade de recrutar novas unidades se reduz, resultando em contrações fracas mesmo com esforço máximo."),
                                              tags$p(tags$b("O QUE SE ESPERA VER NO GRÁFICO:"), 
                                                     "Menor número de potenciais visíveis durante contração. \"Gaps\" no sinal. Potenciais de maior amplitude e complexidade (devido à reinervação). Frequência de disparo elevada nas unidades remanescentes, formando padrão rarefeito e incompleto."),
                                              tags$p(tags$b("COMO FOI SIMULADO:"), 
                                                     "Simula-se contração com número limitado de unidades motoras (1–10), cada uma disparando repetidamente em alta frequência (5–30 Hz). Cada potencial é trifásico, com amplitude aumentada. Há controle da variabilidade, duração e jitter, representando os diferentes graus de perda funcional.")
                                            )
                                          )
                                        ),
                                        checkboxInput("rr_enabled", "Ativar Padrão de Recrutamento Reduzido", FALSE),
                                        conditionalPanel(
                                          condition = "input.rr_enabled == true",
                                          fluidRow(
                                            column(4,
                                                   sliderInput("rr_motor_units", "Número de Unidades Motoras:", 
                                                               min = 1, max = 10, value = 3, step = 1),
                                                   sliderInput("rr_amplitude", "Amplitude das Unidades:", 
                                                               min = 0.2, max = 1.5, value = 0.8, step = 0.1),
                                                   sliderInput("rr_amplitude_sd", "Desvio Padrão da Amplitude:", 
                                                               min = 0, max = 0.3, value = 0.1, step = 0.05)
                                            ),
                                            column(4,
                                                   sliderInput("rr_firing_rate", "Taxa de Disparo (Hz):", 
                                                               min = 5, max = 30, value = 15, step = 1),
                                                   sliderInput("rr_firing_rate_sd", "Desvio Padrão da Taxa:", 
                                                               min = 0, max = 5, value = 2, step = 0.5),
                                                   sliderInput("rr_jitter", "Variabilidade de Tempo (s):", 
                                                               min = 0, max = 0.02, value = 0.005, step = 0.001)
                                            ),
                                            column(4,
                                                   sliderInput("rr_duration", "Duração da Contração (s):", 
                                                               min = 1, max = 10, value = 3, step = 0.5),
                                                   sliderInput("rr_motor_unit_duration", "Duração do Potencial (s):", 
                                                               min = 0.01, max = 0.1, value = 0.03, step = 0.01),
                                                   sliderInput("rr_motor_unit_duration_sd", "Desvio da Duração:", 
                                                               min = 0, max = 0.02, value = 0.005, step = 0.001),
                                                   sliderInput("rr_start", "Tempo Inicial (s):", 
                                                               min = 0, max = 5, value = 4, step = 0.5)
                                            )
                                          )
                                        )
                               )
                             )
                      )
                    ),
                    
                    downloadButton("download_btn", "Exportar Dados (CSV)", 
                                   class = "btn-success")
                )
              ),
              
              fluidRow(
                box(title = "Visualização do Sinal EMG", width = 12, collapsible = TRUE,
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("emg_plot", height = "350px") %>% withSpinner())
              ),
              fluidRow(
                box(title = "Anormalidades Detectadas", width = 12, collapsible = TRUE,
                    status = "warning", solidHeader = TRUE,
                    DT::dataTableOutput("abnormality_table") %>% withSpinner())
              )
      ),
      
      # ---------------------- ABA: TRANSFORMADA DE FOURIER -----------------------------------
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Transformada de Fourier - Análise de EMG", width = 12,
                    collapsible = TRUE, status = "primary", solidHeader = TRUE,
                    p("Este módulo permite analisar dados de EMG no domínio da frequência usando a Transformada de Fourier."),
                    tags$ul(
                      tags$li("Carregue dados reais ou simulados de EMG (formato CSV)"),
                      tags$li("O sistema aplica a transformada de Fourier para analisar componentes de frequência"),
                      tags$li("Visualize tanto o sinal original quanto seu espectro de frequência")
                    ),
                    fileInput("emg_file", "Carregar Dados de EMG (CSV)", accept = ".csv"),
                    fluidRow(
                      column(4,
                             numericInput("sampling_rate_input", "Frequência de Amostragem (Hz):", 
                                          value = 2000, min = 100, max = 10000),
                             checkboxInput("remove_dc", "Remover Componente DC", value = TRUE)
                      ),
                     
                      column(4,
                             selectInput("window_function", "Função de Janelamento:",
                                         choices = c("Retangular" = "rectangular",
                                                     "Hanning" = "hanning",
                                                     "Hamming" = "hamming",
                                                     "Blackman" = "blackman"),
                                         selected = "hanning")
                      ),
                      column(4,
                             numericInput("max_freq_display", "Frequência Máxima para Exibição (Hz):", 
                                          value = 500, min = 10, max = 2500)
                      )
                    ),
                    # Bloco de explicações técnicas interativas
                    div(
                      style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
                      
                      tags$details(
                        tags$summary(HTML("🎚️ <b>Frequência de Amostragem (Hz)</b> – <i>O que é isso?</i>")),
                        tags$p("É o número de vezes por segundo que o sinal é medido. Por exemplo, 2000 Hz significa que 2000 amostras são captadas a cada segundo."),
                        tags$p("🧠 Segundo o Teorema de Nyquist, para representar corretamente um sinal sem distorção, a frequência de amostragem deve ser pelo menos o dobro da maior frequência presente no sinal."),
                        tags$p("🔍 Se você quer analisar até 500 Hz, precisa amostrar com pelo menos 1000 Hz. Caso contrário, ocorre o *aliasing*, onde frequências altas 'parecem' frequências mais baixas, gerando distorções."),
                        tags$p("📌 Exemplo: se um sinal tem componente a 600 Hz e você amostra a 1000 Hz (violando Nyquist), ele parecerá estar em 400 Hz (600 - 1000 = -400 → alias para +400)."),
                        tags$p("🔗 Saiba mais em: ", tags$a(href = "https://www.brickschool.com.br/post/teorema-de-nyquist", target = "_blank", "Teorema de Nyquist – BrickSchool"))
                      ),
                      
                      tags$details(
                        tags$summary(HTML("🚫 <b>Remover Componente DC</b> – <i>Por que fazer isso?</i>")),
                        tags$p("O componente DC é a média do sinal. Em sinais reais, muitas vezes ele é diferente de zero devido a ruídos de base."),
                        tags$p("Remover o componente DC centraliza o sinal em torno de zero, evitando que o gráfico de frequência mostre um pico falso em 0 Hz."),
                        tags$p("✅ Recomendado: manter essa opção ativada para focar nas oscilações reais do sinal EMG.")
                      ),
                      
                      tags$details(
                        tags$summary(HTML("📐 <b>Função de Janelamento</b> – <i>Para que serve?</i>")),
                        tags$p("Quando aplicamos a FFT em um sinal finito, estamos implicitamente cortando o início e o fim. Isso cria 'bordas duras' e pode gerar distorções conhecidas como *vazamento espectral* (spectral leakage)."),
                        tags$p("📉 Imagine que o sinal fosse um som contínuo, mas você parasse a gravação de forma brusca. O corte pode criar artefatos que não existiam no som real."),
                        tags$p("Para corrigir isso, usamos *janelas*: funções matemáticas que suavizam o início e o fim do sinal, como se estivéssemos diminuindo o volume gradualmente."),
                        tags$p("🪟 Exemplo: a janela de Hanning começa em zero, sobe suavemente até o meio e desce de novo até zero."),
                        tags$p("🔍 Se você NÃO usar janelas (janela 'Retangular'), seu espectro pode mostrar frequências falsas ou artefatos."),
                        tags$p("✅ Recomendação: use a janela de *Hanning* para sinais EMG, pois ela reduz bem os efeitos de borda sem distorcer muito a forma das frequências."),
                        tags$table(
                          style = "width:100%; font-size: 14px;",
                          tags$thead(
                            tags$tr(
                              tags$th("🪟 Tipo de Janela"),
                              tags$th("Características"),
                              tags$th("Quando Usar")
                            )
                          ),
                          tags$tbody(
                            tags$tr(tags$td("Retangular"), tags$td("Sem suavização, cortes abruptos"), tags$td("Somente para testes teóricos")),
                            tags$tr(tags$td("Hanning"), tags$td("Transição suave nas bordas, boa redução de vazamento"), tags$td("✅ Ideal para sinais EMG")),
                            tags$tr(tags$td("Hamming"), tags$td("Semelhante à Hanning, um pouco mais 'firme' nas bordas"), tags$td("Boa para sinais ruidosos")),
                            tags$tr(tags$td("Blackman"), tags$td("Atenuação máxima nas bordas, janela mais 'lenta'"), tags$td("Para sinais com muitos picos ou pouca repetição"))
                          )
                        )
                      ),
                      
                      tags$details(
                        tags$summary(HTML("📊 <b>Frequência Máxima para Exibição (Hz)</b> – <i>Como ajustar?</i>")),
                        tags$p("Este valor define o limite direito do gráfico de espectro. Serve para focar em faixas específicas."),
                        tags$p("💡 Exemplo: a maior parte da atividade relevante do EMG está entre 20 e 300 Hz. Se você quer ignorar ruído acima disso, defina o valor máximo como 400 ou 500 Hz."),
                        tags$p("🔧 Isso não afeta os cálculos da FFT – apenas a forma como o gráfico é exibido.")
                      )
                    )
                    ,
                    actionButton("analyze_fft_btn", "Analisar com FFT (Fast Fourier Transform", icon = icon("chart-line"),
                                 class = "btn-primary")
                )
              ),
              
              # Área para visualização dos resultados
              conditionalPanel(
                condition = "input.analyze_fft_btn > 0",
                fluidRow(
                  box(title = "Sinal Original", width = 6, status = "info", solidHeader = TRUE,
                      plotlyOutput("original_signal_plot") %>% withSpinner()),
                  box(title = "Espectro de Frequência", width = 6, status = "warning", solidHeader = TRUE,
                      plotlyOutput("fft_plot") %>% withSpinner())
                ),
                fluidRow(
                  box(title = "Resultados da Análise de Fourier", width = 12, status = "success", solidHeader = TRUE,
                      DT::dataTableOutput("fft_results_table") %>% withSpinner())
                )
              )
      ),
      
      # ABA: GERADOR DE SENOIDES ----------------------------------
      # ---------------- ABA: GERADOR DE SENOIDES ----------------
      tabItem(tabName = "sine_generator",
              fluidRow(
                column(4,
                       box(title = "Configuração de Componentes", width = 12,
                           status = "primary", solidHeader = TRUE,
                           
                           selectInput("component_type", "Tipo de Componente:",
                                       choices = c("Constante" = "const", "Senoide" = "sine"), selected = "const"),
                           
                           conditionalPanel(
                             condition = "input.component_type == 'const'",
                             sliderInput("const_value", "Valor da Constante:",
                                         min = -5, max = 5, value = 1, step = 0.1)
                           ),
                           
                           conditionalPanel(
                             condition = "input.component_type == 'sine'",
                             selectInput("num_sine_components", "Número de Senoides (1 a 100):",
                                         choices = 1:100, selected = 1),
                             uiOutput("sine_config_ui")
                           ),
                           
                           hr(),
                           
                           sliderInput("sine_total_time", "Duração Total (s):", min = 1, max = 10, value = 2, step = 1),
                           sliderInput("sine_sampling_rate", "Frequência de Amostragem (Hz):", min = 500, max = 5000, value = 2000, step = 500),
                           
                          
                       )
                ),
                
                column(8,
                       box(title = "Visualização do Sinal Gerado", width = 12, status = "info", solidHeader = TRUE,
                           plotlyOutput("sine_plot") %>% withSpinner())
                )
              ),
              
              fluidRow(
                column(12,
                       downloadButton("download_sine_btn", "Download CSV", class = "btn-success")
                )
              )
      )
      
    )
  )
)

# =============================  SERVER  ======================================
server <- function(input, output, session) {
  
  # Valores reativos para armazenar dados EMG gerados
  emg_data <- reactiveVal(NULL)
  abnormality_markers <- reactiveVal(NULL)
  
  # Inicializar dados EMG para garantir que o botão de download esteja ativo imediatamente
  observe({
    # Gerar dados iniciais
    if (is.null(emg_data())) {
      # Criar configuração de EMG normal (agora é o padrão)
      normal_emg <- list(
        enabled = TRUE,
        amplitude = 0.6,
        amplitude_sd = 0.1,
        interval = 1.5,
        interval_sd = 0.2,
        duration = 0.15,
        duration_sd = 0.02,
        start_time = 0.5
      )
      
      # Criar configuração de fasciculações (agora desativada por padrão)
      fasciculations <- list(
        enabled = FALSE,
        amplitude = 0.8,
        amplitude_sd = 0.1,
        interval = 2,
        interval_sd = 0.3,
        duration = 0.2,
        duration_sd = 0.03,
        start_time = 1
      )
      
      # Gerar sinal EMG inicial
      result <- generate_emg_signal(
        total_time = 10,
        sampling_rate = 2000,
        baseline_noise = 0.05,
        normal_emg = normal_emg,
        fasciculations = fasciculations,
        fibrillations = list(enabled = FALSE),
        complex_repetitive_discharges = list(enabled = FALSE),
        positive_sharp_waves = list(enabled = FALSE),
        abnormal_motor_units = list(enabled = FALSE),
        reduced_recruitment = list(enabled = FALSE)
      )
      
      # Armazenar os dados gerados
      emg_data(result)
      
      # Gerar marcadores para anormalidades
      markers <- mark_abnormalities(
        emg_data = result,
        normal_emg = normal_emg,
        fasciculations = fasciculations,
        fibrillations = list(enabled = FALSE),
        complex_repetitive_discharges = list(enabled = FALSE),
        positive_sharp_waves = list(enabled = FALSE),
        abnormal_motor_units = list(enabled = FALSE),
        reduced_recruitment = list(enabled = FALSE)
      )
      
      abnormality_markers(markers)
    }
  }, priority = 1000)  # Alta prioridade para executar primeiro
  
  # Expressão reativa para regenerar o sinal EMG sempre que qualquer entrada mudar
  observe({
    # Coletar valores dos inputs
    total_time <- input$total_time
    sampling_rate <- input$sampling_rate
    baseline_noise <- input$baseline_noise
    
    # Coletar configurações para EMG normal
    normal_emg <- list(
      enabled = input$normal_emg_enabled,
      amplitude = input$normal_amplitude,
      amplitude_sd = input$normal_amplitude_sd,
      interval = input$normal_interval,
      interval_sd = input$normal_interval_sd,
      duration = input$normal_duration,
      duration_sd = input$normal_duration_sd,
      start_time = input$normal_start
    )
    
    # Coletar configurações para fasciculações
    fasciculations <- list(
      enabled = input$fascic_enabled,
      amplitude = input$fascic_amplitude,
      amplitude_sd = input$fascic_amplitude_sd,
      interval = input$fascic_interval,
      interval_sd = input$fascic_interval_sd,
      duration = input$fascic_duration,
      duration_sd = input$fascic_duration_sd,
      start_time = input$fascic_start
    )
    
    # Coletar configurações para fibrilações
    fibrillations <- list(
      enabled = input$fib_enabled,
      amplitude = input$fib_amplitude,
      amplitude_sd = input$fib_amplitude_sd,
      interval = input$fib_interval,
      interval_sd = input$fib_interval_sd,
      duration = input$fib_duration,
      duration_sd = input$fib_duration_sd,
      start_time = input$fib_start
    )
    
    # Coletar configurações para Descargas Repetitivas Complexas
    complex_repetitive_discharges <- list(
      enabled = input$crd_enabled,
      amplitude = input$crd_amplitude,
      amplitude_sd = input$crd_amplitude_sd,
      interval = input$crd_interval,
      interval_sd = input$crd_interval_sd,
      duration = input$crd_duration,
      duration_sd = input$crd_duration_sd,
      frequency = input$crd_frequency,
      frequency_sd = input$crd_frequency_sd,
      start_time = input$crd_start
    )
    
    # Coletar configurações para Ondas Agudas Positivas
    positive_sharp_waves <- list(
      enabled = input$psw_enabled,
      amplitude = input$psw_amplitude,
      amplitude_sd = input$psw_amplitude_sd,
      interval = input$psw_interval,
      interval_sd = input$psw_interval_sd,
      duration = input$psw_duration,
      duration_sd = input$psw_duration_sd,
      start_time = input$psw_start
    )
    
    # Coletar configurações para Unidades Motoras Anormais
    abnormal_motor_units <- list(
      enabled = input$amu_enabled,
      amplitude = input$amu_amplitude,
      amplitude_sd = input$amu_amplitude_sd,
      interval = input$amu_interval,
      interval_sd = input$amu_interval_sd,
      duration = input$amu_duration,
      duration_sd = input$amu_duration_sd,
      phases = input$amu_phases,
      phases_sd = input$amu_phases_sd,
      start_time = input$amu_start
    )
    
    # Coletar configurações para Recrutamento Reduzido
    reduced_recruitment <- list(
      enabled = input$rr_enabled,
      motor_units = input$rr_motor_units,
      amplitude = input$rr_amplitude,
      amplitude_sd = input$rr_amplitude_sd,
      firing_rate = input$rr_firing_rate,
      firing_rate_sd = input$rr_firing_rate_sd,
      jitter = input$rr_jitter,
      duration = input$rr_duration,
      motor_unit_duration = input$rr_motor_unit_duration,
      motor_unit_duration_sd = input$rr_motor_unit_duration_sd,
      start_time = input$rr_start
    )
    
    # Gerar novo sinal EMG
    result <- generate_emg_signal(
      total_time = total_time,
      sampling_rate = sampling_rate,
      baseline_noise = baseline_noise,
      normal_emg = normal_emg,
      fasciculations = fasciculations,
      fibrillations = fibrillations,
      complex_repetitive_discharges = complex_repetitive_discharges,
      positive_sharp_waves = positive_sharp_waves,
      abnormal_motor_units = abnormal_motor_units,
      reduced_recruitment = reduced_recruitment
    )
    
    # Atualizar valores reativos
    emg_data(result)
    
    # Gerar marcadores para anormalidades
    markers <- mark_abnormalities(
      emg_data = result,
      normal_emg = normal_emg,
      fasciculations = fasciculations,
      fibrillations = fibrillations,
      complex_repetitive_discharges = complex_repetitive_discharges,
      positive_sharp_waves = positive_sharp_waves,
      abnormal_motor_units = abnormal_motor_units,
      reduced_recruitment = reduced_recruitment
    )
    
    abnormality_markers(markers)
  })
  
  # Plotar o sinal EMG
  output$emg_plot <- renderPlotly({
    req(emg_data())
    
    data <- emg_data()
    markers <- abnormality_markers()
    
    # Criar o gráfico base com o sinal EMG
    p <- plot_ly(x = data$time, y = data$signal, type = "scatter", mode = "lines",
                 line = list(color = "blue", width = 1), name = "Sinal EMG") %>%
      layout(
        title = "Sinal de EMG Simulado",
        xaxis = list(title = "Tempo (s)"),
        yaxis = list(title = "Amplitude (mV)"),
        hovermode = "closest"
      )
    
    # Adicionar marcadores se existirem
    if (!is.null(markers) && nrow(markers) > 0) {
      # Definir cores para cada tipo de anormalidade
      marker_colors <- c(
        "Normal_EMG" = "green",
        "Fasciculation" = "red",
        "Fibrillation" = "orange",
        "CRD" = "purple",
        "PSW" = "brown",
        "AMU" = "magenta",
        "RR_start" = "cyan",
        "RR_end" = "cyan"
      )
      
      # Traduzir tipos para legenda
      type_translations <- c(
        "Normal_EMG" = "Contração Normal",
        "Fasciculation" = "Fasciculação",
        "Fibrillation" = "Fibrilação",
        "CRD" = "Descarga Repetitiva Complexa",
        "PSW" = "Onda Aguda Positiva",
        "AMU" = "Potencial Polifásico (UM Anormal)",
        "RR_start" = "Início do Recrutamento Reduzido",
        "RR_end" = "Fim do Recrutamento Reduzido"
      )
      
      # Adicionar cada tipo de marcador separadamente para a legenda correta
      for (type in unique(markers$type)) {
        type_markers <- markers[markers$type == type, ]
        
        # Determinar forma do marcador
        marker_symbol <- ifelse(type %in% c("RR_start", "RR_end"), "triangle-down", "circle")
        
        # Adicionar ao gráfico
        p <- p %>% add_trace(
          x = type_markers$time,
          y = type_markers$y,
          type = "scatter",
          mode = "markers",
          marker = list(
            color = marker_colors[type],
            size = 8,
            symbol = marker_symbol
          ),
          name = type_translations[type],
          showlegend = TRUE
        )
      }
    }
    
    return(p)
  })
  
  # Exibir tabela de anormalidades
  output$abnormality_table <- renderDT({
    req(abnormality_markers())
    
    # Se chegou aqui, temos marcadores
    markers <- abnormality_markers()
    
    # Traduzir tipos de anormalidades
    type_translations <- c(
      "Normal_EMG" = "Contração Normal",
      "Fasciculation" = "Fasciculação",
      "Fibrillation" = "Fibrilação",
      "CRD" = "Descarga Repetitiva Complexa",
      "PSW" = "Onda Aguda Positiva",
      "AMU" = "Potencial Polifásico (UM Anormal)",
      "RR_start" = "Início do Recrutamento Reduzido",
      "RR_end" = "Fim do Recrutamento Reduzido"
    )
    
    # Arredondar valores de tempo para 2 casas decimais
    markers$time <- round(markers$time, 2)
    
    # Traduzir os tipos
    markers$translated_type <- type_translations[markers$type]
    
    # Formatar a tabela
    datatable(
      markers[, c("time", "translated_type")],
      colnames = c("Tempo (s)" = "time", "Tipo de Anormalidade" = "translated_type"),
      options = list(
        pageLength = 10,
        dom = 'ftip',
        ordering = TRUE,
        language = list(
          "search" = "Buscar:",
          "lengthMenu" = "Mostrar _MENU_ registros",
          "info" = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          "paginate" = list(
            "first" = "Primeiro",
            "last" = "Último", 
            "next" = "Próximo",
            "previous" = "Anterior"
          ),
          "zeroRecords" = "Nenhuma anormalidade encontrada",
          "infoEmpty" = "Mostrando 0 a 0 de 0 registros",
          "infoFiltered" = "(filtrado de _MAX_ registros no total)"
        )
      )
    )
  })
  
  # Handler de download para exportação CSV
  output$download_btn <- downloadHandler(
    filename = function() {
      paste("simulacao_emg_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Garantir que temos dados para exportar, mesmo que estejam vazios
      if (is.null(emg_data())) {
        # Se não tiver dados, criar um pequeno conjunto de dados vazio
        export_data <- data.frame(
          tempo_segundos = numeric(0),
          amplitude_mV = numeric(0),
          evento = character(0)
        )
        write.csv(export_data, file, row.names = FALSE)
        return()
      }
      
      data <- emg_data()
      
      # Combinar tempo e sinal em um data frame
      export_data <- data.frame(
        tempo_segundos = data$time,
        amplitude_mV = data$signal
      )
      
      # Adicionar marcadores a uma coluna separada, se disponível
      if (!is.null(abnormality_markers()) && nrow(abnormality_markers()) > 0) {
        markers <- abnormality_markers()
        
        # Traduzir tipos de anormalidades
        type_translations <- c(
          "Normal_EMG" = "Contração Normal",
          "Fasciculation" = "Fasciculação",
          "Fibrillation" = "Fibrilação",
          "CRD" = "Descarga Repetitiva Complexa",
          "PSW" = "Onda Aguda Positiva",
          "AMU" = "Potencial Polifásico (UM Anormal)",
          "RR_start" = "Início do Recrutamento Reduzido",
          "RR_end" = "Fim do Recrutamento Reduzido"
        )
        
        # Inicializar coluna de evento com strings vazias
        export_data$evento <- ""
        
        # Adicionar marcadores à coluna de evento
        for (i in 1:nrow(markers)) {
          # Encontrar o índice mais próximo do tempo do marcador
          idx <- which.min(abs(export_data$tempo_segundos - markers$time[i]))
          export_data$evento[idx] <- type_translations[markers$type[i]]
        }
      } else {
        # Adicionar coluna de evento vazia se não houver marcadores
        export_data$evento <- ""
      }
      
      # Escrever no arquivo CSV
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # --- FUNÇÕES REATIVAS PARA TRANSFORMADA DE FOURIER ---
  
  # Armazenar dados carregados do arquivo
  emg_file_data <- reactiveVal(NULL)
  
  # Armazenar resultados da FFT
  fft_results <- reactiveVal(NULL)
  
  # Carregar dados do arquivo CSV
  observeEvent(input$emg_file, {
    req(input$emg_file)
    
    # Ler o arquivo CSV
    file_data <- read.csv(input$emg_file$datapath, stringsAsFactors = FALSE)
    
    # Verificar se o arquivo tem a estrutura esperada
    req(all(c("tempo_segundos", "amplitude_mV") %in% colnames(file_data)))
    
    # Armazenar os dados carregados
    emg_file_data(file_data)
    
    # Habilitar o botão de análise
    shinyjs::enable("analyze_fft_btn")
  })
  
  # Analisar dados com FFT
  observeEvent(input$analyze_fft_btn, {
    req(emg_file_data())
    
    # Obter dados
    file_data <- emg_file_data()
    signal <- file_data$amplitude_mV
    time <- file_data$tempo_segundos
    
    # Verificar frequência de amostragem fornecida
    sampling_rate <- input$sampling_rate_input
    
    # Aplicar transformada de Fourier
    fft_result <- calculate_fft(
      signal = signal,
      sampling_rate = sampling_rate,
      window_type = input$window_function,
      remove_dc = input$remove_dc
    )
    
    # Encontrar picos significativos
    peaks <- find_peaks(fft_result$freq, fft_result$magnitude)
    
    # Calcular frequência média ponderada
    if (nrow(peaks) > 0) {
      mean_freq <- sum(peaks$frequency * peaks$magnitude) / sum(peaks$magnitude)
    } else {
      mean_freq <- NA
    }
    
    # Calcular potência total no sinal (soma dos quadrados das magnitudes)
    total_power <- sum(fft_result$magnitude^2)
    
    # Calcular frequência média (centro de massa do espectro)
    weighted_mean_freq <- sum(fft_result$freq * fft_result$magnitude) / sum(fft_result$magnitude)
    
    # Calcular frequência mediana (divide o espectro em duas partes iguais)
    cumulative_power <- cumsum(fft_result$magnitude)
    median_freq_idx <- which(cumulative_power >= cumulative_power[length(cumulative_power)]/2)[1]
    median_freq <- fft_result$freq[median_freq_idx]
    
    # Armazenar resultados
    fft_results(list(
      time = time,
      signal = signal,
      freq = fft_result$freq,
      magnitude = fft_result$magnitude,
      peaks = peaks,
      mean_freq = mean_freq,
      median_freq = median_freq,
      weighted_mean_freq = weighted_mean_freq,
      total_power = total_power
    ))
  })
  
  # Plotar sinal original
  output$original_signal_plot <- renderPlotly({
    req(fft_results())
    
    results <- fft_results()
    
    plot_ly() %>%
      add_trace(
        x = results$time,
        y = results$signal,
        type = "scatter",
        mode = "lines",
        line = list(color = "blue", width = 1),
        name = "Sinal EMG"
      ) %>%
      layout(
        title = "Sinal EMG no Domínio do Tempo",
        xaxis = list(title = "Tempo (s)"),
        yaxis = list(title = "Amplitude (mV)"),
        hovermode = "closest"
      )
  })
  
  # Plotar espectro de frequência (FFT)
  output$fft_plot <- renderPlotly({
    req(fft_results())
    
    results <- fft_results()
    
    # Limitar à frequência máxima selecionada
    max_freq <- input$max_freq_display
    idx <- which(results$freq <= max_freq)
    
    plot_ly() %>%
      add_trace(
        x = results$freq[idx],
        y = results$magnitude[idx],
        type = "scatter",
        mode = "lines",
        line = list(color = "red", width = 1),
        name = "Espectro de Frequência"
      ) %>%
      layout(
        title = "Transformada de Fourier (Espectro de Frequência)",
        xaxis = list(title = "Frequência (Hz)"),
        yaxis = list(title = "Magnitude"),
        hovermode = "closest"
      )
  })
  
  # Exibir tabela de resultados
  output$fft_results_table <- renderDT({
    req(fft_results())
    
    results <- fft_results()
    
    # Preparar dados para a tabela
    summary_data <- data.frame(
      Métrica = c(
        "Frequência Média Ponderada",
        "Frequência Mediana",
        "Potência Total do Sinal",
        "Número de Picos Significativos"
      ),
      `O que é` = c(
        "É a média das frequências presentes no sinal, ponderada pela intensidade de cada componente (magnitude). Cada frequência representa a quantidade de oscilações por segundo no sinal. Frequência média ponderada = (Σ[freq × magnitude]) / (Σ[magnitude])",
        "É a frequência onde metade da energia total do sinal está abaixo e metade acima. Divide o espectro em duas partes iguais. (Mostra se o sinal tende mais para frequências baixas ou altas.)",
        "É a soma dos quadrados das magnitudes da transformada de Fourier. Reflete a energia total do sinal (em mV²), ou seja, o quanto o músculo está ativado ao longo do tempo.",
        "Quantidade de picos significativos no espectro de frequência. (Cada pico representa uma frequência dominante ou recorrente no sinal EMG.)"
      ),
      Fórmula = c(
        "Σ(frequência × magnitude) / Σ(magnitude)",
        "Frequência onde a potência acumulada atinge 50% do total",
        "Σ(magnitude²) para todas as frequências",
        "Picos locais com magnitude > 0.005 (ajustável)"
      ),
      `Possível Interpretação na ELA (Hipótese)` = c(
        "Valores mais baixos podem indicar perda de fibras rápidas e predominância de disparos lentos. Frequência média tende a cair conforme a doença progride.",
        "Pode estar reduzida em ELA, refletindo menor variabilidade no recrutamento motor. (Menos diversidade de frequências no sinal.)",
        "Pode estar reduzida na ELA, indicando menor ativação muscular por perda de unidades motoras. (Menos energia gerada.)",
        "Padrão irregular em ELA, com menos picos significativos ou picos mais amplos devido ao recrutamento reduzido e sincronização comprometida."
      ),
      Valor = c(
        sprintf("%.2f Hz", results$weighted_mean_freq),
        sprintf("%.2f Hz", results$median_freq),
        sprintf("%.5f mV²", results$total_power),
        nrow(results$peaks)
      )
    )
    
    # Adicionar frequências dos picos se existirem
    if (nrow(results$peaks) > 0) {
      peak_data <- data.frame(
        Métrica = paste("Pico", 1:nrow(results$peaks), "Frequência"),
        `O que é` = rep("Componente de frequência dominante no sinal EMG, pode refletir atividade coordenada de unidades motoras.", nrow(results$peaks)),
        Fórmula = rep("Identificado como máximo local na magnitude do espectro", nrow(results$peaks)),
        `Possível Interpretação na ELA (Hipótese)` = rep("Picos mais largos e menos definidos podem indicar dessincronização de unidades motoras, comum em ELA.", nrow(results$peaks)),
        Valor = sprintf("%.2f Hz (Magnitude: %.5f)", 
                        results$peaks$frequency, 
                        results$peaks$magnitude)
      )
      
      summary_data <- rbind(summary_data, peak_data)
    }
    

    
    summary_data <- rbind(summary_data)#, fft_note)
    
    # Formatar a tabela
    datatable(
      summary_data,
      options = list(
        pageLength = 15,
        dom = 'tip',
        ordering = FALSE,
        language = list(
          "search" = "Buscar:",
          "lengthMenu" = "Mostrar _MENU_ registros",
          "info" = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          "paginate" = list(
            "first" = "Primeiro",
            "last" = "Último", 
            "next" = "Próximo",
            "previous" = "Anterior"
          ),
          "zeroRecords" = "Nenhuma métrica encontrada",
          "infoEmpty" = "Mostrando 0 a 0 de 0 registros"
        ),
        columnDefs = list(
          list(width = '20%', targets = 0),
          list(width = '25%', targets = 1),
          list(width = '15%', targets = 2),
          list(width = '30%', targets = 3),
          list(width = '10%', targets = 4)
        )
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center; font-style: italic;',
        'Nota: As interpretações para ELA são hipotéticas e baseadas em literatura. A análise clínica deve ser realizada por profissionais qualificados.'
      )
    ) %>%
      formatStyle(columns = 0:4, fontSize = '90%')
  })
  
  # Armazenar os dados gerados
  sine_signal_data <- reactiveVal(NULL)
  
  # Criar UI dinâmica para os componentes de senoides
  output$sine_config_ui <- renderUI({
    req(input$component_type == "sine")
    req(input$num_sine_components)
    n <- as.integer(input$num_sine_components)
    lapply(1:n, function(i) {
      tagList(
        hr(),
        h4(paste("Senoide", i)),
        sliderInput(paste0("sine_amp_", i), paste("Amplitude A", i),
                    min = 0, max = 2, value = 1, step = 0.1),
        sliderInput(paste0("sine_freq_", i), paste("Frequência F", i, "(Hz)"),
                    min = 1, max = 500, value = i * 10, step = 1)
      )
    })
  })
  
  # Gera o sinal composto
  observe({
    total_time <- input$sine_total_time
    sampling_rate <- input$sine_sampling_rate
    time <- seq(0, total_time, by = 1/sampling_rate)
    signal <- rep(0, length(time))
    evento <- rep("", length(time))
    
    if (input$component_type == "const") {
      signal <- signal + input$const_value
    } else if (input$component_type == "sine") {
      n <- as.integer(input$num_sine_components)
      for (i in 1:n) {
        A <- input[[paste0("sine_amp_", i)]]
        F <- input[[paste0("sine_freq_", i)]]
        if (!is.null(A) && !is.null(F)) {
          component <- A * sin(2 * pi * F * time)
          signal <- signal + component
        }
      }
    }
    
    df <- data.frame(
      tempo_segundos = time,
      amplitude_mV = signal,
      evento = evento
    )
    sine_signal_data(df)
  })
  
  # Plotar o sinal
  output$sine_plot <- renderPlotly({
    req(sine_signal_data())
    df <- sine_signal_data()
    plot_ly(df, x = ~tempo_segundos, y = ~amplitude_mV, type = 'scatter', mode = 'lines',
            line = list(color = 'blue')) %>%
      layout(title = "Sinal Gerado", xaxis = list(title = "Tempo (s)"),
             yaxis = list(title = "Amplitude (mV)"))
  })
  
  # Botão de download
  output$download_sine_btn <- downloadHandler(
    filename = function() {
      paste0("sinal_senoidal_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(sine_signal_data(), file, row.names = FALSE)
    }
  )
  
  
}

# Executar a aplicação
shinyApp(ui = ui, server = server)