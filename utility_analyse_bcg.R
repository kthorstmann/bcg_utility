# =============================================================================
# Utility-Analyse nach dem Brogden-Cronbach-Gleser-Modell
# =============================================================================
#
# Autor:
#   Prof. Dr. Kai T. Horstmann
#   Universität Siegen, Psychologische Diagnostik
#
# Zweck:
#   Dieses Skript berechnet den ökonomischen Nutzen von Personalauswahl-
#   verfahren auf Basis des BCG-Modells (Brogden, 1949; Cronbach & Gleser,
#   1965). Es enthält kommentierte Einzelschritte und eine Funktion für
#   flexible Anwendung.
#
# Zentrale Referenzen:
#   - Brogden, H. E. (1949). When testing pays off. Personnel Psychology.
#   - Cronbach, L. J., & Gleser, G. C. (1965). Psychological tests and
#     personnel decisions (2nd ed.). University of Illinois Press.
#   - Schmidt, F. L., & Hunter, J. E. (1998). The validity and utility of
#     selection methods in personnel psychology. Psychological Bulletin.
#   - Sackett, P. R., et al. (2022). Revisiting meta-analytic estimates of
#     validity in personnel selection. Journal of Applied Psychology.
#   - Taylor, H. C., & Russell, J. T. (1939). The relationship of validity
#     coefficients to the practical effectiveness of tests in selection.
#     Journal of Applied Psychology.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Die BCG-Formel
# -----------------------------------------------------------------------------
#
# Die Kernformel lautet:
#
#   ΔU = N × Δr × SDy × Z̄s
#
# Dabei ist:
#   N    = Anzahl der Einstellungen pro Jahr
#   Δr   = Validitätsdifferenz zwischen neuem und altem Verfahren
#   SDy  = Standardabweichung der Arbeitsleistung in Geldeinheiten (€)
#   Z̄s   = Mittlerer standardisierter Prädiktorscore der selektierten
#           Bewerber*innen (abhängig von der Selektionsquote)
#
# ΔU gibt den jährlichen Produktivitätsgewinn in Euro an, der durch den
# Wechsel von einem weniger validen zu einem valideren Auswahlverfahren
# entsteht.


# -----------------------------------------------------------------------------
# 2. Berechnung von Z̄s (Naylor-Shine-Ansatz)
# -----------------------------------------------------------------------------
#
# Z̄s ist der mittlere Prädiktorscore derjenigen Bewerber*innen, die
# oberhalb des Selektions-Cutoffs liegen. Bei Normalverteilung des
# Prädiktors ergibt sich:
#
#   Z̄s = φ(z_c) / SR
#
# wobei:
#   z_c = Cutoff-Wert auf der z-Skala (= qnorm(1 - SR))
#   φ() = Dichte der Standardnormalverteilung (= dnorm())
#   SR  = Selektionsquote (Anteil der Eingestellten an allen Bewerber*innen)
#
# Beispiel: Bei einer Selektionsquote von 30 % werden nur die besten 30 %
# eingestellt. Der mittlere z-Score dieser Top-30-% liegt bei Z̄s ≈ 1.17.

berechne_zs <- function(sr) {
  # sr: Selektionsquote (Wert zwischen 0 und 1)
  #
  # Gibt den mittleren standardisierten Prädiktorscore der selektierten

  # Bewerber*innen zurück.
  
  if (sr <= 0 | sr > 1) stop("Selektionsquote muss zwischen 0 und 1 liegen.")
  
  # Cutoff auf der z-Skala: Welcher z-Wert trennt die oberen SR % ab?
  z_cutoff <- qnorm(1 - sr)
  
  # Ordinate (Dichte) der Normalverteilung am Cutoff
  ordinate <- dnorm(z_cutoff)
  
  # Z̄s = Ordinate / Selektionsquote
  zs <- ordinate / sr
  
  return(zs)
}

# Demonstration: Z̄s für verschiedene Selektionsquoten
cat("--- Z̄s für verschiedene Selektionsquoten ---\n")
sr_werte <- c(0.05, 0.10, 0.20, 0.30, 0.50, 0.70, 0.90)
zs_werte <- sapply(sr_werte, berechne_zs)
print(data.frame(
  Selektionsquote = sr_werte,
  Zs = round(zs_werte, 3)
))
cat("\nInterpretation: Je selektiver (kleinere SR), desto höher Z̄s,\n")
cat("d.h. desto bessere Bewerber*innen werden im Mittel eingestellt.\n\n")


# -----------------------------------------------------------------------------
# 3. SDy – Die Standardabweichung der Leistung in Geldeinheiten
# -----------------------------------------------------------------------------
#
# SDy quantifiziert, wie viel Euro Unterschied eine Standardabweichung
# in der Arbeitsleistung ausmacht. Eine Person, die eine SD über dem
# Durchschnitt liegt, produziert SDy Euro mehr Wert pro Jahr.
#
# Gängige Schätzungen:
#   - Schmidt & Hunter (1982): SDy ≥ 40 % des Jahresgehalts
#   - Becker & Huselid (1992): 74–100 % in Einzelhandelsfilialen
#   - Hunter, Schmidt & Judiesch (1990): SDy als % des Outputs steigt
#     mit Jobkomplexität:
#       - Einfache Arbeit:     19 %
#       - Mittlere Komplexität: 32 %
#       - Hohe Komplexität:     48 %
#
# Die 40-%-Regel ist konservativ und wird häufig als Untergrenze verwendet.

berechne_sdy <- function(jahresgehalt, sdy_prozent = 40) {
  # jahresgehalt: Brutto-Jahresgehalt in Euro
  # sdy_prozent:  SDy als Prozentsatz des Gehalts (Default: 40 %)
  
  sdy <- jahresgehalt * sdy_prozent / 100
  return(sdy)
}

# Beispiel
gehalt <- 55000
sdy <- berechne_sdy(gehalt, sdy_prozent = 40)
cat(sprintf("Bei einem Gehalt von %s € und SDy = 40%%:\n", 
            format(gehalt, big.mark = ".")))
cat(sprintf("  SDy = %s €\n", format(sdy, big.mark = ".")))
cat("  Das heißt: Eine Person, die eine SD über dem Durchschnitt liegt,\n")
cat("  produziert ca. 22.000 € mehr Wert pro Jahr.\n\n")


# -----------------------------------------------------------------------------
# 4. Validitätskoeffizienten: Welche Verfahren funktionieren?
# -----------------------------------------------------------------------------
#
# Die folgenden Validitätskoeffizienten stammen aus Schmidt & Hunter (1998)
# und Sackett et al. (2022). Die wahren Werte liegen vermutlich zwischen
# beiden Schätzungen.

validitaeten <- data.frame(
  Verfahren = c(
    "Graphologie",
    "Berufserfahrung (Jahre)",
    "Unstrukturiertes Interview",
    "Gewissenhaftigkeit",
    "Assessment Center",
    "Integritätstest",
    "Strukturiertes Interview",
    "Arbeitsprobe",
    "GMA (Allg. kognitive Fähigkeit)",
    "GMA + Strukturiertes Interview"
  ),
  r_Schmidt_Hunter_1998 = c(.02, .18, .38, .31, .37, .41, .51, .54, .51, .63),
  r_Sackett_2022 = c(NA, .07, .19, .12, .27, .31, .42, .33, .31, NA),
  stringsAsFactors = FALSE
)

cat("--- Validitätskoeffizienten nach Verfahren ---\n")
print(validitaeten, row.names = FALSE)
cat("\nAnmerkung: Sackett et al. (2022) korrigierten viele Werte nach unten.\n")
cat("Die Werte von Schmidt & Hunter (1998) gelten als Obergrenze.\n\n")


# -----------------------------------------------------------------------------
# 5. Die BCG-Berechnung Schritt für Schritt
# -----------------------------------------------------------------------------

cat("=== Beispielberechnung: BCG-Modell ===\n\n")

# Parameter definieren
N <- 50            # Einstellungen pro Jahr
gehalt <- 55000    # Jahresgehalt in Euro
sdy_pct <- 40      # SDy als % des Gehalts
r_alt <- 0.15      # Validität des bisherigen Verfahrens (z. B. Bauchgefühl)
r_neu <- 0.51      # Validität des neuen Verfahrens (z. B. struct. Interview)
sr <- 0.30          # Selektionsquote
tenure <- 5         # Mittlere Betriebszugehörigkeit in Jahren

cat(sprintf("  N (Einstellungen/Jahr):        %d\n", N))
cat(sprintf("  Jahresgehalt:                  %s €\n", format(gehalt, big.mark = ".")))
cat(sprintf("  SDy (%% des Gehalts):           %d %%\n", sdy_pct))
cat(sprintf("  Validität alt (r_alt):         %.2f\n", r_alt))
cat(sprintf("  Validität neu (r_neu):         %.2f\n", r_neu))
cat(sprintf("  Selektionsquote (SR):          %.2f\n", sr))
cat(sprintf("  Betriebszugehörigkeit:         %.1f Jahre\n", tenure))
cat("\n")

# Schritt 1: SDy berechnen
sdy <- berechne_sdy(gehalt, sdy_pct)
cat(sprintf("Schritt 1 – SDy = %d %% × %s € = %s €\n", 
            sdy_pct, format(gehalt, big.mark = "."), format(sdy, big.mark = ".")))

# Schritt 2: Z̄s berechnen
zs <- berechne_zs(sr)
cat(sprintf("Schritt 2 – Z̄s = φ(%.3f) / %.2f = %.3f\n", 
            qnorm(1 - sr), sr, zs))

# Schritt 3: Validitätsdifferenz
delta_r <- r_neu - r_alt
cat(sprintf("Schritt 3 – Δr = %.2f - %.2f = %.2f\n", r_neu, r_alt, delta_r))

# Schritt 4: Nutzen pro Einstellung pro Jahr
u_pro_person <- delta_r * sdy * zs
cat(sprintf("Schritt 4 – Nutzen/Einstellung/Jahr = %.2f × %s × %.3f = %s €\n",
            delta_r, format(sdy, big.mark = "."), zs,
            format(round(u_pro_person), big.mark = ".")))

# Schritt 5: Gesamtnutzen pro Jahr
u_jahr <- N * u_pro_person
cat(sprintf("Schritt 5 – Nutzen/Jahr = %d × %s € = %s €\n",
            N, format(round(u_pro_person), big.mark = "."),
            format(round(u_jahr), big.mark = ".")))

# Schritt 6: Kumulierter Nutzen über Betriebszugehörigkeit
u_gesamt <- u_jahr * tenure
cat(sprintf("Schritt 6 – Kumuliert (%s Jahre) = %s € × %.1f = %s €\n",
            tenure, format(round(u_jahr), big.mark = "."), tenure,
            format(round(u_gesamt), big.mark = ".")))
cat("\n")


# -----------------------------------------------------------------------------
# 6. Taylor-Russell-Tafeln: Fehleinstellungsquote
# -----------------------------------------------------------------------------
#
# Die Taylor-Russell-Tafeln zeigen, welcher Anteil der eingestellten
# Personen tatsächlich geeignet ist (Success Ratio), gegeben:
#   - die Validität des Verfahrens
#   - die Selektionsquote
#   - die Basisrate (Anteil Geeigneter in der Bewerberpopulation)
#
# Bei Basisrate = 50 % und Zufallsauswahl (r = 0) sind 50 % der
# Einstellungen Fehlbesetzungen. Je höher die Validität und je
# niedriger die Selektionsquote, desto besser die Trefferquote.

# Approximation der Taylor-Russell-Erfolgsquote über die bivariate
# Normalverteilung. Für eine exakte Berechnung kann das Paket 'mvtnorm'
# verwendet werden.

taylor_russell <- function(r, sr, br = 0.50) {
  # r:  Validitätskoeffizient
  # sr: Selektionsquote
  # br: Basisrate (Anteil Geeigneter in der Population, Default: 50 %)
  #
  # Gibt die Erfolgsquote (Success Ratio) zurück, d.h. den Anteil
  # der Eingestellten, die tatsächlich geeignet sind.
  
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    # Fallback: Analytische Approximation ohne mvtnorm
    z_sr <- qnorm(1 - sr)
    z_br <- qnorm(1 - br)
    # Bedingte Normalverteilung: Y | X > z_sr
    cond_mean <- r * z_sr
    cond_sd <- sqrt(1 - r^2)
    # P(Y > z_br | X > z_sr) ≈ P(Z > (z_br - cond_mean) / cond_sd)
    # Aber das ist nur die bedingte WK am Punkt z_sr, nicht über alle X > z_sr.
    # Bessere Approximation: Numerische Integration
    integrand <- function(x) {
      pnorm((r * x - z_br) / sqrt(1 - r^2)) * dnorm(x)
    }
    erfolg <- integrate(integrand, lower = z_sr, upper = Inf)$value / sr
    return(erfolg)
  }
  
  # Exakte Berechnung mit bivariater Normalverteilung
  z_sr <- qnorm(1 - sr)
  z_br <- qnorm(1 - br)
  sigma <- matrix(c(1, r, r, 1), nrow = 2)
  # P(X > z_sr UND Y > z_br)
  p_joint <- mvtnorm::pmvnorm(
    lower = c(z_sr, z_br),
    upper = c(Inf, Inf),
    corr = sigma
  )[1]
  # Erfolgsquote = P(Y > z_br | X > z_sr) = P(joint) / P(X > z_sr)
  erfolg <- p_joint / sr
  return(erfolg)
}
taylor_russell(r = .10, sr = .30, br = .10)

cat("--- Taylor-Russell-Tafeln (Basisrate = 50 %) ---\n\n")

sr_grid <- c(0.05, 0.10, 0.20, 0.30, 0.50, 0.70)
r_grid <- c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60)

tr_matrix <- outer(r_grid, sr_grid, Vectorize(function(r, sr) {
  round(taylor_russell(r, sr, br = 0.50) * 100, 1)
}))
rownames(tr_matrix) <- paste0("r=.", sprintf("%02d", r_grid * 100))
colnames(tr_matrix) <- paste0("SR=.", sprintf("%02d", sr_grid * 100))

cat("Erfolgsquote (% korrekte Einstellungen):\n")
print(tr_matrix)
cat("\nLesebeispiel: Bei r = .50 und SR = .30 sind ca.",
    tr_matrix["r=.50", "SR=.30"], "% der Eingestellten geeignet.\n")
cat("Bei Zufallsauswahl (r = .00) sind es immer 50 %.\n\n")


# Fehlbesetzungen im Vergleich
fehler_alt <- 1 - taylor_russell(r_alt, sr)
fehler_neu <- 1 - taylor_russell(r_neu, sr)
cat(sprintf("--- Fehleinstellungen im Vergleich (SR = %.2f, BR = 50%%) ---\n", sr))
cat(sprintf("  Altes Verfahren (r = %.2f): %.1f %% Fehleinstellungen (%d von %d)\n",
            r_alt, fehler_alt * 100, round(N * fehler_alt), N))
cat(sprintf("  Neues Verfahren (r = %.2f): %.1f %% Fehleinstellungen (%d von %d)\n",
            r_neu, fehler_neu * 100, round(N * fehler_neu), N))
cat(sprintf("  → %d vermiedene Fehlbesetzungen pro Jahr\n\n",
            round(N * fehler_alt) - round(N * fehler_neu)))


# -----------------------------------------------------------------------------
# 7. Verfahrensvergleich: Nutzen verschiedener Auswahlmethoden
# -----------------------------------------------------------------------------

cat("--- Jährlicher Nutzengewinn verschiedener Verfahren vs. Bauchgefühl (r = .15) ---\n\n")

vergleich <- data.frame(
  Verfahren = validitaeten$Verfahren,
  r = validitaeten$r_Schmidt_Hunter_1998
)

vergleich$Delta_r <- vergleich$r - r_alt
vergleich$Nutzen_pro_Jahr <- round(N * vergleich$Delta_r * sdy * zs)
vergleich$Nutzen_kumuliert <- round(vergleich$Nutzen_pro_Jahr * tenure)
vergleich$Fehlquote_pct <- round(
  sapply(vergleich$r, function(r) (1 - taylor_russell(r, sr)) * 100), 1
)

print(vergleich, row.names = FALSE)
cat("\n")


# -----------------------------------------------------------------------------
# 8. Wrapper-Funktion: utility_bcg()
# -----------------------------------------------------------------------------
#
# Diese Funktion bündelt alle Berechnungen und gibt ein übersichtliches
# Ergebnisobjekt zurück.

utility_bcg <- function(
    n_einstellungen,
    jahresgehalt,
    r_alt,
    r_neu,
    selektionsquote,
    sdy_prozent = 40,
    betriebszugehoerigkeit = 5,
    basisrate = 0.50,
    verfahrenskosten_pro_person = 0
) {
  # ---------------------------------------------------------------------------
  # Argumente:
  #   n_einstellungen:          Anzahl Einstellungen pro Jahr
  #   jahresgehalt:             Brutto-Jahresgehalt in Euro
  #   r_alt:                    Validität des bisherigen Verfahrens
  #   r_neu:                    Validität des neuen Verfahrens
  #   selektionsquote:          Anteil Eingestellter an Bewerber*innen (0–1)
  #   sdy_prozent:              SDy als % des Gehalts (Default: 40)
  #   betriebszugehoerigkeit:   Mittlere Verweildauer in Jahren (Default: 5)
  #   basisrate:                Anteil Geeigneter in Population (Default: 0.50)
  #   verfahrenskosten_pro_person: Kosten des neuen Verfahrens pro Person in €
  #                                (z. B. Testlizenzen, AC-Durchführung)
  #
  # Rückgabe:
  #   Liste mit allen Zwischen- und Endergebnissen
  # ---------------------------------------------------------------------------
  
  # Validierung
  stopifnot(
    n_einstellungen > 0,
    jahresgehalt > 0,
    r_alt >= 0, r_alt <= 1,
    r_neu >= 0, r_neu <= 1,
    selektionsquote > 0, selektionsquote <= 1,
    sdy_prozent > 0,
    betriebszugehoerigkeit > 0,
    basisrate > 0, basisrate < 1
  )
  
  # Berechnungen
  sdy <- jahresgehalt * sdy_prozent / 100
  zs <- berechne_zs(selektionsquote)
  delta_r <- r_neu - r_alt
  
  # Brutto-Nutzen
  nutzen_pro_person_jahr <- delta_r * sdy * zs
  nutzen_jahr_brutto <- n_einstellungen * nutzen_pro_person_jahr
  nutzen_kumuliert_brutto <- nutzen_jahr_brutto * betriebszugehoerigkeit
  
  # Verfahrenskosten (Anzahl zu testender Personen = N / SR)
  n_bewerber <- n_einstellungen / selektionsquote
  kosten_verfahren_jahr <- n_bewerber * verfahrenskosten_pro_person
  
  # Netto-Nutzen
  nutzen_jahr_netto <- nutzen_jahr_brutto - kosten_verfahren_jahr
  nutzen_kumuliert_netto <- nutzen_jahr_netto * betriebszugehoerigkeit
  
  # Taylor-Russell
  erfolg_alt <- taylor_russell(r_alt, selektionsquote, basisrate)
  erfolg_neu <- taylor_russell(r_neu, selektionsquote, basisrate)
  fehl_alt <- round(n_einstellungen * (1 - erfolg_alt))
  fehl_neu <- round(n_einstellungen * (1 - erfolg_neu))
  
  # ROI
  roi <- if (kosten_verfahren_jahr > 0) {
    nutzen_jahr_brutto / kosten_verfahren_jahr
  } else {
    Inf
  }
  
  # Ergebnis zusammenstellen
  ergebnis <- list(
    # Parameter
    parameter = list(
      n_einstellungen = n_einstellungen,
      jahresgehalt = jahresgehalt,
      r_alt = r_alt,
      r_neu = r_neu,
      delta_r = delta_r,
      selektionsquote = selektionsquote,
      sdy_prozent = sdy_prozent,
      betriebszugehoerigkeit = betriebszugehoerigkeit,
      basisrate = basisrate,
      verfahrenskosten_pro_person = verfahrenskosten_pro_person
    ),
    # Zwischenergebnisse
    zwischenwerte = list(
      sdy_euro = sdy,
      zs = zs,
      n_bewerber_getestet = n_bewerber
    ),
    # Nutzen
    nutzen = list(
      pro_person_pro_jahr = nutzen_pro_person_jahr,
      pro_jahr_brutto = nutzen_jahr_brutto,
      pro_jahr_netto = nutzen_jahr_netto,
      kumuliert_brutto = nutzen_kumuliert_brutto,
      kumuliert_netto = nutzen_kumuliert_netto,
      verfahrenskosten_pro_jahr = kosten_verfahren_jahr,
      roi = roi
    ),
    # Fehleinstellungen
    fehleinstellungen = list(
      erfolgsquote_alt = erfolg_alt,
      erfolgsquote_neu = erfolg_neu,
      fehleinstellungen_alt = fehl_alt,
      fehleinstellungen_neu = fehl_neu,
      vermiedene_fehleinstellungen = fehl_alt - fehl_neu
    )
  )
  
  class(ergebnis) <- "utility_bcg"
  return(ergebnis)
}

# Print-Methode für schöne Ausgabe
print.utility_bcg <- function(x, ...) {
  p <- x$parameter
  z <- x$zwischenwerte
  n <- x$nutzen
  f <- x$fehleinstellungen
  
  fmt <- function(val) format(round(val), big.mark = ".")
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║         UTILITY-ANALYSE (Brogden-Cronbach-Gleser)          ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")
  
  cat("─── Parameter ─────────────────────────────────────────────────\n")
  cat(sprintf("  Einstellungen/Jahr:        %s\n", fmt(p$n_einstellungen)))
  cat(sprintf("  Jahresgehalt:              %s €\n", fmt(p$jahresgehalt)))
  cat(sprintf("  SDy (%% des Gehalts):       %d %% → %s €\n", p$sdy_prozent, fmt(z$sdy_euro)))
  cat(sprintf("  Validität alt → neu:       .%02d → .%02d (Δr = .%02d)\n",
              round(p$r_alt * 100), round(p$r_neu * 100), round(p$delta_r * 100)))
  cat(sprintf("  Selektionsquote:           .%02d → Z̄s = %.3f\n",
              round(p$selektionsquote * 100), z$zs))
  cat(sprintf("  Betriebszugehörigkeit:     %.1f Jahre\n", p$betriebszugehoerigkeit))
  
  if (p$verfahrenskosten_pro_person > 0) {
    cat(sprintf("  Verfahrenskosten/Person:   %s € (%s Bewerber*innen → %s €/Jahr)\n",
                fmt(p$verfahrenskosten_pro_person),
                fmt(z$n_bewerber_getestet),
                fmt(n$verfahrenskosten_pro_jahr)))
  }
  
  cat("\n─── Nutzengewinn ──────────────────────────────────────────────\n")
  cat(sprintf("  Pro Einstellung/Jahr:      %s €\n", fmt(n$pro_person_pro_jahr)))
  cat(sprintf("  Pro Jahr (brutto):         %s €\n", fmt(n$pro_jahr_brutto)))
  
  if (p$verfahrenskosten_pro_person > 0) {
    cat(sprintf("  Verfahrenskosten/Jahr:   − %s €\n", fmt(n$verfahrenskosten_pro_jahr)))
    cat(sprintf("  Pro Jahr (netto):          %s €\n", fmt(n$pro_jahr_netto)))
    cat(sprintf("  ROI:                       %.1f:1\n", n$roi))
  }
  
  cat(sprintf("  ──────────────────────────────────────────\n"))
  cat(sprintf("  Kumuliert (%s J.):        %s €\n",
              p$betriebszugehoerigkeit,
              fmt(if (p$verfahrenskosten_pro_person > 0) n$kumuliert_netto
                  else n$kumuliert_brutto)))
  
  cat("\n─── Fehleinstellungen (Taylor-Russell, BR = ",
      round(p$basisrate * 100), "%) ─────────────\n", sep = "")
  cat(sprintf("  Altes Verfahren:           %.1f %% Fehler → %d von %d\n",
              (1 - f$erfolgsquote_alt) * 100, f$fehleinstellungen_alt, p$n_einstellungen))
  cat(sprintf("  Neues Verfahren:           %.1f %% Fehler → %d von %d\n",
              (1 - f$erfolgsquote_neu) * 100, f$fehleinstellungen_neu, p$n_einstellungen))
  cat(sprintf("  Vermiedene Fehlbes.:       %d pro Jahr\n", f$vermiedene_fehleinstellungen))
  cat("\n")
}


# =============================================================================
# 9. Anwendungsbeispiele
# =============================================================================

cat("\n###############################################################\n")
cat("# ANWENDUNGSBEISPIELE\n")
cat("###############################################################\n")

# --- Beispiel 1: Fachkraft ---
cat("\n--- Beispiel 1: Fachkraft ---\n")
erg1 <- utility_bcg(
  n_einstellungen = 50,
  jahresgehalt = 55000,
  r_alt = 0.15,
  r_neu = 0.51,
  selektionsquote = 0.30
)
print(erg1)

# --- Beispiel 2: Führungskraft mit Verfahrenskosten ---
cat("\n--- Beispiel 2: Führungskraft mit AC-Kosten ---\n")
erg2 <- utility_bcg(
  n_einstellungen = 10,
  jahresgehalt = 120000,
  r_alt = 0.20,
  r_neu = 0.63,
  selektionsquote = 0.15,
  sdy_prozent = 60,
  betriebszugehoerigkeit = 6,
  verfahrenskosten_pro_person = 500  # AC-Kosten pro getesteter Person
)
print(erg2)

# --- Beispiel 3: Großunternehmen ---
cat("\n--- Beispiel 3: Großunternehmen (500 Einstellungen) ---\n")
erg3 <- utility_bcg(
  n_einstellungen = 500,
  jahresgehalt = 50000,
  r_alt = 0.10,
  r_neu = 0.51,
  selektionsquote = 0.20,
  sdy_prozent = 40,
  betriebszugehoerigkeit = 4.5
)
print(erg3)

# --- Beispiel 4: Minimalinvestition – nur Interviews strukturieren ---
cat("\n--- Beispiel 4: Nur unstr. → strukturiertes Interview ---\n")
cat("    (Die billigste Maßnahme mit dem besten Kosten-Nutzen-Verhältnis)\n")
erg4 <- utility_bcg(
  n_einstellungen = 100,
  jahresgehalt = 50000,
  r_alt = 0.33,                      # Unstrukturiertes Interview
  r_neu = 0.51,                      # Strukturiertes Interview
  selektionsquote = 0.30,
  verfahrenskosten_pro_person = 0    # Keine Zusatzkosten!
)
print(erg4)


# =============================================================================
# 10. Sensitivitätsanalyse: Was passiert, wenn sich Parameter ändern?
# =============================================================================

cat("\n--- Sensitivitätsanalyse: Nutzen/Jahr bei variierendem SDy% und Δr ---\n\n")

delta_r_grid <- seq(0.05, 0.50, by = 0.05)
sdy_pct_grid <- c(20, 30, 40, 60, 80)

sens_matrix <- outer(delta_r_grid, sdy_pct_grid, Vectorize(function(dr, sdp) {
  round(50 * dr * (55000 * sdp / 100) * berechne_zs(0.30))
}))
rownames(sens_matrix) <- paste0("Δr=.", sprintf("%02d", delta_r_grid * 100))
colnames(sens_matrix) <- paste0("SDy=", sdy_pct_grid, "%")

cat("Nutzen/Jahr in € (N=50, Gehalt=55.000€, SR=.30):\n")
print(sens_matrix)
cat("\n")
cat("→ Selbst bei konservativen Annahmen (SDy=20%, Δr=.10) liegt der\n")
cat("  jährliche Nutzengewinn bereits bei",
    format(sens_matrix["Δr=.10", "SDy=20%"], big.mark = "."), "€.\n")
