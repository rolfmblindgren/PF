# Overgang fra ICD-10-typer til ICD-11-trekk

Dette er en liten Shiny-app som fungerer som et pedagogisk overgangsverktøy mellom personlighetsforstyrrelser i ICD-10 og en trekkbasert modell som ligner ICD-11.

Tanken er å hjelpe brukere som allerede kjenner de tradisjonelle PF-typene, men som synes trekkspråket i ICD-11 er vanskeligere å lære. Appen viser hvilke kjente ICD-10-typer en gitt trekkprofil ligner mest på, og gjør samtidig synlig hvor mye de gamle typene overlapper.

## Krav

- R
- Pakkene `shiny`, `dplyr`, `ggplot2`, `shiny.i18n`, `grendelMeta` og `grendelStripe`

## Start appen

Kjør fra prosjektmappen:

```r
shiny::runApp()
```

eller:

```sh
Rscript -e "shiny::runApp()"
```

## Stripe-donasjoner

Appen kan sende brukeren til Stripe Checkout for en enkel donasjon. Dette krever minst:

- `STRIPE_SECRET_KEY`

Hvis du også vil registrere fullforte donasjoner robust pa serversiden, kan du starte en liten webhook-tjeneste med:

- `STRIPE_WEBHOOK_SECRET`
- valgfritt `STRIPE_DONATION_LOG_PATH` for hvor JSONL-loggen skal skrives

Webhook-skriptet ligger i [scripts/stripe_webhook.R](/Users/roffe/Documents/prosjekter/R/pf/scripts/stripe_webhook.R) og kan startes slik:

```sh
Rscript scripts/stripe_webhook.R
```

Som standard lytter det pa port `8010`. Endre dette med `STRIPE_WEBHOOK_PORT` dersom du vil ha en annen port.

## Kalibrering av vekter

Det ligger også et skript for å teste og tune vektene mot faglige ankerprofiler:

```sh
Rscript calibrate.R
```

Skriptet gjør dette:

- evaluerer dagens vekter mot et sett med ankerprofiler
- justerer vektene automatisk med random search mot en tapsfunksjon
- skriver ut nye vekter og en enkel oversikt over topp-1-fordeling på hele `7^5`-gridet
- lagrer rapportfiler i [calibration_outputs](/Users/roffe/Documents/prosjekter/R/pf/calibration_outputs)

Hvis du vil ta i bruk de tunede vektene i appen, kan du kopiere [calibration_outputs/tuned_weights.csv](/Users/roffe/Documents/prosjekter/R/pf/calibration_outputs/tuned_weights.csv) til [tuned_weights.csv](/Users/roffe/Documents/prosjekter/R/pf/tuned_weights.csv). Appen leser denne filen automatisk hvis den finnes.

## Grid-rapport

Det ligger også et eget skript for å gå gjennom hele `7^5`-gridet og peke ut profiler som ser kontraintuitive ut:

```sh
Rscript grid_report.R
```

Skriptet skriver blant annet:

- [calibration_outputs/grid_report.md](/Users/roffe/Documents/prosjekter/R/pf/calibration_outputs/grid_report.md)
- [calibration_outputs/grid_flag_summary.csv](/Users/roffe/Documents/prosjekter/R/pf/calibration_outputs/grid_flag_summary.csv)
- [calibration_outputs/grid_flagged_profiles.csv](/Users/roffe/Documents/prosjekter/R/pf/calibration_outputs/grid_flagged_profiles.csv)

## Hva appen gjør

- Brukeren setter fem trekkdimensjoner på skalaen 0-6.
- Appen bruker disse som en ICD-11-lignende trekkprofil.
- Modellen beregner en profilsamsvarsskår for hver ICD-10-type.
- `lambda` justerer hvor sterk kontrasten blir mellom profiler som ellers ligger nær hverandre.

## Pedagogisk poeng

- Appen er ment som en læringsbro fra kjente ICD-10-typer til ICD-11-lignende trekk.
- Den viktigste gevinsten er ofte ikke å peke ut én “riktig” type, men å vise hvilke typer som ligger nær hverandre.
- Det gjør det lettere å se hvor personlighetsforstyrrelser overlapper, og hvilke diagnostiske spørsmål trekkmodellen rydder bedre opp i enn en ren typeforståelse.

## Viktig

Vektene i modellen er en pedagogisk faglig forenkling, ikke en offisiell konverteringsnøkkel mellom ICD-10 og ICD-11.

Modellen er kun et pedagogisk verktøy og skal ikke brukes diagnostisk.

## Release-historikk

- Se [NEWS.md](/Users/roffe/Documents/prosjekter/R/pf/NEWS.md) for kort release-historikk.
- Se [.github/RELEASE_TEMPLATE.md](/Users/roffe/Documents/prosjekter/R/pf/.github/RELEASE_TEMPLATE.md) som mal for neste release.
