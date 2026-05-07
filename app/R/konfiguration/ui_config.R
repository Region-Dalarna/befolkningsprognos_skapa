# Liten i-ikon med tooltip (Bootstrap-styled).
info_ikon <- function(text) {
  bslib::tooltip(
    tags$span(
      "ⓘ",  # ⓘ
      style = "cursor: help; color: #0d6efd; margin-left: 6px; font-weight: bold; user-select: none;"
    ),
    text,
    placement = "right"
  )
}

ui_config <- function(app_kontext, lan_i_data, geografier_i_data) {

  geo <- hamta_geografi_val()
  lan_val <- lan_i_data
  enskild_val <- geografier_i_data

  tagList(

    # JS-bro för "Save as"-dialog vid spara av inställningar
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('spara_installningar_filer', async function(msg) {
          const namn = msg.filnamn;
          const innehall = msg.innehall;

          if ('showSaveFilePicker' in window) {
            try {
              const handle = await window.showSaveFilePicker({
                suggestedName: namn,
                types: [{
                  description: 'JSON-fil',
                  accept: { 'application/json': ['.json'] }
                }]
              });
              const writable = await handle.createWritable();
              await writable.write(innehall);
              await writable.close();
              Shiny.setInputValue('spara_status', { ok: true, t: Date.now() }, { priority: 'event' });
            } catch (e) {
              if (e.name !== 'AbortError') {
                Shiny.setInputValue('spara_status', { ok: false, fel: e.message, t: Date.now() }, { priority: 'event' });
              }
            }
          } else {
            // Fallback: Firefox/Safari utan File System Access API
            const blob = new Blob([innehall], { type: 'application/json' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = namn;
            document.body.appendChild(a);
            a.click();
            setTimeout(() => { URL.revokeObjectURL(url); a.remove(); }, 100);
            Shiny.setInputValue('spara_status', { ok: true, t: Date.now() }, { priority: 'event' });
          }
        });
      "))
    ),

    div(
      class = "config-scroll",

      tags$div(
        style = "margin-bottom: 10px; font-size: 0.88em; color: #475569;",
        tags$em("Tips: Klicka på rubrikerna nedan för att fälla ut/in respektive sektion. "),
        "Under ", tags$b("Geografi & grundläggande val"), " ligger även sektionerna ",
        tags$b("Antaganden – viktningsparametrar"), " och ",
        tags$b("Scenariojusteringar"), "."
      ),

      accordion(
        open = c(
          "🌍 Geografi & grundläggande val",
          "⚙️ Antaganden – viktningsparametrar",
          "📊 Scenariojusteringar"
        ),

        accordion_panel(
          title = "🌍 Geografi & grundläggande val",
          fluidRow(
            column(5,
                   card(
                     card_header("Geografi"),
                     conditionalPanel(
                       "input.prognostyp == 'regional'",
                       selectizeInput(
                         "geografi_regional",
                         "Välj län",
                         choices = lan_val,
                         selected = "Dalarnas län"
                       )
                     ),
                     conditionalPanel(
                       "input.prognostyp == 'enskild'",
                       selectizeInput(
                         "geografi_enskild",
                         "Välj län eller kommun",
                         choices = enskild_val,
                         selected = "Dalarnas län"
                       )
                     ),
                     selectInput(
                       "prognostyp", "Prognostyp",
                       choices = c(
                         "Regional (hela länet)" = "regional",
                         "Enskild (ett område)"  = "enskild"
                       )
                     ),
                     numericInput(
                       "prognos_slut", "Prognosslutår",
                       2050, min = 2026
                     )
                   )
            ),
            column(7,
                   tags$details(
                     tags$summary(
                       "⚙️ Avancerade inställningar",
                       style = "cursor: pointer; font-weight: 600; padding: 0.75rem 1rem; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; user-select: none;"
                     ),
                     div(
                       style = "padding: 1rem; border: 1px solid #dee2e6; border-top: none; border-radius: 0 0 4px 4px;",
                       checkboxInput(
                         "ckm",
                         tagList(
                           "Använd CKM-data",
                           info_ikon(
                             "CKM (Cell Key Method) är SCB:s skyddade statistik där små värden har störts något för att skydda enskilda individers integritet. Bocka av om du har behörighet till råa registerdata och vill räkna på exakta värden."
                           )
                         ),
                         TRUE
                       ),
                       selectInput(
                         "namnare_dodsrisker",
                         tagList(
                           "Nämnare för dödsrisker",
                           info_ikon(
                             "Bestämmer vilken befolkning som används som nämnare när dödsrisker beräknas. Medelfolkmängd (rekommenderas) använder genomsnittet av befolkningen under året och ger stabilare risktal vid stora befolkningsförändringar. Totfolkmängd använder befolkningen den 31 december."
                           )
                         ),
                         choices = c(
                           "Medelfolkmängd" = "medelfolkmangd",
                           "Totfolkmängd (31 dec)" = "totfolkmangd"
                         )
                       ),
                       selectInput(
                         "avrundning",
                         tagList(
                           "Avrundning",
                           info_ikon(
                             "Hur prognosens värden avrundas. Ingen avrundning behåller decimaler – matematiskt mest exakt. Heltal avrundar varje cell vilket kan göra att summor inte stämmer exakt mot delarna. Stokastisk avrundning använder slumpmässig avrundning som bevarar summor i genomsnitt och rekommenderas vid små värden i kommunprognoser."
                           )
                         ),
                         choices = c(
                           "Ingen avrundning" = "ingen",
                           "Heltal"           = "heltal",
                           "Stokastisk"       = "stokastisk"
                         ),
                         selected = "ingen"
                       ),
                       conditionalPanel(
                         "input.prognostyp == 'regional'",
                         selectInput(
                           "metod_avstamning_regional",
                           tagList(
                             "Metod för regional avstämning",
                             info_ikon(
                               "Metod för att stämma av kommunernas summor mot länsprognosen så att delarna stämmer med totalen. Minsta kvadrat (rekommenderas) minimerar avvikelser och hanterar negativa värden korrekt. Raking är en iterativ proportionell justering som bevarar marginaler. Proportionell skalar alla kommuner med samma faktor – enklast men mest känslig för utfall nära noll."
                             )
                           ),
                           choices = c(
                             "Minsta kvadrat" = "minsta_kvadrat",
                             "Raking"         = "raking",
                             "Proportionell"  = "proportional"
                           ),
                           selected = "minsta_kvadrat"
                         )
                       ),
                       checkboxInput(
                         "dodsfall_fore_aldring",
                         tagList(
                           "Dödsfall före åldring",
                           info_ikon(
                             "Bestämmer ordningen i prognoscykeln. När ikryssad (rekommenderas) räknas dödsfall ut innan befolkningen åldras ett år, så att personer i åldersgrupp x dör med dödsrisker för åldersgrupp x. Om avbockad åldras befolkningen först och dödsfallen räknas på den åldrade befolkningen."
                           )
                         ),
                         TRUE
                       )
                     )
                   )
            )
          )
        ),

        accordion_panel(
          title = "⚙️ Antaganden – viktningsparametrar",

          ## ---- Verktygsrad: Spara / Ladda inställningar ----
          tags$div(
            style = "margin-bottom: 12px; padding: 10px 14px; background: #f1f5f9; border: 1px solid #cbd5e1; border-radius: 4px;",
            tags$div(
              style = "font-size: 0.88em; color: #334155; margin-bottom: 10px;",
              tags$b("Spara/ladda inställningar:"),
              " spara antaganden och eventuella reviderade komponenter till en fil — och läs in samma uppgifter senare för att vara säker på att du använder identiska indata."
            ),
            tags$div(
              style = "display: flex; gap: 8px; align-items: stretch;",
              actionButton(
                "spara_installningar_btn",
                "💾 Spara inställningar",
                class = "btn-outline-primary",
                style = "flex: 1;"
              ),
              actionButton(
                "ladda_installningar_btn",
                "📂 Ladda inställningar",
                class = "btn-outline-primary",
                style = "flex: 1;",
                onclick = "document.getElementById('ladda_installningar_fil').click();"
              ),
              # Dold fileInput — triggas av knappen ovan
              tags$div(
                style = "position: absolute; left: -9999px; width: 1px; height: 1px; overflow: hidden;",
                fileInput(
                  "ladda_installningar_fil",
                  label = NULL,
                  accept = c(".json", "application/json")
                )
              )
            )
          ),

          card(
            card_header("Viktningsparametrar per demografisk komponent"),

            ## ---- Förklaring (permanent synlig) ----
            tags$div(
              style = "padding: 0.75rem 1rem; border: 1px solid #cfe0f3; border-radius: 4px; background: #fafcff; font-size: 0.88em; margin-bottom: 0.75rem;",
              tags$div(
                style = "font-weight: 600; margin-bottom: 0.5rem; color: #1e3a5f;",
                "ℹ️ Förklaring av viktningsmetoder och Alpha"
              ),
              tags$p(
                tags$b("Antal år"),
                " avgör hur många historiska år som används för att beräkna risktalen.
                Fler år ger stabilare värden men reagerar långsammare på trendbrott."
              ),
              tags$p(tags$b("Viktningsmetoder:")),
              tags$ul(
                tags$li(
                  tags$b("1 – Jämn: "),
                  "alla år väger lika mycket (1/N). Bra när historien är stabil och inga
                  tydliga trender finns."
                ),
                tags$li(
                  tags$b("2 – Linjär: "),
                  "vikten ökar linjärt med året — det senaste året väger mest, det äldsta
                  minst. Ger en mjuk övergång mot nyare data."
                ),
                tags$li(
                  tags$b("3 – EWMA "),
                  "(Exponentially Weighted Moving Average): vikten avtar exponentiellt
                  bakåt i tiden. Reagerar snabbast på trendbrott."
                )
              ),
              tags$ul(
                style = "margin-bottom: 0;",
                tags$li(
                  tags$b("Alpha (α): "),
                  "Styr hur snabbt EWMA-vikterna avtar bakåt i tiden (mellan 0,1 och 0,9).",
                  tags$em(" Använd komma som decimaltecken.")
                ),
                tags$li(
                  tags$b("Lågt alpha (0,1–0,3) – långsam anpassning: "),
                  "många år påverkar resultatet ungefär lika mycket. Stabilt men
                  reagerar långsamt på trendbrott."
                ),
                tags$li(
                  tags$b("Mellanalpha (0,4–0,6) – balanserad anpassning: "),
                  "rimlig balans mellan stabilitet och lyhördhet för senare års
                  mönster."
                ),
                tags$li(
                  tags$b("Högt alpha (0,7–0,9) – snabb anpassning: "),
                  "de senaste åren dominerar. Reagerar snabbt men blir känsligt
                  för enskilda år."
                )
              )
            ),

            ## ---- Header-rad ----
            fluidRow(
              class = "risk-header-row",
              column(3, span("Komponent", class = "risk-header")),
              column(2, span("Antal år", class = "risk-header")),
              column(3, span("Viktningsmetod", class = "risk-header")),
              column(3, span("Alpha (EWMA, 0.1–0.9)", class = "risk-header"))
            ),

            risk_ui("fodelserisker", "Födelserisker"),
            risk_ui("dodsrisker", "Dödsrisker", default_vtyp = 1),
            risk_ui("inflyttningsrisker", "Inflyttningsrisker"),
            risk_ui("utflyttningsrisker", "Utflyttningsrisker"),
            risk_ui("invandringsrisker", "Invandringsrisker"),
            risk_ui("utvandringsrisker", "Utvandringsrisker")
          )
        ),

        accordion_panel(
          title = "📊 Scenariojusteringar",

          # Kort beskrivning av vad scenariojusteringar gör
          tags$div(
            style = "padding: 0.75rem 1rem; border: 1px solid #cfe0f3; border-radius: 4px; background: #fafcff; font-size: 0.88em; margin-bottom: 0.75rem;",
            tags$p(
              style = "margin-bottom: 0.4rem;",
              "Här kan du justera de beräknade risktalen för specifika tidsperioder för att simulera ",
              tags$b("alternativa framtider"),
              " — till exempel effekten av ett nytt bostadsområde, förändrad migrationspolitik eller satsningar på äldrevården."
            ),
            tags$p(
              style = "margin-bottom: 0.4rem;",
              "Justeringarna anges som multiplikatorer på de risktal som annars hade använts. Standardprognosen utgår från oförändrade historiska mönster; aktivera nedan för att skapa en alternativprognos."
            ),
            tags$p(
              style = "margin-bottom: 0;",
              "Multiplikatorer på beräknade risktal: ",
              tags$b("1,00"), " = ingen förändring, ",
              tags$b("1,10"), " = +10%, ",
              tags$b("0,90"), " = −10%. ",
              tags$em("Använd komma som decimaltecken.")
            )
          ),

          # checkbox högst upp
          tags$div(
            style = "display:flex; align-items:center; margin-bottom:6px;",
            checkboxInput("alt_aktivera", label = "Aktivera scenariojusteringar", value = FALSE)
          ),

          tags$div(
            id = "alt_wrap",
            style = "position: relative;",

            # overlay som blockerar klick när alt_aktivera = FALSE (som i gamla)
            conditionalPanel(
              condition = "!input.alt_aktivera",
              tags$div(class = "alt-overlay")
            ),

            # cards per komponent
            tags$div(
              id = "alt_container",
              class = "scroll-cards",
              lapply(
                list(
                  list(id="fodelserisker",  label="Födelserisker"),
                  list(id="dodsrisker",     label="Dödsrisker"),
                  list(id="inflyttningsrisker", label="Inflyttningsrisker"),
                  list(id="utflyttningsrisker", label="Utflyttningsrisker"),
                  list(id="invandringsrisker", label="Invandringsrisker"),
                  list(id="utvandringsrisker", label="Utvandringsrisker")
                ),
                function(k) {
                  card(
                    card_header(k$label),
                    uiOutput(paste0("alt_ui_", k$id))
                  )
                }
              )
            )
          )
        )
      )
    ),

    br(), br(),

    div(
      class = "config-footer",
      uiOutput("korstatus"),
      actionButton(
        "kor",
        "🚀 Kör prognos",
        class = "btn-primary btn-lg"
      )
    )
  )
}
