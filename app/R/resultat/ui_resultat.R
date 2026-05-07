ui_resultat <- function(app_kontext) {
  tagList(

    # JS-bro för "Save as"-dialog vid spara av prognos (Excel).
    # Faller tillbaka till vanlig nedladdning om showSaveFilePicker
    # saknas eller anropet sker utan användargest (auto-trigger).
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('spara_prognos_filer', async function(msg) {
          const namn = msg.filnamn;
          const b64  = msg.innehall_b64;

          const binary = atob(b64);
          const bytes = new Uint8Array(binary.length);
          for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);

          const xlsxMime = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';

          const fallbackDownload = () => {
            const blob = new Blob([bytes], { type: xlsxMime });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = namn;
            document.body.appendChild(a);
            a.click();
            setTimeout(() => { URL.revokeObjectURL(url); a.remove(); }, 100);
          };

          if ('showSaveFilePicker' in window) {
            try {
              const handle = await window.showSaveFilePicker({
                suggestedName: namn,
                types: [{
                  description: 'Excel-fil',
                  accept: { [xlsxMime]: ['.xlsx'] }
                }]
              });
              const writable = await handle.createWritable();
              await writable.write(bytes);
              await writable.close();
              Shiny.setInputValue('spara_prognos_status', { ok: true, t: Date.now() }, { priority: 'event' });
            } catch (e) {
              if (e.name === 'AbortError') return; // användaren avbröt
              if (e.name === 'SecurityError' || e.name === 'NotAllowedError') {
                // Saknas användargest (t.ex. auto-trigger) — fall tillbaka.
                fallbackDownload();
                Shiny.setInputValue('spara_prognos_status', { ok: true, t: Date.now() }, { priority: 'event' });
              } else {
                Shiny.setInputValue('spara_prognos_status', { ok: false, fel: e.message, t: Date.now() }, { priority: 'event' });
              }
            }
          } else {
            // Firefox/Safari utan File System Access API
            fallbackDownload();
            Shiny.setInputValue('spara_prognos_status', { ok: true, t: Date.now() }, { priority: 'event' });
          }
        });
      "))
    ),

    # Geografi-väljare (visas endast vid regional prognos)
    uiOutput("vald_geografi_ui"),

    navset_tab(

      # ----------------------------------------------------------
      # FLIK 1 – DEMOGRAFISKA KOMPONENTER (tidsserie)
      # ----------------------------------------------------------
      nav_panel(
        title = "Demografiska komponenter",

        layout_sidebar(
          sidebar = sidebar(
            title = "Information",
            p("Visar demografiska komponenter för vald geografi med både historiska ",
              "data (svart) och prognosdata (blå)."),
            p("Den streckade linjen markerar övergången mellan historisk data och prognos.")
          ),

          div(
            # Rad: Total befolkning + förändring
            fluidRow(
              class = "mb-3",
              column(6, card(
                card_header("Total befolkning"),
                card_body(girafeOutput("plot_total_befolkning", height = "300px"))
              )),
              column(6, card(
                card_header("Total befolkningsförändring"),
                card_body(girafeOutput("plot_total_forandring", height = "300px"))
              ))
            ),

            # Rad: Födelsekomponenter
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Födda"),
                card_body(girafeOutput("plot_fodda"))
              )),
              column(4, card(
                card_header("Döda"),
                card_body(girafeOutput("plot_doda"))
              )),
              column(4, card(
                card_header("Födelsenetto"),
                card_body(girafeOutput("plot_fodelsenetto"))
              ))
            ),

            # Rad: Inrikes flyttningar
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Inrikes inflyttade"),
                card_body(girafeOutput("plot_inrikes_inflyttade"))
              )),
              column(4, card(
                card_header("Inrikes utflyttade"),
                card_body(girafeOutput("plot_inrikes_utflyttade"))
              )),
              column(4, card(
                card_header("Inrikes flyttnetto"),
                card_body(girafeOutput("plot_inrikes_netto"))
              ))
            ),

            # Rad: Utrikes flyttningar
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Invandrade"),
                card_body(girafeOutput("plot_invandrade", height = "300px"))
              )),
              column(4, card(
                card_header("Utvandrade"),
                card_body(girafeOutput("plot_utvandrade", height = "300px"))
              )),
              column(4, card(
                card_header("Utrikes flyttnetto"),
                card_body(girafeOutput("plot_utrikes_netto", height = "300px"))
              ))
            )
          )
        )
      ),

      # ----------------------------------------------------------
      # FLIK 2 – 1-ÅRSKLASSER
      # ----------------------------------------------------------
      nav_panel(
        title = "1-årsklasser",

        layout_sidebar(
          sidebar = sidebar(
            title = "Inställningar",
            checkboxGroupInput(
              "ar_ettar",
              "Välj år att visa:",
              choices  = NULL,
              selected = NULL
            ),
            hr(),
            h6("Information"),
            p("Visar demografiska komponenter fördelade på 1-årsklasser (summerat över könen)."),
            p("'Födda efter moderns ålder' visar antal födda fördelat på moderns ålder (15-49 år)."),
            p("Välj ett eller flera år för att jämföra historiska data och prognosdata.")
          ),

          div(
            # Rad 1: Födelsekomponenter
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Födda efter moderns ålder"),
                card_body(plotOutput("plot_fodda_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Döda per åldersklass"),
                card_body(plotOutput("plot_doda_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Total befolkning per åldersklass"),
                card_body(plotOutput("plot_total_befolkning_ettar", height = "300px"))
              ))
            ),

            # Rad 2: Inrikes
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Inrikes inflyttade per åldersklass"),
                card_body(plotOutput("plot_inrikes_inflyttade_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Inrikes utflyttade per åldersklass"),
                card_body(plotOutput("plot_inrikes_utflyttade_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Inrikes flyttnetto per åldersklass"),
                card_body(plotOutput("plot_inrikes_netto_ettar", height = "300px"))
              ))
            ),

            # Rad 3: Utrikes
            fluidRow(
              class = "mb-3",
              column(4, card(
                card_header("Invandrade per åldersklass"),
                card_body(plotOutput("plot_invandrade_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Utvandrade per åldersklass"),
                card_body(plotOutput("plot_utvandrade_ettar", height = "300px"))
              )),
              column(4, card(
                card_header("Utrikes flyttnetto per åldersklass"),
                card_body(plotOutput("plot_utrikes_netto_ettar", height = "300px"))
              ))
            )
          )
        )
      ),

      # ----------------------------------------------------------
      # FLIK 3 – RISKTAL
      # ----------------------------------------------------------
      nav_panel(
        title = "Risktal",

        layout_sidebar(
          sidebar = sidebar(
            title = "Inställningar",
            checkboxGroupInput(
              "ar_risk_multi",
              "Välj år att visa:",
              choices  = NULL,
              selected = NULL
            ),
            selectInput(
              "kon_risk",
              "Välj kön:",
              choices  = c("Båda" = "Båda", "Kvinnor" = "kvinnor", "Män" = "män"),
              selected = "kvinnor"
            ),
            hr(),
            h6("Information"),
            p(strong("Historiska år"), " visas med tunnare linjer och är något genomskinliga."),
            p(strong("Prognosår"), " visas med tjockare linjer."),
            p("Födelserisker visas enbart för kvinnor 15-49 år."),
            p("För övriga risktal, välj ett kön för tydligare visualisering.")
          ),

          div(
            # Rad 1: Födelserisker + Dödsrisker
            fluidRow(
              class = "mb-3",
              column(6, card(
                card_header("Födelserisker"),
                card_body(plotOutput("plot_fodelserisker", height = "300px"))
              )),
              column(6, card(
                card_header("Dödsrisker"),
                card_body(plotOutput("plot_dodsrisker", height = "300px"))
              ))
            ),

            # Rad 2: Inrikes
            fluidRow(
              class = "mb-3",
              column(6, card(
                card_header("Inflyttningsrisker"),
                card_body(plotOutput("plot_inflyttningsrisker", height = "300px"))
              )),
              column(6, card(
                card_header("Utflyttningsrisker"),
                card_body(plotOutput("plot_utflyttningsrisker", height = "300px"))
              ))
            ),

            # Rad 3: Utrikes
            fluidRow(
              class = "mb-3",
              column(6, card(
                card_header("Invandringsrisker"),
                card_body(plotOutput("plot_invandringsrisker", height = "300px"))
              )),
              column(6, card(
                card_header("Utvandringsrisker"),
                card_body(plotOutput("plot_utvandringsrisker", height = "300px"))
              ))
            )
          )
        )
      ),

      # ----------------------------------------------------------
      # FLIK 4 – METOD OCH INSTÄLLNINGAR
      # ----------------------------------------------------------
      nav_panel(
        title = "Metod och inställningar",

        layout_sidebar(
          sidebar = sidebar(
            title = "Information",
            uiOutput("metod_scenario_info"),
            hr(),
            p(em("Klicka på rubrikerna nedan för att läsa mer om respektive komponent."))
          ),

          div(
            # Övergripande metodbeskrivning
            card(
              card_header(class = "bg-primary text-white",
                          h4("Om kohort-komponent-metoden")),
              card_body(
                HTML("
              <p><strong>Kohort-komponent-metoden</strong> är den vanligaste metoden för
              befolkningsprognoser och används av de flesta statistikmyndigheter världen över,
              inklusive SCB. Metoden följer befolkningen uppdelad i kohorter (födelseårsgrupper)
              och beräknar hur varje kohort förändras över tiden genom demografiska komponenter.</p>

              <h5>Prognosens huvudsteg:</h5>
              <ol>
                <li><strong>Startbefolkning:</strong> Utgår från den senaste kända befolkningen
                uppdelad efter kön och 1-årsklasser (0-100+ år).</li>
                <li><strong>Åldring:</strong> För varje prognosår åldras befolkningen ett år.</li>
                <li><strong>Demografiska händelser:</strong> Antal födda, döda, in- och utflyttade
                samt in- och utvandrade beräknas med åldersspecifika risktal.</li>
                <li><strong>Ny befolkning:</strong> Gammal befolkning + Födda − Döda +
                Inflyttade − Utflyttade + Invandrade − Utvandrade.</li>
                <li><strong>Upprepning:</strong> Processen upprepas för varje prognosår.</li>
              </ol>

              <h5>Risktal och antaganden:</h5>
              <p>Risktalen baseras på historiska mönster viktade med vald viktningsmetod och
              utjämnas sedan med splines. Lokala risktal sätts i relation till riksprognosen
              för att fånga nationella trender.</p>
              ")
              )
            ),

            # Regional vs Enskild prognos
            card(
              card_header(class = "bg-info text-white",
                          h4("Regional och enskild prognos - skillnader och avstämning")),
              card_body(
                HTML("
              <h5>Två typer av prognoser</h5>
              <p>Modellen kan köra två typer av prognoser:</p>

              <ol>
                <li><strong>Enskild prognos:</strong> Beräknar befolkningsutvecklingen för EN geografi (kommun eller region)
                utan hänsyn till andra geografier. Enklare och snabbare, men saknar regional konsistens.</li>

                <li><strong>Regional prognos:</strong> Beräknar samtidigt för ett län och alla dess kommuner med
                avstämning så att kommunernas summor matchar länstotalerna. Mer komplex men säkerställer konsistens.</li>
              </ol>

              <h5>Varför behövs avstämning?</h5>
              <p>När prognoser beräknas separat för län och kommuner uppstår ofta skillnader:</p>
              <ul>
                <li>Summan av kommunernas födda ≠ Länets födda</li>
                <li>Summan av kommunernas inflyttning över länsgräns ≠ Länets inflyttning</li>
                <li>Och så vidare för alla komponenter...</li>
              </ul>

              <p>Detta beror på att risktal beräknas separat för varje geografi baserat på deras historiska data.
              Små slumpmässiga variationer och avrundningar gör att totalerna inte stämmer.</p>

              <h5>Avstämningsprocessen</h5>
              <p><strong>Grundprincipen:</strong> Länets totaler anses mer tillförlitliga eftersom de baseras på större
              befolkningsunderlag. Därför justeras kommunernas värden så att summan matchar länets.</p>

              <h6>1. Komponenter som alltid avstäms (födda, döda, invandring, utvandring):</h6>
              <p>För dessa komponenter är processen relativt enkel:</p>
              <ol>
                <li>Beräkna komponenten för varje kommun baserat på deras risktal</li>
                <li>Summera alla kommuners värden per kön/ålder/år</li>
                <li>Jämför med länets motsvarande värde</li>
                <li>Beräkna justeringsfaktor: <code>Länstotal / Kommunsumma</code></li>
                <li>Multiplicera varje kommuns värde med justeringsfaktorn</li>
              </ol>

              <p><em>Exempel:</em> Om kommunerna tillsammans beräknas få 1050 födda pojkar i åldern 0,
              men länet ska ha 1000, blir justeringsfaktorn 1000/1050 = 0,952.
              Varje kommuns antal födda pojkar multipliceras med 0,952.</p>

              <h6>2. Speciell hantering av inrikes flyttningar:</h6>
              <p>Inrikes flyttningar är mer komplexa eftersom vi måste skilja på:</p>
              <ul>
                <li><strong>Flyttningar inom länet</strong> (mellan länets kommuner)</li>
                <li><strong>Flyttningar över länsgräns</strong> (till/från andra län)</li>
              </ul>

              <p><strong>Länets flyttningar = ENDAST flyttningar över länsgräns</strong><br>
              <strong>Kommunernas flyttningar = BÅDE inom länet OCH över länsgräns</strong></p>

              <p>Avstämningsprocessen för flyttningar:</p>
              <ol>
                <li>För varje kommun: Separera flyttningar i två delar baserat på historiska andelar
                  <ul>
                    <li>Del som går inom länet (påverkas INTE av avstämning)</li>
                    <li>Del som går över länsgräns (denna del justeras)</li>
                  </ul>
                </li>
                <li>Summera kommunernas länsgränsflyttningar</li>
                <li>Jämför med länets total (som bara innehåller länsgränsflyttningar)</li>
                <li>Justera ENDAST länsgränsdelen: <code>Ny länsgränsdel = Original × (Länstotal / Kommunsumma)</code></li>
                <li>Kommunens nya total = Inom länet + Justerad länsgränsdel</li>
              </ol>

              <h6>3. Hantering av saknade värden:</h6>
              <p>Om länet har värden för en viss kön/ålder-kombination men ingen kommun har det:</p>
              <ul>
                <li>Fördela länets värde jämnt mellan kommunerna</li>
                <li>Varje kommun får: <code>Länsvärde / Antal kommuner</code></li>
              </ul>

              <h6>4. Avrundningskorrigering:</h6>
              <p>Efter justering kan avrundningsfel göra att summan fortfarande inte stämmer exakt. Då:</p>
              <ul>
                <li>Beräkna återstående differens</li>
                <li>Lägg hela differensen på den största kommunen (som tål avrundningsfelet bäst)</li>
              </ul>

              <h5>Fördelar med regional avstämning</h5>
              <ul>
                <li><strong>Konsistens:</strong> Summan av kommunerna = Länstotalen</li>
                <li><strong>Robusthet:</strong> Länets större befolkning ger stabilare skattningar</li>
                <li><strong>Realism:</strong> Fångar regionala samband och begränsningar</li>
                <li><strong>Jämförbarhet:</strong> Kommuner kan jämföras rättvist inom regionen</li>
              </ul>

              <h5>När bör man välja vad?</h5>
              <ul>
                <li><strong>Välj enskild prognos när:</strong> Du bara behöver prognos för en kommun/region,
                vill ha snabbt resultat, eller när regional konsistens inte är kritisk.</li>

                <li><strong>Välj regional prognos när:</strong> Du behöver prognoser för flera kommuner,
                vill säkerställa att totalerna stämmer, eller när du ska jämföra kommuner inom länet.</li>
              </ul>
              ")
              )
            ),

            # Standard vs Alternativ prognos
            card(
              card_header(class = "bg-warning text-white",
                          h4("Standard- och alternativprognos - scenarioanalys")),
              card_body(
                HTML("
              <h5>Två prognosscenarier</h5>
              <p>Modellen kan köra två olika scenarier för att hantera osäkerhet om framtiden:</p>

              <ol>
                <li><strong>Standardprognos:</strong> Utgår från att historiska mönster fortsätter oförändrade.
                De risktal som beräknats från historiska data appliceras rakt av på hela prognosperioden.</li>

                <li><strong>Alternativprognos:</strong> Tillåter justeringar av risktalen för specifika perioder
                för att simulera förändrade förutsättningar eller politiska beslut.</li>
              </ol>

              <h5>Hur fungerar periodiseringar?</h5>
              <p>Periodiseringar är justeringar av risktal som gäller för specifika tidsperioder.
              De fungerar som multiplikatorer på de historiskt beräknade risktalen:</p>

              <div class='alert alert-info'>
                <strong>Exempel:</strong> Om inflyttningsrisken för 25-åringar historiskt varit 0,01 (1%),
                och vi tillämpar en multiplikator på 1,10 för perioden 2025-2029, blir den justerade risken
                0,011 (1,1%) under dessa år.
              </div>

              <h5>Kopplingen till historiska beräkningar</h5>
              <p><strong>Viktigt att förstå:</strong> Alternativscenarier bygger fortfarande på historiska data som bas.
              De historiska mönstren är utgångspunkten som sedan justeras.</p>

              <p>Processen ser ut så här:</p>
              <ol>
                <li><strong>Historisk analys:</strong> Risktal beräknas baserat på faktiska data från de senaste 7-10 åren
                (beroende på komponent). Detta ger grundmönstret.</li>

                <li><strong>Viktning av historik:</strong> Olika viktningsmetoder (jämn, linjär eller EWMA) används
                för att balansera mellan stabilitet och aktualitet i de historiska mönstren.</li>

                <li><strong>Utjämning:</strong> Risktalen utjämnas med splines för att ta bort slumpmässiga variationer
                och få mjuka åldersprofiler.</li>

                <li><strong>Scenariojustering:</strong> FÖRST EFTER alla dessa steg appliceras eventuella
                scenariojusteringar som multiplikatorer på de färdiga risktalen.</li>
              </ol>

              <h5>Varför behövs alternativscenarier?</h5>
              <p>Historiska data fångar inte framtida strukturella förändringar:</p>
              <ul>
                <li><strong>Politiska beslut:</strong> Nya bostadsområden, infrastruktursatsningar, företagsetableringar</li>
                <li><strong>Demografiska trender:</strong> Förändrade barnafödandemönster, pensionsålder</li>
                <li><strong>Externa chocker:</strong> Pandemier, ekonomiska kriser, migration</li>
                <li><strong>Lokala satsningar:</strong> Universitetsexpansion, vårdcentraler, äldreboenden</li>
              </ul>

              <h5>Exempel på periodiseringar</h5>
              <table class='table table-sm'>
                <thead>
                  <tr>
                    <th>Komponent</th>
                    <th>Period</th>
                    <th>Justering</th>
                    <th>Motivering</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td>Inflyttning</td>
                    <td>2025-2029</td>
                    <td>+10%</td>
                    <td>Nytt bostadsområde med 500 lägenheter</td>
                  </tr>
                  <tr>
                    <td>Födelser</td>
                    <td>2030-2040</td>
                    <td>+5%</td>
                    <td>Familjevänlig politik och barnomsorgsgaranti</td>
                  </tr>
                  <tr>
                    <td>Dödsrisker</td>
                    <td>2027-2040</td>
                    <td>-2%</td>
                    <td>Förbättrad äldreomsorg och vårdcentral</td>
                  </tr>
                  <tr>
                    <td>Utvandring</td>
                    <td>2025-2030</td>
                    <td>+20%</td>
                    <td>Osäker arbetsmarknad för nyanlända</td>
                  </tr>
                </tbody>
              </table>

              <h5>Tolkning av resultat</h5>
              <p>När du jämför standard- och alternativprognos:</p>
              <ul>
                <li><strong>Standardprognos</strong> = \"Business as usual\" - vad händer om inget förändras</li>
                <li><strong>Alternativprognos</strong> = \"What if\" - vad händer vid specifika förändringar</li>
                <li><strong>Skillnaden</strong> = Effekten av de antagna förändringarna</li>
              </ul>

              <div class='alert alert-warning'>
                <strong>Varning:</strong> Alternativscenarier är inte prognoser av vad som kommer hända -
                de är simuleringar av vad som skulle kunna hända under givna antaganden.
                Använd dem för att förstå känslighet och planera för olika utfall.
              </div>

              <h5>Best practice för scenarioanalys</h5>
              <ol>
                <li><strong>Dokumentera antaganden:</strong> Varje justering bör ha en tydlig motivering</li>
                <li><strong>Var realistisk:</strong> Extrema justeringar (±50%) är sällan trovärdiga</li>
                <li><strong>Tänk systemiskt:</strong> Om inflyttning ökar, påverkas även utflyttning och födelser</li>
                <li><strong>Använd flera scenarier:</strong> Kör optimistiskt, realistiskt och pessimistiskt</li>
                <li><strong>Följ upp:</strong> Jämför prognoser mot utfall och justera metoden</li>
              </ol>
              ")
              )
            ),

            # Komponent-accordion
            accordion(
              open = FALSE,

              accordion_panel(
                title = "Födelserisker",
                uiOutput("fodda_info")
              ),
              accordion_panel(
                title = "Dödsrisker",
                uiOutput("doda_info")
              ),
              accordion_panel(
                title = "Inflyttningsrisker",
                uiOutput("inflyttning_info")
              ),
              accordion_panel(
                title = "Utflyttningsrisker",
                uiOutput("utflyttning_info")
              ),
              accordion_panel(
                title = "Invandringsrisker",
                uiOutput("invandring_info")
              ),
              accordion_panel(
                title = "Utvandringsrisker",
                uiOutput("utvandring_info")
              )
            )
          )
        )
      )

    ) # navset_tab
  ) # tagList
}
