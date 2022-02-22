#shiny packages
library(shiny)
library(shinyjs)
library(shinyWidgets)

#general packages
library(tidyverse)


unique.con <- c('p', 'b', 't', 'd', 'k', 'ɡ', 
                'm', 'n', 'ŋ',
                'f', 'v', 'θ', 'ð', 's', 'z', 'ʃ', 'ʒ', 'h',
                'ɹ', 'j', 'l', 'w', 'tʃ', 'dʒ')

unique.vow <- c('i', 'ɪ', 'ɛ', 'a', 'ə', 'ɜ', 'u', 'ʊ', 'ʌ', 'ɔ', 'ɑ', 'ɒ',
                'eɪ', 'ɑɪ', 'ɔɪ', 'ɑʊ', 'əʊ', 'ɪə', 'ʊə', 'ɛə')

inventory <- c(unique.con, unique.vow)


#### READ IN DICTIONARY
dict <- read.delim("beep.txt", header=F, stringsAsFactors = F) %>%
  mutate(V1 = tolower(V1))

#convert BEEP transcription into IPA symbols
dict <- dict %>%
  mutate(V2 = str_replace_all(V2, c('ng' = 'ŋ', 'ch' = 'tʃ', 'eh' = 'ɛ', 'g' = 'ɡ', 'th' = 'θ', 'dh' = 'ð',
                                    'sh' = 'ʃ', 'zh' = 'ʒ', 'hh' = 'h', 'jh' = 'dʒ',
                                    'iy' = 'i', 'ih' = 'ɪ', 'eh' = 'ɛ', 'ae' = 'a', 'ax' = 'ə', 'er' = 'ɜ',
                                    'uw' = 'u', 'uh' = 'ʊ', 'ah' = 'ʌ', 'ao' = 'ɔ', 'aa' = 'ɑ', 'oh' = 'ɒ',
                                    'ey' = 'eɪ', 'ay' = 'ɑɪ', 'oy' = 'ɔɪ', 'aw' = 'ɑʊ', 'ow' = 'əʊ', 'ia' = 'ɪə',
                                    'ua' = 'ʊə', 'ea' = 'ɛə', 'r' = 'ɹ', 'y' = 'j')))


consonants <- c(rep('p', 3), rep('b', 2), rep('t', 5), rep('d', 5), rep('k', 4), 'ɡ', 
                rep('m', 3), rep('n', 5), rep('ŋ', 2),
                rep('f', 2), rep('v', 2), 'θ', rep('ð', 4), rep('s', 5), rep('z', 4), 'ʃ', 'ʒ', rep('h', 2),
                rep('ɹ', 5), 'j',
                rep('l', 4), rep('w', 2), 'tʃ', 'dʒ')

vowels <- c(rep('i', 3), rep('ɪ', 4), rep('ɛ', 3), rep('a', 3), 
            rep('ə', 4), 'ɜ',
            rep('u', 2), 'ʊ', 'ʌ', 'ɔ', 'ɑ', 'ɒ',
            rep('eɪ', 2), rep('aɪ', 2), 'ɔɪ', 'aʊ', 'əʊ', 'ɪə', 'ʊə', 'ɛə')


#### DISPLAY STYLE FOR SOUNDS
opening.html <- '<span style="background-color: #3471eb; color: #ffffff; font-size: 60px; padding-left: 5px; padding-right: 5px; border-style: solid; display: inline-block; width: 85px; margin-right: 5px"><b>'

closing.html <- '</b></span>'


############# UI #############

ui <- fluidPage(
  
  tags$head(includeHTML(("google-analytics.html"))),
  
  tags$head(tags$style(
    HTML("
         @import url('https://fonts.googleapis.com/css2?family=Inter&display=swap');
         #titlePanel {
           font-family: 'Inter', sans-serif;
           color: white;
           font-size: 40px;
           background: #3471eb;
           border-style: solid;
           border-size: 2px;
           border-radius: 20px;
           /*width: 800px;*/
           width: 75%;
           height: auto;
           padding: 10px 30px 10px 30px;
           margin: 0px 0px 15px 0px;
         }
         #title {
           /*font-size: 50px;*/
           font-size: 4vw;
           margin-top: 10px;
         }
         #subtitle {
           /*font-size: 20px;*/
           font-size: 2vw;
           margin: 0px 0px 10px 0px;
           padding: 0px;
         } 
         "))),
  
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "top"
  ),
  
  #tags$div(tags$img(src='test.png', width = 108, height = 108, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
  
  titlePanel(title = "", windowTitle = "IPA Countdown [aɪ pʰiː eɪ kʰaʊnt̚daʊn"),
  
  fluidRow(
    column(2,
      wellPanel(
        useShinyjs(),
        h4('Draw sounds'),
        helpText('Pick one of the following:'),
        actionButton('drawC', 'consonant', icon=icon("plus"), style="color: #fff; background-color: #3471eb; width: 100%;"),
        actionButton('drawV', 'vowel', icon=icon("plus"), style="color: #fff; background-color: #3471eb; width: 100%;"),
        br(),
        helpText('...or auto-fill:'),
        actionButton('surprise', 'surprise me', icon=icon("random"), style="color: #fff; background-color: #3471eb; width: 100%;"),
        br(), br(),
        actionButton('reset', 'reset board', icon=icon("times-circle"), style="color: #fff; background-color: #b51e00; width: 100%;")),
      wellPanel(
        h4('Dictionary corner'),
        helpText("5 longest possible words"),
        disabled(actionButton('answers', 'show me!', icon=icon("book"), style="color: #fff; background-color: #46C53E; width: 100%;")),
        textOutput("top1"),
        textOutput("top2"),
        textOutput("top3"),
        textOutput("top4"),
        textOutput("top5")
        ),
      wellPanel(
        actionButton('info', 'help/info', icon=icon("question-circle"), style="width: 100%;"),
        br(), br(),
        tags$div(style="font-size: 12px", 
                 HTML(paste0("Developed by ", tags$br(),
                    tags$a(href="https://www.gbailey.uk/", target="_blank", "George Bailey"), 
                    " (2020)", tags$br(), tags$br(),
                    icon("twitter"),
                    " Follow me on ", 
                    tags$a(href="https://twitter.com/grbails/", target="_blank", "Twitter"), 
                    "!")))
      )
      ),
  
      column(10, align="center",
             
             tags$div(id="titlePanel", 
                      h1(id='title', 'IPA Countdown'),
                      h3(id='subtitle', '[aɪ pʰiː eɪ kʰaʊnt̚daʊn]')
             ),
             
             fluidRow(
             tags$video(id="video2", type="video/mp4", src="countdown_clock.mp4", 
                        controls="controls", width="50%")),
    fluidRow(tags$br(),
      htmlOutput("s1", inline=T), htmlOutput("s2", inline=T), htmlOutput("s3", inline=T), 
      htmlOutput("s4", inline=T), htmlOutput("s5", inline=T), htmlOutput("s6", inline=T), 
      htmlOutput("s7", inline=T), htmlOutput("s8", inline=T), htmlOutput("s9", inline=T)
      )
    )
  )
)

########### SERVER ###########


server <- function(input, output) {
  
  drawnSounds <- reactiveValues(sounds = rep('-', 9),
                                drawn = 0,
                                text = NULL)
  
  #initialise the displayed sounds on load
  output$s1 <- renderText({ paste(opening.html, drawnSounds$sounds[1], closing.html) })
  output$s2 <- renderText({ paste(opening.html, drawnSounds$sounds[2], closing.html) })
  output$s3 <- renderText({ paste(opening.html, drawnSounds$sounds[3], closing.html) })
  output$s4 <- renderText({ paste(opening.html, drawnSounds$sounds[4], closing.html) })
  output$s5 <- renderText({ paste(opening.html, drawnSounds$sounds[5], closing.html) })
  output$s6 <- renderText({ paste(opening.html, drawnSounds$sounds[6], closing.html) })
  output$s7 <- renderText({ paste(opening.html, drawnSounds$sounds[7], closing.html) })
  output$s8 <- renderText({ paste(opening.html, drawnSounds$sounds[8], closing.html) })
  output$s9 <- renderText({ paste(opening.html, drawnSounds$sounds[9], closing.html) })
  
  #draw a consonant
  observeEvent(input$drawC, {
    drawnSounds$sounds[drawnSounds$drawn+1] <- sample(consonants, 1)
    drawnSounds$drawn = drawnSounds$drawn + 1
    
    if (drawnSounds$drawn == 1) {
      output$s1 <- renderText({ paste(opening.html, drawnSounds$sounds[1], closing.html) })
    } else if (drawnSounds$drawn == 2) {
      output$s2 <- renderText({ paste(opening.html, drawnSounds$sounds[2], closing.html) })
    } else if (drawnSounds$drawn == 3) {
      output$s3 <- renderText({ paste(opening.html, drawnSounds$sounds[3], closing.html) })
    } else if (drawnSounds$drawn == 4) {
      output$s4 <- renderText({ paste(opening.html, drawnSounds$sounds[4], closing.html) })
    } else if (drawnSounds$drawn == 5) {
      output$s5 <- renderText({ paste(opening.html, drawnSounds$sounds[5], closing.html) })
    } else if (drawnSounds$drawn == 6) {
      output$s6 <- renderText({ paste(opening.html, drawnSounds$sounds[6], closing.html) })
    } else if (drawnSounds$drawn == 7) {
      output$s7 <- renderText({ paste(opening.html, drawnSounds$sounds[7], closing.html) })
    } else if (drawnSounds$drawn == 8) {
      output$s8 <- renderText({ paste(opening.html, drawnSounds$sounds[8], closing.html) })
    } else if (drawnSounds$drawn == 9) {
      output$s9 <- renderText({ paste(opening.html, drawnSounds$sounds[9], closing.html) })
    }
    
    if (drawnSounds$drawn == 9) {
      shinyjs::disable("drawC")
      shinyjs::disable("drawV")
      shinyjs::enable("answers")
    }
  })
  
  #draw a vowel
  observeEvent(input$drawV, {
    drawnSounds$sounds[drawnSounds$drawn+1] <- sample(vowels, 1)
    drawnSounds$drawn = drawnSounds$drawn + 1
    
    if (drawnSounds$drawn == 1) {
      output$s1 <- renderText({ paste(opening.html, drawnSounds$sounds[1], closing.html) })
    } else if (drawnSounds$drawn == 2) {
      output$s2 <- renderText({ paste(opening.html, drawnSounds$sounds[2], closing.html) })
    } else if (drawnSounds$drawn == 3) {
      output$s3 <- renderText({ paste(opening.html, drawnSounds$sounds[3], closing.html) })
    } else if (drawnSounds$drawn == 4) {
      output$s4 <- renderText({ paste(opening.html, drawnSounds$sounds[4], closing.html) })
    } else if (drawnSounds$drawn == 5) {
      output$s5 <- renderText({ paste(opening.html, drawnSounds$sounds[5], closing.html) })
    } else if (drawnSounds$drawn == 6) {
      output$s6 <- renderText({ paste(opening.html, drawnSounds$sounds[6], closing.html) })
    } else if (drawnSounds$drawn == 7) {
      output$s7 <- renderText({ paste(opening.html, drawnSounds$sounds[7], closing.html) })
    } else if (drawnSounds$drawn == 8) {
      output$s8 <- renderText({ paste(opening.html, drawnSounds$sounds[8], closing.html) })
    } else if (drawnSounds$drawn == 9) {
      output$s9 <- renderText({ paste(opening.html, drawnSounds$sounds[9], closing.html) })
    }
    
    if (drawnSounds$drawn == 9) {
      shinyjs::disable("drawC")
      shinyjs::disable("drawV")
      shinyjs::enable("answers")
    }
  })
  
  #auto-fill sounds
  observeEvent(input$surprise, {

    rnd <- runif(9, 0, 1)
    
    for (i in 1:9) {
      if (rnd[i] <= 0.66) {
        drawnSounds$sounds[i] <- sample(consonants, 1)
      } else {
        drawnSounds$sounds[i] <- sample(vowels, 1)
      }
    }
    
    #drawnSounds$sounds <- c(sample(vowels, 3), sample(consonants, 6))
    drawnSounds$drawn = 9
    
    drawnSounds$sounds <- sample(drawnSounds$sounds)
    
    output$s1 <- renderText({ paste(opening.html, drawnSounds$sounds[1], closing.html) })
    output$s2 <- renderText({ paste(opening.html, drawnSounds$sounds[2], closing.html) })
    output$s3 <- renderText({ paste(opening.html, drawnSounds$sounds[3], closing.html) })
    output$s4 <- renderText({ paste(opening.html, drawnSounds$sounds[4], closing.html) })
    output$s5 <- renderText({ paste(opening.html, drawnSounds$sounds[5], closing.html) })
    output$s6 <- renderText({ paste(opening.html, drawnSounds$sounds[6], closing.html) })
    output$s7 <- renderText({ paste(opening.html, drawnSounds$sounds[7], closing.html) })
    output$s8 <- renderText({ paste(opening.html, drawnSounds$sounds[8], closing.html) })
    output$s9 <- renderText({ paste(opening.html, drawnSounds$sounds[9], closing.html) })
    
    shinyjs::disable("drawC")
    shinyjs::disable("drawV")
    shinyjs::enable("answers")
    
    drawnSounds$text <- NULL
  })
  
  #reset board
  observeEvent(input$reset, {
    drawnSounds$sounds = rep('-', 9)
    drawnSounds$drawn = 0
    
    shinyjs::enable("drawC")
    shinyjs::enable("drawV")
    shinyjs::disable("answers")
    
    drawnSounds$text <- NULL
  })
  
  #dictionary corner!
  observeEvent(input$answers, {
    
    #filter out words containing a sound not in our set
    dict.filt <- dict %>%
      filter(!str_detect(V2, paste(inventory[!inventory %in% drawnSounds$sounds], collapse = '|'))) %>%
      mutate(no_sounds = str_count(V2, " ")+1) %>%
      arrange(desc(no_sounds))
    
    sounds_count <- drawnSounds$sounds %>%
      table() %>% 
      as.data.frame()
    
    count = 0
    
    for (x in 1:nrow(dict.filt)) {
      sounds_compare <- dict.filt[x, ]$V2 %>% 
        str_split(' ') %>% 
        unlist() %>% 
        table() %>% 
        as.data.frame() %>%
        left_join(sounds_count, by='.') %>%
        filter(Freq.x > Freq.y)
      
      if (nrow(sounds_compare) > 0) {
        dict.filt[x, ] <- c(NA, NA, NA)
      } else {
        count = count + 1
      }
      
      if (count == 5) {
        break
      }
      
    }
    
    dict.filt <- dict.filt %>%
      filter(!is.na(V1)) %>%
      head(5)
    
    drawnSounds$text <- paste0(seq(1, 5, 1), '. ', dict.filt$V1[1:5], 
                               ' /', gsub(' ', '', dict.filt$V2[1:5]), '/ (', 
                               dict.filt$no_sounds[1:5], ')')
  })
  
  #info screen
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "IPA Countdown",
      #tags$div(HTML(paste0("Developed by ", tags$a(href="https://www.gbailey.uk/", target="_blank", "George Bailey"), " (follow me on ", tags$a(href="https://twitter.com/grbails/", target="_blank", "Twitter"), "!)"))),
      
      h3('Introduction to the app'),
      tags$div(HTML(paste0("For the uninitiated among you, ", tags$a(href="https://en.wikipedia.org/wiki/Countdown_(game_show)", target="_blank", "Countdown"), " is a classic British gameshow in which contestants randomly draw nine letters from the English alphabet, choosing either a consonant or vowel one at a time, and try to make the longest possible word from them. This app is essentially the same, only it deals with sounds of the ", tags$a(href="https://www.internationalphoneticassociation.org/content/full-ipa-chart", target="_blank", "International Phonetic Alphabet (IPA)"), " and phonemic representations, rather than letters and orthographic representations, and should therefore be useful as a revision exercise for students learning the IPA and studying the phonetics of English in particular (the app currently only supports sounds broadly considered to be in the phonemic inventory of [British] English - a full list is given below)."))),
      
      h3('Using the app'),
      
      "Begin by choosing either a consonant or a vowel, one at a time, until you have a full set of 9 sounds. Alternatively you can press the 'surprise me' button, which automatically draws 9 sounds out at once, with a bias towards consonants (you'll usually end up with about 5-6 consonants and 3-4 vowels). Then, press play on the countdown clock and the game begins! You have 30 seconds to find the longest possible word using only those sounds, and you can only use each sound once (unless of course you draw the same sound out more than once). To start a new game, press the 'reset' button and then you're ready to go again!", tags$br(), tags$br(),
      
      tags$div(HTML(paste0("In the ", tags$strong("Dictionary corner"), " section, you can choose to display the 5 'best' possible words that you could have made with the sounds in play (where 'best' = containing the most sounds). Note that this is based on phonemic transcriptions from the ", tags$a(href="http://svr-www.eng.cam.ac.uk/comp.speech/Section1/Lexical/beep.html", target="_blank", "BEEP pronouncing dictionary"), ", and is therefore based on Southern Standard British English pronunciations, with no regional variants included (e.g. 'bus' is only transcribed as /bʌs/, with no northern British English /bʊs/ entry). The BEEP pronouncing dictionary is a great resource but by no means is it perfect, so be warned that there might be some words absent or some phonemic transcriptions that you disagree with."))),
      
      h3('Technical details'),
      tags$div(HTML(paste0("The probability of drawing out a particular sound is roughly proportional to the frequency with which that sound occurs in the language, as estimated ", tags$a(href="https://cmloegcmluin.wordpress.com/2012/11/10/relative-frequencies-of-english-phonemes/", target="_blank", "here"), ". There are no length diacritics on any of the vowels, so phonological vowel length is ignored for the purposes of this game. Sounds are drawn by sampling (with replacement) from the following inventory:",
      
      h5('Consonants:'),
      "3x /p/, 2x /b/, 5x /t/, 5x /d/, 4x /k/, 1x /g/, 3x /m/, 5x /n/, 2x /ŋ/, 2x /f/, 2x /v/, 1x /θ/, 4x /ð/, 5x /s/, 4x /z/, 1x /ʃ/, 1x /ʒ/, 2x /h/, 1x /tʃ/, 1x /dʒ/, 5x /ɹ/, 1x /j/
, 4x /l/, 2x /w/",
      
      h5('Vowels:'),
      "3x /i/, 4x /ɪ/, 3x /ɛ/, 3x /a/, 4x /ə/, 1x /ɜ/, 2x /u/, 1x /ʊ/, 1x /ʌ/, 1x /ɔ/, 1x /ɑ/, 1x /ɒ/, 2x /eɪ/, 2x /aɪ/, 1x /ɔɪ/, 1x /aʊ/, 1x /əʊ/, 1x /ɪə/, 1x /ʊə/, 1x /ɛə/"
      
      ))),
      easyClose = TRUE,
      footer = modalButton("Dismiss"),
    ))
  })
  
  output$top1 <- renderText({
    if (is.null(drawnSounds$text)) return()
    drawnSounds$text[1]
  })
  output$top2 <- renderText({
    if (is.null(drawnSounds$text)) return()
    drawnSounds$text[2]
  })
  output$top3 <- renderText({
    if (is.null(drawnSounds$text)) return()
    drawnSounds$text[3]
  })
  output$top4 <- renderText({
    if (is.null(drawnSounds$text)) return()
    drawnSounds$text[4]
  })
  
  output$top5 <- renderText({
    if (is.null(drawnSounds$text)) return()
    drawnSounds$text[5]
  })
  
}

shinyApp(ui, server)