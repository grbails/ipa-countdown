library(tidyverse)

unique.con <- c('p', 'b', 't', 'd', 'k', 'ɡ', 
               'm', 'n', 'ŋ',
               'f', 'v', 'θ', 'ð', 's', 'z', 'ʃ', 'ʒ', 'h',
               'ɹ', 'j', 'l', 'w', 'tʃ', 'dʒ')

unique.vow <- c('i', 'ɪ', 'ɛ', 'æ', 'ə', 'ɜ', 'u', 'ʊ', 'ʌ', 'ɔ', 'ɑ', 'ɒ',
                'eɪ', 'ɑɪ', 'ɔɪ', 'ɑʊ', 'əʊ', 'ɪə', 'ʊə', 'ɛə')

inventory <- c(unique.con, unique.vow)

dict <- read.delim("beep.txt", header=F, stringsAsFactors = F) %>%
  mutate(V1 = tolower(V1))

#convert BEEP transcription into IPA symbols
dict <- dict %>%
  mutate(V2 = str_replace_all(V2, c('ng' = 'ŋ', 'ch' = 'tʃ', 'eh' = 'ɛ', 'g' = 'ɡ', 'th' = 'θ', 'dh' = 'ð',
                                    'sh' = 'ʃ', 'zh' = 'ʒ', 'hh' = 'h', 'jh' = 'dʒ',
                                    'iy' = 'i', 'ih' = 'ɪ', 'eh' = 'ɛ', 'ae' = 'æ', 'ax' = 'ə', 'er' = 'ɜ',
                                    'uw' = 'u', 'uh' = 'ʊ', 'ah' = 'ʌ', 'ao' = 'ɔ', 'aa' = 'ɑ', 'oh' = 'ɒ',
                                    'ey' = 'eɪ', 'ay' = 'ɑɪ', 'oy' = 'ɔɪ', 'aw' = 'ɑʊ', 'ow' = 'əʊ', 'ia' = 'ɪə',
                                    'ua' = 'ʊə', 'ea' = 'ɛə', 'r' = 'ɹ', 'y' = 'j')))

ipa_to_check <- c('tʃ', 'ɛ', 'n', 'p', 'b', 'ɡ', 'ɪ', 'ɜ')

#dict.test <- dict %>%
#  filter(V1 %in% c('pig', 'church', 'big', 'pen', 'test', 'banter', 'wrong', 'face'))

#filter out words containing a sound not in our set
dict.filt <- dict %>%
  filter(!str_detect(V2, paste(inventory[!inventory %in% ipa_to_check], collapse = '|')))

sounds_count <- ipa_to_check %>%
  table() %>% 
  as.data.frame()

for (x in 1:nrow(dict.filt)) {
  sounds_compare <- dict.filt[x, ]$V2 %>% 
    str_split(' ') %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame() %>%
    left_join(sounds_count, by='.') %>%
    filter(Freq.x > Freq.y)
  
  if (nrow(sounds_compare) > 0) {
    dict.filt[x, ] <- c(NA, NA)
  }
  
}

dict.filt <- dict.filt %>%
  filter(!is.na(V1)) %>%
  mutate(no_sounds = str_count(V2, " ")+1) %>%
  arrange(desc(no_sounds))
