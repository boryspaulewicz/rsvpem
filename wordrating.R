## Procedura: każda osoba oceniała 291, każde ze słów na wszystkich
## wymiarach. Przebieg:
##
## 1s lub 2s prezentacja słowa na środku
##
## słowo pomniejszone nieco wyżej i poniżej skala 7-punktowa, a właściwie
## trzy skale na raz.
##
## Pewne reguły na temat losowania kategorii słów.
source('../task/task.R')
TASK.NAME <<- 'wordrating'

library(data.table)

NOF.TRIALS = 291
FIXATION.TIME = 1000
POST.FIXATION.TIME = 1000
PRESENTATION.TIME = 2000

## Wczytujemy słowa z bazy
words = data.table(db.query.csv('select * from words;'))
setorder(words, count)

WINDOW$set.visible(T)
WINDOW$set.mouse.cursor.visible(T)

FX = fixation(WINDOW, size = .02)

scales = list(emotion = c('Jakie emocje itd...', 'Bardzo negatywne', 'Negatywne', 'Neutralne', 'Pozytywne', 'Bardzo pozytywne'),
              imagine = c('Jak łatwo sobie wyobrazić...', 'Bardzo trudno', 'Trudno', 'Ani ani', 'Łatwo', 'Bardzo łatwo'))

trial.code = function(trial, gram = 3, item = 1, samegender = 'same', scale = 'emotion'){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    if(trial == 1){
        state = 'press-space'
    }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    ## Losujemy słowo do oceny
    word = original.word = words$word[scenario[[gram]][trial]]
    ## Ewentualna zmiana genderu słowa
    if(gram == 3){
        if(((samegender == 'same') && (USER.DATA$gender == 'K')) ||
           ((samegender != 'same') && (USER.DATA$gender == 'M'))){
            word = str_replace_all(word, 'y$', 'a')
        }
    }
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Proszę nacisnąć spację aby rozpocząć")
            center.win(TXT)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$draw(TXT)
            WINDOW$display()
            if(KEY.RELEASED[Key.Space + 1] > start){
                state = 'show-fixation'
            }
        }, 'show-fixation' = {
            WINDOW$clear(c(0, 0, 0))
            lapply(FX, WINDOW$draw)
            WINDOW$display()
            state = 'clear-fixation'
            fixation.start = CLOCK$time
        }, 'clear-fixation' = {
            if((CLOCK$time - fixation.start) > FIXATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                state = 'post-fixation'
                fixation.cleared = CLOCK$time
            }
        }, 'post-fixation' = {
            if((CLOCK$time - fixation.cleared) > POST.FIXATION.TIME){
                state = 'show-stim'
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            TXT$set.string(word)
            center.win(TXT)
            WINDOW$draw(TXT)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) > PRESENTATION.TIME){
                value = -1
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                scale.onset = CLOCK$time
                state = 'rating'
            }
        }, 'rating' = {
            WINDOW$clear(c(0, 0, 0))
            ## Rysujemy słowo
            TXT$set.string(word)
            center.win(TXT)$move(c(0, WINDOW$get.size()[2] * -.2))
            WINDOW$draw(TXT)
            ## Pytanie dla skali (np. jak łatwo jest sobie wyobrazić...)
            TXT$set.string(scales[[scale]][1])
            center.win(TXT)$move(c(0, WINDOW$get.size()[2] * .02))
            WINDOW$draw(TXT)
            value = draw.scale(scales[[scale]][-1])
            WINDOW$display()
            if(BUTTON.PRESSED[1] > scale.onset)state = 'done'
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(originalword = original.word, word = word, value = value)
            return(res)
        })
    }
}

gui.show.instruction("Za chwilę pojawi się okno danych osobowych")
## gui.user.data()
USER.DATA = list(name = 'admin', age = 37, gender = 'M')
cnd = db.random.condition(c('same-emotion', 'diff-emotion', 'same-imagine', 'diff-imagine'))
scenario = list()
for(g in 1:3)scenario[[g]] = sample(which(words$gram == g), NOF.TRIALS)
run.trials(trial.code, expand.grid(scale = str_split(cnd, '-')[[1]][2],
                                   samegender = str_split(cnd, '-')[[1]][1],
                                   gram = 3, item = 1:2),
           condition = cnd)
if(!interactive())quit("no")
