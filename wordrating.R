## Procedura: każda osoba oceniała 291, każde ze słów na wszystkich
## wymiarach. Przebieg:
##
## 1s lub 2s prezentacja słowa na środku
##
## słowo pomniejszone nieco wyżej i poniżej skala 7-punktowa, a właściwie
## trzy skale na raz.
##
## Pewne reguły na temat losowania kategorii słów.
source('../task.R')
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

## Parametry i obiekty skali
bar.width = .8
bar = center(new(RectangleShape, c(WINDOW$get.size()[1] * bar.width, 2)), WINDOW)
bar.height = .05
bar.left = center(new(RectangleShape, c(2, WINDOW$get.size()[2] * bar.height)), WINDOW)
bar.right = center(new(RectangleShape, c(2, WINDOW$get.size()[2] * bar.height)), WINDOW)
bar.middle = center(new(RectangleShape, c(2, WINDOW$get.size()[2] * bar.height)), WINDOW)
bar.left$set.position(c(bar$get.global.bounds()['left'], bar$get.position()['y']))
bar.middle$set.position(c((bar$get.global.bounds() %*% c(1, 0, .5, 0))[,1], bar$get.position()['y']))
bar.right$set.position(c((bar$get.global.bounds() %*% c(1, 0, 1, 0))[,1], bar$get.position()['y']))
bar.point = center(new(CircleShape, .005 * WINDOW$get.size()[1], 20), WINDOW)
scales = list('znak emocji' = c("BARDZO NEGATYWNE", "NEGATYWNE", "NEUTRALNE", "POZYTYWNE", "BARDZO POZYTYWNE"),
    'pobudzenie' = c('ŚNIĘTY JAK RYBA', 'RACZEJ ŚNIĘTY', 'TAKI SE', 'POBUDZONY', 'DRŻĄCE CIAŁO'))

draw.scale = function(labels){
    ## Rysujemy kreskę poziomą
    WINDOW$draw(bar)
    ## Rysujemy trzy kreski pionowe
    for(b in c(bar.left, bar.middle, bar.right))WINDOW$draw(b)
    ## Rysujemy punkt wskazywany przez myszkę
    mp = mouse.get.position()
    gb = bar$get.global.bounds()
    bar.point.pos = c(min(max(mp['x'], gb['left']), gb['left'] + gb['width']),
                      bar$get.position()['y'])
    bar.point.val = ((bar.point.pos[1] - gb['left']) / gb['width'])
    bar.point$set.position(bar.point.pos)
    ## bar.point$set.fill.color(c(0, bar.point.val, 1 - bar.point.val))
    WINDOW$draw(bar.point)
    ## Rysujemy tekst odpowiadający punktowi
    TXT$set.string(labels[which(bar.point.val <= (1:length(labels)) / length(labels))[1]])
    center(TXT, WINDOW)$move(c(0, WINDOW$get.size()[2] * .1))
    WINDOW$draw(TXT)
    bar.point.val
}

trial.code = function(trial, gram = 3, item = 1, samegender = 1){
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
        if((samegender && (USER.DATA$gender == 'K')) ||
           ((!samegender) && (USER.DATA$gender == 'M'))){
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
                values = rep(-1, length(scales))
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                scale = 1
                scale.onset = CLOCK$time
                state = 'rating'
            }
        }, 'rating' = {
            WINDOW$clear(c(0, 0, 0))
            TXT$set.string(word)
            center.win(TXT)$move(c(0, WINDOW$get.size()[2] * -.2))
            WINDOW$draw(TXT)
            TXT$set.string(names(scales)[scale])
            center.win(TXT)$move(c(0, WINDOW$get.size()[2] * .05))
            WINDOW$draw(TXT)
            value = draw.scale(scales[[scale]])
            WINDOW$display()
            if(BUTTON.PRESSED[1] > scale.onset){
                values[scale] = value
                scale.onset = CLOCK$time
                if(scale == length(scales)){
                    state = 'done'
                }else{
                    scale = scale + 1
                }
            }
        }, 'done' = {
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(originalword = original.word, word = word)
            for(s in 1:length(scales))res[[paste('v', s, sep = '')]] = values[s]
            return(res)
        })
    }
}

## gui.show.instruction("Za chwilę pojawi się okno danych osobowych")
## init.session()
## SESSION.ID <<- 0
TASK.NAME <<- 'wordrating'
gui.user.data()
## USER.DATA = list(name = 'admin', age = 37, gender = 'M')
## source('./condition/cfg.R')
scenario = list()
for(g in 1:3)scenario[[g]] = sample(which(words$gram == g), NOF.TRIALS)
run.trials(trial.code, expand.grid(gram = 3, item = 1:2, samegender = c(T, F)))
if(!interactive())quit("no")
