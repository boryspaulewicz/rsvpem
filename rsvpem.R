## Koster: T1 był poz, neu lub neg, po 9 samoopisowych przymiotników,
## T2 zawsze neu. Słowa T1 miały długość 4-6 znaków, wybrane w oparciu
## o oceny emocji i familiarności. T2 był wybierany z zestawu 27 słów
## neutralnych 3-6 znaków. Każde ze słów T1 i T2 powtórzyło sie 5
## razy. Jako dystraktory użyto zesta 79 słów mało znanych, 9-18
## znaków. 10 prób treningowych i 135 właściwych (135 / 27 = 5).

## Fajny zestaw słów do zadania pamięciowego lub att blinka jest w
## grant.sonata/pilot/memtrain

source('~/cs/code/r/tasks/task/task.R')
db.connect('task')
TASK.NAME <<- 'rsvpem'

FIXATION.TIME = 500
POST.FIXATION.TIME = 500
PRESENTATION.TIME = 96
ISI = 16
NOF.ITEMS = 13

## Wczytujemy słowa z bazy i przygotowujemy zestaw bodźców
words = readRDS('nawl.rds')
words = words[words$Gram == 3,]
words$val = st(words$val_M_all)
neg = words$NAWL_word[words$val < -1.5][1:40]
neu = words$NAWL_word[abs(words$val) < .4]
pos = words$NAWL_word[abs(words$val) > 1.5][1:40]
rm(words)

WINDOW$set.visible(T)
WINDOW$set.mouse.cursor.visible(T)

FX = fixation(WINDOW, size = .02)

trial.code = function(trial, t1em = sample(c('neg', 'neu', 'pos'), 1), t1pos = sample(3:5, 1), t2lag = sample(c(2, 4, 6), 1)){
    ## Kod specyficzny dla zadania
    ## ...
    ## Szablon
    t2pos = t1pos + t2lag
    ## Mieszamy
    neg = sample(neg)
    word = sample(neu)
    pos = sample(pos)
    ## if(trial == 1){
    state = 'press-space'
    ## }else{ state = 'show-fixation' }
    if(WINDOW$is.open())process.inputs()
    start = CLOCK$time
    while(WINDOW$is.open()){
        process.inputs()
        ## Możliwość wyjścia z etapu za pomocą ESC
        if(KEY.PRESSED[Key.Escape + 1] > start)return(NULL)
        ## Kod specyficzny dla zadania
        switch(state, 'press-space' = {
            TXT$set.string("Naciśnij spację")
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
                item = 1
            }
        }, 'show-stim' = {
            WINDOW$clear(c(0, 0, 0))
            ## Targety rysujemy na zielono
            if(item %in% c(t1pos, t2pos)){
                TXT$set.color(c(0, 1, 0))
            }else{
                TXT$set.color(c(1, 1, 1))
            }
            ## Pierwszy target ma kontrolowaną walencję
            if(item == t1pos){
                stim = list(neg = neg, neu = word, pos = pos)[[t1em]][item]
            }else{
                stim = word[item]
            }
            TXT$set.string(stim)
            bounds = TXT$get.local.bounds()
            TXT$set.origin(c(bounds['width'] / 2, bounds['top'] + bounds['height'] / 2))
            TXT$set.position(WINDOW$get.size() / 2)
            WINDOW$draw(TXT)
            WINDOW$display()
            stim.onset = CLOCK$time
            state = 'stim-present'
        }, 'stim-present' = {
            if((CLOCK$time - stim.onset) >= PRESENTATION.TIME){
                WINDOW$clear(c(0, 0, 0))
                WINDOW$display()
                stim.cleared = CLOCK$time
                state = 'stim-cleared'
            }
        }, 'stim-cleared' = {
            if((CLOCK$time - stim.cleared) >= ISI){
                if(item <= NOF.ITEMS){
                    item = item + 1
                    state = 'show-stim'
                }else{
                    state = 'done'
                }
            }
        }, 'done' = {
            WINDOW$set.visible(F)
            value1 = gui.get.value("", "Słowo 1")
            value2 = gui.get.value("", "Słowo 2")
            WINDOW$set.visible(T)
            WINDOW$clear(c(0, 0, 0))
            WINDOW$display()
            res = list(value1 = value1, value2 = value2)
            return(res)
        })
    }
}

gui.show.instruction("Za chwilę pojawi się okno danych osobowych")
## gui.user.data()
USER.DATA = list(name = 'admin', age = 37, gender = 'M')
cnd = 'same-emotion' ## db.random.condition(c('same-emotion', 'diff-emotion', 'same-imagine', 'diff-imagine'))
run.trials(trial.code, expand.grid(t1em = c('neg', 'neu', 'pos'), t1pos = 3:5, t2lag = c(2, 4, 6)), condition = cnd)
if(!interactive())quit("no")