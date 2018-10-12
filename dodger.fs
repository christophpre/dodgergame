\ Title:    Dodger Game - Stackbasierte Sprachen 2017W
\ Authors:  Andreas Scheidl
\           Christoph Presch

needs ansi.fs
needs random.fs

variable dodger-pos                 \ x position of the dodger on the bottom line
variable dodger-symbol              \ saves the ASCII code for the dodger symbol
variable enemy-symbol               \ saves the ASCII code for the enemy symbol
variable difficulty                 \ saves the current difficulty
variable score                      \ saves the current score
variable lines                      \ saves the current number of lines
variable is-pause                   \ saves the pause state of the game
variable shield-symbol              \ saves the ASCII code for the shield powerup symbol
variable speed-reduce-symbol        \ saves the ASCII code for the speed reduce powerup symbol
variable speed-increase-symbol      \ saves the ASCII code for the speed increase powerup symbol
variable score-symbol               \ saves the ASCII code for the additional score symbol
variable shield                     \ saves the boolean state whether the dodger currently has a shield powerup or not

rows 1 -  Constant MAXROW           \ defines the height of the game board
cols 1 -  Constant MAXCOL           \ defines the width of the game board
68        Constant DODGER_STANDARD  \ defines the ASCII code of the standard dodger symbol
83        Constant DODGER_SHIELD    \ defines the ASCII code of the dodger symbol with active shield
8         Constant MAX_DIFFICULTY   \ defines the maximum difficulty (speed) of the game
32        Constant k-space          \ defines the ASCII value for the space key-event
121       Constant k-y              \ defines the ASCII value for the y key-event
110       Constant k-n              \ defines the ASCII value for the n key-event

DODGER_STANDARD dodger-symbol !     \ initializes the ASCII code for the dodger symbol
88 enemy-symbol !                   \ initializes the ASCII code for the enemy symbol
36 score-symbol !                   \ initializes the ASCII code for the extra score symbol
33 shield-symbol !                  \ initializes the ASCII code for the shield symbol
73 speed-increase-symbol !          \ initializes the ASCII code for the speed increase powerup symbol
82 speed-reduce-symbol !            \ initializes the ASCII code for the speed reduce powerup symbol

false       shield !                \ initializes the shield boolean state
20          difficulty !            \ initializes the starting difficulty of the game
0 MAXROW -  score !                 \ initializes the score of the game - only gets positive when first enemy is reached
0 MAXROW -  lines !                 \ initializes the survived lines of the game
false       is-pause !              \ initializes the boolean state for the pause state of the game

\ usage save: value xindex yindex enemy-grid !
\ usage read: xindex yindex enemy-grid @
: 2dim-array
  create ( width height "name" ) over ,  * cells allot
  does> ( x y -- addr ) dup cell+ >r  @ * + cells r> + ;

MAXCOL MAXROW 2dim-array enemy-grid \ initialize enemy data structure

: initialize ( -- ) \ initializes the screen
  page
  MAXCOL 2 / dodger-pos ! ;

: print-line ( row -- )
  { row }
  MAXCOL 0 u+do
    i row enemy-grid @ .
  loop ;

: draw-symbol ( s -- )
  dup
  case
    enemy-symbol @ of
      emit
    endof
    score-symbol @ of
      <a yellow >fg a> attr! emit
    endof
    shield-symbol @ of
      <a cyan >fg a> attr! emit
    endof
    speed-increase-symbol @ of
      <a red >fg a> attr! emit
    endof
    speed-reduce-symbol @ of
      <a green >fg a> attr! emit
    endof
  2drop
  endcase
  <a white >fg a> attr! ;

: draw-dodger ( -- ) \ draws the dodger on each tick
  dodger-pos @ MAXROW 1 - at-xy dodger-symbol @ emit ;

: draw-line ( row -- ) \ draws a line at a row
  { row }
  0 row at-xy
  MAXCOL 0 u+do
    i row enemy-grid @
    0 = if
      space
    else
      i row enemy-grid @ draw-symbol
    endif
  loop
  cr ;

: draw-grid ( -- ) \ draws the enemies on each tick
  MAXROW 0 u+do
    i draw-line
  loop ;

: draw-pause ( -- )
  MAXCOL 2 / 10 - MAXROW 2 / 3 - at-xy ." ####################"
  MAXCOL 2 / 10 - MAXROW 2 / 2 - at-xy ." #                  #"
  MAXCOL 2 / 10 - MAXROW 2 / 1 - at-xy ." #   Game paused!   #"
  MAXCOL 2 / 10 - MAXROW 2 /     at-xy ." #                  #"
  MAXCOL 2 / 10 - MAXROW 2 / 1 + at-xy ." ####################" ;

: pause ( -- )
  is-pause @ false = if
    draw-pause
    true is-pause !
  else
    false is-pause !
  endif ;

: clear-row ( row -- )
  MAXCOL 0 u+do
    0 i 0 enemy-grid !
  loop ;

: spawn-symbol ( symbol -- )
  MAXCOL random 0 enemy-grid ! ;

: spawn-enemies ( -- )
  clear-row
  enemy-symbol @ spawn-symbol ;

: spawn-powerups ( -- )
  rnd 13 mod 0 = if
    shield-symbol @ spawn-symbol
  endif
  rnd 11 mod 0 = if
    score-symbol @ spawn-symbol
  endif
  rnd 20 mod 0 = if
    speed-increase-symbol @ spawn-symbol
  endif 
  rnd 20 mod 0 = if
    speed-reduce-symbol @ spawn-symbol
  endif ;

: wait ( ms -- ) \ waits for an amount of miliseconds to create the game tick
  ms ;

: move-left ( -- )
  dodger-pos @ 0 <> if
    dodger-pos @ MAXROW 1 - at-xy space
    dodger-pos @ 1 - dodger-pos !
  endif ;

: move-right ( -- )
  dodger-pos @ MAXCOL 1 - <> if
    dodger-pos @ MAXROW 1 - at-xy space
    dodger-pos @ 1 + dodger-pos !
  endif ;

: move-enemy-row ( row -- )
  dup 0 swap enemy-grid
  MAXCOL 0 u+do
    2dup @ swap 1 + i swap enemy-grid !
    cell+
  loop
  2drop ;

: move-enemies ( -- )
  MAXROW 1 + 2 u+do
    MAXROW i - move-enemy-row
  loop ;

: check-input ( -- )
  ekey? if
    is-pause @ false = if
      ekey case
        k-left of
          move-left
        endof
        k-right of
          move-right
        endof
        k-space of
          pause
        endof
      endcase
    else
      ekey case
        k-space of
          pause
        endof
      endcase
    endif
  endif ;

: delete-symbol ( col row -- )
  0 -rot enemy-grid ! ;

: enemy-collision ( -- b )
  shield @ true = if \ consumes shield
    false shield !
    DODGER_STANDARD dodger-symbol !
    dodger-pos @ MAXROW 1 - delete-symbol
    false  
  else \ game over
    true   
  endif ;

: shield-collision ( -- b )
  true shield !
  DODGER_SHIELD dodger-symbol !
  dodger-pos @ MAXROW 1 - delete-symbol
  false ;

: score-collision ( -- b )
  score @ 100 + score !
  dodger-pos @ MAXROW 1 - delete-symbol
  false ;

: speed-increase-collision ( -- b )
  difficulty @ 10 - dup MAX_DIFFICULTY > if
    difficulty !
  else
    drop MAX_DIFFICULTY difficulty !
  endif
  dodger-pos @ MAXROW 1 - delete-symbol
  false ;

: speed-reduce-collision ( -- b ) 
  difficulty @ 10 + difficulty !
  dodger-pos @ MAXROW 1 - delete-symbol
  false ;

: check-collision ( -- b )
 dodger-pos @ MAXROW 1 - enemy-grid @
 case
   enemy-symbol @ of            \ collision with enemy
     enemy-collision
   endof
   shield-symbol @ of           \ gain shield
     shield-collision
   endof
   score-symbol @ of            \ gain additional score
     score-collision
   endof
   speed-increase-symbol @ of   \ increase difficulty
     speed-increase-collision
   endof
   speed-reduce-symbol @ of     \ reduce difficulty
     speed-reduce-collision
   endof
 false
 endcase ;

: gameover ( -- )
  MAXCOL 2 / 10 - MAXROW 2 / 3 - at-xy ." ####################"
  MAXCOL 2 / 10 - MAXROW 2 / 2 - at-xy ." #    Game over!    #"
  MAXCOL 2 / 10 - MAXROW 2 / 1 - at-xy ." #    Score: " score @ .
  MAXCOL 2 / 9 + MAXROW 2 / 1 -  at-xy ." #"
  MAXCOL 2 / 10 - MAXROW 2 /     at-xy ." #                  #"
  MAXCOL 2 / 10 - MAXROW 2 / 1 + at-xy ." ####################" ;

: continue ( -- b )
  MAXCOL 2 / 7 - MAXROW 2 / at-xy ." Continue? (y/n)"
  key
  case
    k-y of
      true
    endof
    k-n of
      false
    endof
  recurse
  endcase ;

: increase-difficulty ( -- )
  lines @ 50 mod 0= if
    difficulty @ 10 - dup MAX_DIFFICULTY > if
      difficulty !
    else
      drop MAX_DIFFICULTY difficulty !
    endif
  endif ;

: draw-score ( -- )
  score @ 0 > if
    score @
  else
    0
  endif
  0 MAXROW at-xy ." Score " . ."  Difficulty " difficulty @ . ;

: game-loop ( -- b )
  0 begin
    draw-dodger
    is-pause @ false = if
      1 + dup difficulty @ mod 0= if
 	      increase-difficulty
        spawn-powerups
        move-enemies
        spawn-enemies
        draw-grid
        lines @ 1 + lines !
        score @ 1 + score !
        draw-score
        drop 0
 	    endif
    endif
    10 wait
    check-input
    check-collision
  until
  gameover
  continue ;

: start ( -- )
  initialize game-loop if
    recurse
  else
    page
    bye
  endif ;

start
