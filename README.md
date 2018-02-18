# Plate generator for 3d split keyboard

3d split keyboard with emphasis on thumb *(finger brother)* usage. 

Currently this repo contains plate generator which produces OpenScad models. I
am working on body design in parametric Cad and experiment with 3d printing.

## Current progress

![Main plate](doc/img/main_plate.png?raw=true)
![Thumb plate](doc/img/thumb_plate.png?raw=true)

At some point I became excited by resulting plates and tried to generate the
whole body like this:

![Demo](doc/img/body.png?raw=true)

I dropped this idea and decided that it's better to use some parametric cad
system.

## Similar projects 

There are many similar projects. Explore
[MechanicalKeyboards](https://www.reddit.com/r/MechanicalKeyboards) subreddit
for more examples.

Keyboard with separate parts for each hand:
+ https://www.ergodox.io/
+ https://imgur.com/a/mwTFj

"3d keyboard":
+ https://imgur.com/gallery/lcP4s
+ https://www.reddit.com/r/MechanicalKeyboards/comments/7gzpe6/photos_testing_my_new_40_3d_split_layout/

Here is 3d split keyboard where author wrote generator for 3d shape:
https://github.com/adereth/dactyl-keyboard

## Design goals

My idea is very similar to those projects, I want:
1. minimalist keyboard with small amount of keys
2. split keyboard, parts stand almost vertically
3. custom keys position convenient to my hands
4. separate keys-pad for thumb located orthogonal to main plate.

Purpose of the design:
1. minimalism is to minimize hands movement
2. purpose of such plate position is to relax wrists
3. don't sure if it critical, but If I design my own keyboard, I can optimize
   for my body
3. orthogonal location of thumb-plate is something unique in this setup so I
   will discuss it in details.

## Orthogonal thumb plate position

Thumb have been discriminated since invention of the keyboard. It is the most
useful *(most agile)* finger, but it's potential is not used for typing. The
reason for this is that thumb is nearly orthogonal to other fingers so thumb
struggles to press keys located on the same plane with keys designed for other
fingers. With this project I want to explore thumb potential. 

* improved typing speed and comfort achieved by moving Esc, Ret and other
  emacs-vim-heavy non-character keys to thumb pad;
* one hand typing achieved either by moving lots of keys to thumb pad or by
  implementing modal keys *(similar to FN key from laptop keyboard)*.
