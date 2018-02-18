# Plate generator for 3d split keyboard

3d split keyboard with emphasis on thumb usage. 

This repo contains plate generator which produces OpenScad models.

Currently I am working on [body design](https://cad.onshape.com/documents/637317aecfc4aec4822f128e/w/aec18f0c194858b7c392cf7a/e/b1da523669b7d7269cc1f27a) in parametric Cad.

## Current progress

![Main plate](doc/img/main_plate.png?raw=true)
![Thumb plate](doc/img/thumb_plate.png?raw=true)

I tried to generate the whole body like this:

![Demo](doc/img/body.png?raw=true)

But I dropped this idea and switched to parametric cad system.

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

My idea is quite similar to those projects:
1. minimalist keyboard with small amount of keys
2. split keyboard, parts stand almost vertically
3. custom keys position convenient for me
4. separate keys-pad for thumb located orthogonal to main plate.

Purpose of the design:
1. minimize hands movement
2. vertical plate position should relax wrists
3. just because I can
3. see below

## Orthogonal thumb plate position

Thumb has been discriminated for many decades starting from the invention of the
keyboard. It is the agilest and hence most useful digit. Sadly enough, it's
potential is almost wasted during typing. The reason for that is that thumb is
positioned nearly orthogonal to fingers. Obviously, thumb struggles to press
keys located on the same plane with finger keys.

With this project, I want to explore thumb potential. 

Potential benefits are:
* improved typing speed and comfort achieved by moving Esc, Ret and other
  emacs-vim-heavy non-character keys to thumb pad;
* one hand typing achieved either by moving lots of keys to thumb pad or by
  implementing modal keys *(similar to FN key from laptop keyboard)*.
