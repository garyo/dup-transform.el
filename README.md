# dup-transform.el
Simple Emacs package for graphics programmers, to duplicate and transform rgb/xy lines.

Graphics programmers often need to write near-duplicate code to process rgb or rgba pixels, or xy/xyz coordinates.
This package aims to make that a little more friction-free by auto-detecting common patterns and duplicating the lines with the proper transformations.

## Cycling lines

Given this:
```c++
    projection_dims.x = std::lround((float)projection_dims.x * downsample.x);
```
with point on that line, run `M-x dup-transform` to get this:
```c++
    projection_dims.x = std::lround((float)projection_dims.x * downsample.x);
    projection_dims.y = std::lround((float)projection_dims.y * downsample.y);
```

or this:
```c++
    for (x = 0; x < xdim; x++) {
      pixel[x].r = bg->r;
    }
```
into this:
```c++
    for (x = 0; x < xdim; x++) {
      pixel[x].r = bg->r;
      pixel[x].g = bg->g;
      pixel[x].b = bg->b;
    }
```
or even:
```c++
      pixel[x].r = bg_pixel[0]; // red
```

into this:
```c++
      pixel[x].r = bg_pixel[0]; // red
      pixel[x].g = bg_pixel[1]; // green
      pixel[x].b = bg_pixel[2]; // blue
      pixel[x].a = bg_pixel[3]; // alpha
```

with one keystroke.

It also works with the region, if one is selected.

## Cycling Words

`dup-transform` also includes a command `M-x dup-transform-cycle-word` to "cycle" the word at point, if it's r/g/b/a, red/green/blue/alpha or x/y/z. Prefix arg cycles by that many elements; specifically `M-- M-x dup-transform-cycle-word` cycles backwards. This way you can copy/paste in the usual way, and quickly update the r/g/b/a symbols.

If you enable `dup-transform-mode`, a minor mode included with this package, it'll bind `dup-transform-cycle-word` to `C-c <up>` with an auto-repeat on `<up>` or `<down>` arrow keys.

## Installation

It's not on MELPA yet, so if you're using `use-package` or one of its variants, just do something like this:
```elisp
(use-package dup-transform
  :ensure (:host github :repo "garyo/dup-transform.el")
  :hook ((prog-mode . dup-transform-mode)
)
```
This will enable `dup-transform-mode` in all programming modes.

## Customization

`dup-transform` tries to be a bit smart about how many copies you want. If it sees something that looks like rgb, it'll make three copies total. Otherwise it'll make two (assuming you're a 2d graphics coder, and it's doing xy coords). You can customize both of those defaults. If you mostly do RGBA (red/green/blue/alpha) work, then `M-x customize` the `dup-transform` group and set `dup-transform-rgb-default-n` to 4. If you do a lot of 3d graphics, set `dup-transform-xy-default-n` to 3.

It's not fancy, but if you're a graphics coder, it may save you some time. I bind it to `C-c x`.

