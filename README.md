# dup-transform.el
Simple Emacs package for graphics programmers, to duplicate and transform rgb/xy lines.

Graphics programmers often need to write near-duplicate code to process rgb or rgba pixels, or xy/xyz coordinates.
This package aims to make that a little more friction-free by auto-detecting common patterns and duplicating the lines with the proper transformations.

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

## Customization

`dup-transform` tries to be a bit smart about how many copies you want. If it sees something that looks like rgb, it'll make three copies total. Otherwise it'll make two (assuming you're a 2d graphics coder, and it's doing xy coords). You can customize both of those defaults. If you mostly do RGBA (red/green/blue/alpha) work, then `M-x customize` the `dup-transform` group and set `dup-transform-rgb-default-n` to 4. If you do a lot of 3d graphics, set `dup-transform-xy-default-n` to 3.

It's not fancy, but if you're a graphics coder, it may save you some time. I bind it to `C-c x`.
