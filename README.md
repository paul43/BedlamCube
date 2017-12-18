# BedlamCube

This project contains my old hacky attempt to write a [Bedlam Cube](https://en.wikipedia.org/wiki/Bedlam_cube) solver using recursion in F#, inspired by Tomas Petricek's [blog about solving a snake puzzle](http://tomasp.net/blog/2014/puzzling-fsharp/).

It works pretty well, finding new solutions every few seconds on my laptop. There's a bug in it somewhere that means it only finds about 99% of all solutions that exist to the puzzle but at this point I'm not bothered enough to track it down.

The code is a bit of a mess as remenants of a couple of different strategies persist and all the bit-hacks and optimisations pretty much obfuscate wat was once quite a neat loop. If I ever blog about it I'll have to start from scratch with a clean slate.

## Potential ToDos:

* Multithreading -- It's an essentially sequential search so it's not clear this would work
* GUI -- It would be nice to pop up animated cubes as we find solutions. Should be easily adapted from TomasP's blog post.

#### Potential Gotchas

* Crashes with stack overflow in Debug mode due to deep recursion. Have to compile in release mode for optimised tail calls.
