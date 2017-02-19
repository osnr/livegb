# livegb

## Inspiration

There are a lot of ideas floating around about how to build more powerful programming environments.

Can you edit the program while it's running?

Can you have a debugger that remembers old states of your program, so you can scrub back and forth in time?

Can you have the information you want at hand as you're editing and navigating the code -- runtime information, not just static source text?

The Game Boy is interesting because it has some familiar software -- Pokémon, Mario, and so on -- but it's a small enough system that you can do a lot of weird, inefficient tricks.

It has a 4 MHz processor and 8 KB of RAM.

So if we want to go back in time, we can just copy the whole computer state; it's not a big deal!

## How it works

A code editor and emulator appear in your browser. The whole development system runs in the browser, so you can edit and play without leaving your browser window.

You edit the Game Boy assembly on the left, and it reloads the ROM pretty much instantly on the right, as you're typing.

## What I did

I wrote a completely new Game Boy assembler so it would run in the browser, so it would run fast enough to compile in real time, and so I could instrument the output properly.

The emulator is an existing JS Game Boy emulator (patched a little to support real-time ROM changes), and the demo is a small one-file demo someone wrote.

## Future directions

Hot-swap is not really hot-swap right now, just fast rerun (the demo game is mostly stateless anyway).
Smarter hot-swap (with a custom assembler and more emulator tools, we can track provenance of regions of RAM, etc). Or do input replay instead.

Track provenance of regions of display, so you click a pixel and it knows where it came from in the code.

Get it working with the [full Pokémon Red disassembly](https://github.com/pret/pokered).

Share a link to your edited game, possibly including your place in it, with other people.

Export your edited game as a ROM.
