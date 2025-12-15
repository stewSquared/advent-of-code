# Synacor Challenge

This directory contains my work for completing the [Synacor Challenge](http://github.com/stewSquared/synacor-challenge) as a [live coding stream](https://www.youtube.com/playlist?list=PLnP_dObOt-rVv2-4VHCeLHy6JKph18abo).

Synacor is a multi-layered VM reverse-engineering puzzle created by Eric Wastl for OSCON in 2012, years before he released Advent of Code. We're given a VM specification and binary file and left to explore and unlock all the puzzles contained inside. This will likely involve emulation, interpreters, reverse engineering, and who knows what else.

I'm sharing my problem solving and design approach live on stream! If you'd like to see my thought process and bounce ideas with me, you can catch me on [Twitch](https://twitch.tv/stewSquared) and [YouTube](https://youtube.com/@stewSquared) just before midnight on weekdays.

## Stream Notes

### 2025-12-14 -- Part 1 -- Introduction and Modeling

- laid the **framework** for our VM emulator, following the approach from my Intcode solutions for Advent of Code 2019.

- Modeled opcodes as an enum. We can figure out parsing later when we clean up the number system.

- Modeled state as an immutable case class with a program counter, registers, stack, and memory. These are all modeled with placeholder types aliases for now, but we want to keep implementation details away from our model code. Added helpers for state transitions that Opcodes effect.

- Modeled the unsigned 16-bit numbers as a case class containing the low and high bytes. We probably just want an opaque type over Int with bitmasking, because translating things to little endian is cumbersome and error-prone.

- Implemented opcodes while slowly growing a DSL. We still need to introduce typesafety for words, proper memory and register handling, and Opcodes related to the call stack.

## Roadmap

- [ ] Write a roadmap
