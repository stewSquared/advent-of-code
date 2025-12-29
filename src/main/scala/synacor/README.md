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

### 2025-12-15 -- Part 2 -- Getting a Runnable VM Emulator

- Added typesafe number system for handling words as literals, addresses, or register references. Backed by Int with opaque types.
- Added proper memory model
- Added proper register bank model
- Implemented all Opcodes except IN
- Loaded binary memory and ran a welcome message

### 2025-12-16 -- Part 3 -- Entering the Text Adventure

- passed the power on self test
- completed IN opcode
- wired together a console
- solved a maze in a text adventure
- FSM for state transition

### 2025-12-17 -- Part 4 -- Separating VM state from execution

- finished the coin/math puzzle in the text adventure
- reached the "end" of the tex adventure
- learned that we need to hack the game
- added an algebraic data type for VM Ticks
- wired together an emulator layer for easier interaction with state

### 2025-12-18 -- Part 5 -- Time Travel

- added the ability to undo commands
- added a command history
- add the ability to replay a history
- unlocked time travel
- added ability to set/get register
- tried teleporting with a different register value

### 2025-12-18 -- Part 6 -- Live Bytecode Inspection

- added ability to print a VM state's currently running instruction
- added an oplog to our emulator
- recorded instructions that run when using the teleporter

### 2025-12-23 -- Part 7 -- Bypassing the confirmation mechanism

- added proper instruction modeling
- added method for extracting functions
- found ackerman function being called
- bypassed ackerman
- found new area!
- found a journal

### 2025-12-24 -- Part 7 -- Vault Puzzle and Bytecode Extraction

- filtered running bytecode
- explored vault <<<
- tried dijkstra/bfs to find solution
- probably need to use math?
- overflow problem?
- diffed orb vs orbless walking
- extracted "- 9" bytecode, four functions of interest <<<
- still need to reverse ackerman function <<<

### 2025-12-28 -- Part 7

- ackerman reverse engineered properly
- turns out, it has a different contstant
- need to find the correct R8 value for that constant
- narrowed down vault puzzle ranges
- state space small enough, no need for search order strategies
- accidentally prevented reaching goal -- bug is found

## Wishlist

- [X] Interface for entering input into running VM
- [X] Interact with power-on self test!
- [X] type params for valid state transitions?
- [?] use a state monad and transformer?
- [X] ADT Algebraic data type for VM Ticks
- [/] Emulate at higher level so we can explore/diff/inject VM states
- [X] ability to rewind state
- [X] ability to replay commands
- [X] ability to set the 8th register
- [/] log running bytecode and PC locations between two states
  - [X] turning on/off the printing
  - [?] write to file?
- [X] PRINT currently running instruction
- [X] Count ticks between commands
- [X] model the assembly language? - useful for ASM DSL, or parsing
- [X] Add hooks for R8 access
- [X] Read the journal, scout ahead in the new area
- [ ] Figure out correct R8 value for ackerman
- [ ] optimize ackermann with constant R8 value
- [/] disassemble bytecode into something readable => reverse engineering
- [ ] functions indpendent from repl command for worksheet
- [/] ability to inspect state // or play with worksheet
- [ ] diff two states?
- [/] nice error types?
- [?] ASM DSL?
- [?] Compiler?
- [?] disassemble to HLL
- [?] VM State Transition AST / Machine Code Interpreter AST?
