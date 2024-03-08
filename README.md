# Chip8-Core

A `no_std`-compatible simulation layer for Chip8 emulation

## Architecture independence

Chip8-Core consists of all the platform-independent and architecture-independent logical components
of a Chip8 emulator/interpreter (the "simulation layer").

It contains all the logic needed to simulate the Chip8 architecture in Rust, in a way that is
agnostic of a concrete implementation of the IO emulation systems.

It can be used either as a dependency, or as a forking base to implement a Chip8 emulator on
virtually any compilation target supported by the Rust toolchain. In particular, being
`no_std`-compatible, it can be used in implementations for embedded devices.

[Chip8-Desktop](https://github.com/st-ario/chip-8-desktop) is a sample implementation of a Chip8
emulator running on Linux and Windows.

## S-Chip and sprite-clipping compatibility

Some Chip8 ROMs expect S-CHIP compatible bit-shift and store/load behavior. Moreover, some ROMs
are meant to wrap around the screen sprites that would end up being rendered off-screen, while
other ROMs just expect for these sprites to be clipped.

Users can choose whether to pick the desired behavior at runtime (by setting the `_schip_compat` and
`_sprite_clipping` flags of a `Chip8` instance), or to compile the library with a built-in choice,
which is mostly useful for embedded implementations.

The default behavior enables the run-time selection of these behaviors, whereas the
`compiletime-compat` feature (implied by `no_std`) requires the user to set the desired behaviors
at compile time, through the `variant-opcodes` and `sprite-clipping` features.
