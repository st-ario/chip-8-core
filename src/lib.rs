mod framebuffer;
pub use framebuffer::FrameBuffer;
use framebuffer::*;

const RAM_SIZE: usize = 0xFFF;
pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;

/* utility functions to process opcodes */
#[inline(always)]
const fn mask_8(instr: u16) -> u8 {
    (instr & 0xFF) as u8
}
#[inline(always)]
const fn mask_4(instr: u16) -> u8 {
    (instr & 0xF) as u8
}
#[inline(always)]
const fn mask_x(instr: u16) -> usize {
    ((instr & 0xF00) >> 8) as usize
}
#[inline(always)]
const fn mask_y(instr: u16) -> usize {
    ((instr & 0xF0) >> 4) as usize
}

/* stored in RAM at the beginning of the interpreter area: 0x000 to 0x1FF */
const DIGITS_SPRITES: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

const DIGIT_SPRITE_SIZE: u8 = 5;

pub struct IOCallbacks<'a> {
    pub sound_setter: &'a (dyn Fn(u8) + Send + Sync), // set sound timer register
    pub time_setter: &'a (dyn Fn(u8) + Send + Sync),  // set delay timer register
    pub time_getter: &'a (dyn Fn() -> u8 + Send + Sync), // get value of delay timer register
    pub rng: &'a (dyn Fn() -> u8 + Send + Sync),      // generate random number
    pub wait_for_key: &'a (dyn Fn() -> u8 + Send + Sync), // suspend execution until any key is pressed, return key value
    pub is_pressed: &'a (dyn Fn(u8) -> bool + Send + Sync), // check the key corresponding to the argument
}

pub struct Chip8<'a> {
    framebuffer: FrameBufferInternal, // SCREEN_WIDTH x SCREEN_HEIGHT 1-byte bitsets of on/off pixels
    reg: [u8; 16],                    // general purpose registers
    reg_vi: u16,                      // big-endian 16-bit register, used to store memory addresses
    program_counter: u16,             // native endianness, pseudo-register "pc"
    stack_pointer: u8,                // pseudo-register "sp"
    call_stack: [u16; 16], // native endianness, 16 levels of nested subroutines, panic on stack overflow
    callbacks: IOCallbacks<'a>,
    ram: [u8; RAM_SIZE],
}

impl<'a> Chip8<'a> {
    pub fn new(program: &[u8], callbacks: IOCallbacks<'a>) -> Chip8<'a> {
        let mut res = Chip8 {
            framebuffer: FrameBufferInternal::default(),
            reg: [0; 16],
            reg_vi: 0,
            call_stack: [0; 16],
            program_counter: 0,
            stack_pointer: 0,
            ram: [0; RAM_SIZE],
            callbacks,
        };

        /* system initialization */

        // 0x000 to 0x1FF: reserved
        res.ram[0..80].copy_from_slice(&DIGITS_SPRITES);
        // programs start at 0x200
        res.ram[0x200..0x200 + program.len()].copy_from_slice(program);
        res.program_counter = 0x200;

        res
    }

    pub fn fb_ref(&self) -> &FrameBuffer {
        self.framebuffer.as_ref()
    }

    const INSTRUCTION_SIZE: u16 = std::mem::size_of::<u16>() as u16;

    pub fn execute_next_instruction(&mut self) {
        let instr_begin = self.program_counter as usize;
        let instr_end = instr_begin + Chip8::INSTRUCTION_SIZE as usize;

        let instr = u16::from_be_bytes(self.ram[instr_begin..instr_end].try_into().unwrap());

        self.parse_instruction(instr);
        // Each instruction takes care of suitably incrementing the program counter
        // if the program counter overflows RAM, then the program is not well formed
        // and the application panics
    }

    fn parse_instruction(&mut self, instr: u16) {
        match instr >> 12 {
            0x0 => match instr & 0x0FF {
                0xE0 => self.cls(),
                0xEE => self.ret(),
                _ => (),
            },
            0x1 => self.jmp(instr & 0xFFF),
            0x2 => self.call(instr & 0xFFF),
            0x3 => self.se(mask_x(instr), mask_8(instr)),
            0x4 => self.sne(mask_x(instr), mask_8(instr)),
            0x5 => {
                if instr & 0x00F == 0 {
                    self.sevv(mask_x(instr), mask_y(instr))
                }
            }
            0x6 => self.mov(mask_x(instr), mask_8(instr)),
            0x7 => self.add(mask_x(instr), mask_8(instr)),
            0x8 => match instr & 0x00F {
                0x0 => self.movv(mask_x(instr), mask_y(instr)),
                0x1 => self.or(mask_x(instr), mask_y(instr)),
                0x2 => self.and(mask_x(instr), mask_y(instr)),
                0x3 => self.xor(mask_x(instr), mask_y(instr)),
                0x4 => self.addv(mask_x(instr), mask_y(instr)),
                0x5 => self.sub(mask_x(instr), mask_y(instr)),
                0x6 => self.shr(mask_x(instr), mask_y(instr)),
                0x7 => self.subn(mask_x(instr), mask_y(instr)),
                0xE => self.shl(mask_x(instr), mask_y(instr)),
                _ => (),
            },
            0x9 => {
                if instr & 0x00F == 0 {
                    self.snev(mask_x(instr), mask_y(instr))
                }
            }
            0xA => self.movi(instr & 0xFFF),
            0xB => self.jmpv(instr & 0xFFF),
            0xC => self.rnd(mask_x(instr), mask_8(instr)),
            0xD => self.display(mask_x(instr), mask_y(instr), mask_4(instr)),
            0xE => match instr & 0x0FF {
                0x9E => self.skp(mask_x(instr)),
                0xA1 => self.skpn(mask_x(instr)),
                _ => (),
            },
            0xF => match instr & 0x0FF {
                0x07 => self.ldt(mask_x(instr)),
                0x0A => self.wmovk(mask_x(instr)),
                0x15 => self.setdt(mask_x(instr)),
                0x18 => self.setst(mask_x(instr)),
                0x1E => self.addi(mask_x(instr)),
                0x29 => self.ldsprite(mask_x(instr)),
                0x33 => self.bcd(mask_x(instr)),
                0x55 => self.ldarray(mask_x(instr)),
                0x65 => self.rdarray(mask_x(instr)),
                _ => (),
            },
            _ => (),
        }
    }

    /* instructions */

    fn cls(&mut self) {
        /* 0x00E0 - Clear screen */
        self.framebuffer = FrameBufferInternal::default();
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn ret(&mut self) {
        /* 0x00EE - Return from a subroutine */
        self.stack_pointer -= 1;
        self.program_counter = self.call_stack[self.stack_pointer as usize];
    }

    fn jmp(&mut self, addr: u16) {
        /* 0x1NNN - Jump to location */
        self.program_counter = addr;
    }

    fn call(&mut self, addr: u16) {
        /* 0x2NNN - Call subroutine */

        // save the instruction to be executed _after_ return
        self.call_stack[self.stack_pointer as usize] =
            self.program_counter + Chip8::INSTRUCTION_SIZE;
        self.stack_pointer += 1;
        self.program_counter = addr;
    }

    fn se(&mut self, vx: usize, val: u8) {
        /* 0x3XNN - Skip next instruction if [VX] equal NN */
        self.program_counter += if self.reg[vx] == val {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn sne(&mut self, vx: usize, val: u8) {
        /* 0x4XNN - skip next instruction if [VX] not equal NN */
        self.program_counter += if self.reg[vx] != val {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn sevv(&mut self, vx: usize, vy: usize) {
        /* 5XY0 - Skip next instruction if [VX] equal [VY] */
        self.program_counter += if self.reg[vx] == self.reg[vy] {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn mov(&mut self, vx: usize, val: u8) {
        /* 6XNN - Store */
        self.reg[vx] = val;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn add(&mut self, vx: usize, val: u8) {
        /* 7XNN - Add */
        self.reg[vx] = self.reg[vx].wrapping_add(val);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn movv(&mut self, vx: usize, vy: usize) {
        /* 8XY0 - Store [VY] in VX */
        self.reg[vx] = self.reg[vy];
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn or(&mut self, vx: usize, vy: usize) {
        /* 8XY1 - Store [VX] OR [VY] in VX */
        self.reg[vx] |= self.reg[vy];
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn and(&mut self, vx: usize, vy: usize) {
        /* 8XY2 - Store [VX] AND [VY] in VX */
        self.reg[vx] &= self.reg[vy];
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn xor(&mut self, vx: usize, vy: usize) {
        /* 8XY3 - Store [VX] XOR [VY] in VX */
        self.reg[vx] ^= self.reg[vy];
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn addv(&mut self, vx: usize, vy: usize) {
        /* 8XY4 - Store [VX] + [VY] in VX, store boolean carry in VF */
        let res = self.reg[vx].overflowing_add(self.reg[vy]);
        self.reg[vx] = res.0;
        self.reg[0xF] = res.1 as u8;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn sub(&mut self, vx: usize, vy: usize) {
        /* 8XY5 - Store [VX] - [VY] in VX, set VF = NOT borrow as bool */
        self.reg[0xF] = (self.reg[vx] > self.reg[vy]) as u8;
        self.reg[vx] = self.reg[vx].wrapping_sub(self.reg[vy]);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(not(feature = "variant-opcodes"))]
    fn shr(&mut self, vx: usize, vy: usize) {
        /* 8XY6 - Set VX as [VY] >> 1, set VF = least-significant bit before shift */
        self.reg[0xF] = self.reg[vy] & 0x1;
        self.reg[vx] = self.reg[vy] >> 1;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(feature = "variant-opcodes")]
    fn shr(&mut self, vx: usize, _vy: usize) {
        /* 8XY6 - Set VX as [VX] >> 1, set VF = least-significant bit before shift */
        self.reg[0xF] = self.reg[vx] & 0x1;
        self.reg[vx] >>= 1;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn subn(&mut self, vx: usize, vy: usize) {
        /* 8XY7 - Store [VY] - [VX] in VX, set VF = NOT borrow */
        self.reg[0xF] = (self.reg[vy] > self.reg[vx]) as u8;
        self.reg[vx] = self.reg[vy].wrapping_sub(self.reg[vx]);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(not(feature = "variant-shift-opcodes"))]
    fn shl(&mut self, vx: usize, vy: usize) {
        /* 8XYE - Set VX as [VY] << 1, set VF = most-significant bit before shift */
        self.reg[0xF] = self.reg[vy] & 0b1000_0000;
        self.reg[vx] = self.reg[vy] << 1;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(feature = "variant-shift-opcodes")]
    fn shl(&mut self, vx: usize, _vy: usize) {
        /* 8XYE - Set VX as [VX] << 1, set VF = most-significant bit before shift */
        self.reg[0xF] = self.reg[vx] & 0b1000_0000;
        self.reg[vx] <<= 1;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn snev(&mut self, vx: usize, vy: usize) {
        /* 9XY0 - Skip next instruction if [VX] != [VY] */
        self.program_counter += if self.reg[vx] != self.reg[vy] {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn movi(&mut self, addr: u16) {
        /* ANNN - Store NNN in VI */
        self.reg_vi = u16::to_be(addr);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn jmpv(&mut self, addr: u16) {
        /* BNNN - Jump to address NNN + V0, panic if out of RAM bounds */
        let dest = addr + self.reg[0x0] as u16;
        self.program_counter = dest;
    }

    fn rnd(&mut self, vx: usize, val: u8) {
        /* CXNN - Set VX to random byte AND val */
        self.reg[vx] = (self.callbacks.rng)() & val;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn display(&mut self, vx: usize, vy: usize, val: u8) {
        /* DXYN - Draw a sprite at position ([VX], [VY]) with N bytes of sprite data
         * starting at [I].
         * Set VF to 1 if any pixel is switched off, otherwise set it to 0.
         * Sprites are always 8 bits of width, and up to 15 bites of height.
         * Sprites are rendered by XOR-ing with the current framebuffer data.
         * If the program tries to draw with out-of-bounds initial coordinates, the values
         * are reduced mod 32 or mod 64, depending on the direction.
         * If a sprite overflows the screen boundaries, it's clipped */
        let x = self.reg[vx] as usize % SCREEN_WIDTH;
        let y = self.reg[vy] as usize % SCREEN_HEIGHT;
        let first_influenced_byte = x / 8;
        let byte_offset = x % 8;

        let sum = y + val as usize;
        let last_row = if sum > SCREEN_HEIGHT {
            SCREEN_HEIGHT
        } else {
            sum
        };

        let mut current_row = y;
        let mut sprite_row = 0;
        let addr = u16::from_be(self.reg_vi);

        /* framebuffer update */
        while current_row < last_row {
            let sprite_chunk = self.ram[addr as usize + sprite_row];
            let left_chunk = sprite_chunk >> byte_offset;

            // there is a right chunk only if the left chunk is offset
            // moreover, if the right chunk would fall off the screen, it's just ignored
            let valid_right_chunk =
                byte_offset != 0 && first_influenced_byte < SCREEN_WIDTH_IN_U8 - 1;

            let right_chunk = if valid_right_chunk {
                Some(sprite_chunk << (8 - byte_offset))
            } else {
                None
            };

            // check whether something will be turned off
            // pixels will turn off if and only if they are a 1 and they will be XORed with a 1
            let flipped_left =
                self.framebuffer.data[current_row][first_influenced_byte] & left_chunk != 0;
            let flipped_right = if let Some(rc) = right_chunk {
                self.framebuffer.data[current_row][first_influenced_byte + 1] & rc != 0
            } else {
                false
            };

            self.reg[0xF] = (flipped_left | flipped_right) as u8;

            // update framebuffer
            self.framebuffer.data[current_row][first_influenced_byte] ^= left_chunk;
            if let Some(rc) = right_chunk {
                self.framebuffer.data[current_row][first_influenced_byte + 1] ^= rc;
            }

            current_row += 1;
            sprite_row += 1;
        }

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn skp(&mut self, vx: usize) {
        /* EX9E - Skip next instruction if the key corresponding to [VX] is pressed */
        self.program_counter += if (self.callbacks.is_pressed)(self.reg[vx]) {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn skpn(&mut self, vx: usize) {
        /* EXA1 - Skip next instruction if the key corresponding to [VX] is not pressed */
        self.program_counter += if !(self.callbacks.is_pressed)(self.reg[vx]) {
            2 * Chip8::INSTRUCTION_SIZE
        } else {
            Chip8::INSTRUCTION_SIZE
        };
    }

    fn ldt(&mut self, vx: usize) {
        /* FX07 - Store [DT] in VX */
        self.reg[vx] = (self.callbacks.time_getter)();
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn wmovk(&mut self, vx: usize) {
        /* FX0A - Wait for a key press, store its value in VX */
        let key = (self.callbacks.wait_for_key)();
        self.reg[vx] = key;
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn setdt(&mut self, vx: usize) {
        /* FX15 - Set DT to [VX] */
        (self.callbacks.time_setter)(self.reg[vx]);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn setst(&mut self, vx: usize) {
        /* FX18 - Set ST to [VX] */
        (self.callbacks.sound_setter)(self.reg[vx]);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn addi(&mut self, vx: usize) {
        /* FX1E - set VI to [VI] + [VX] */
        let current = u16::from_be(self.reg_vi);
        let addr = current.wrapping_add(self.reg[vx] as u16);

        self.reg_vi = u16::to_be(addr);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn ldsprite(&mut self, vx: usize) {
        /* FX29 - Set VI to the address of the sprite for digit [VX] */
        let addr = DIGIT_SPRITE_SIZE as u16 * self.reg[vx] as u16;
        self.reg_vi = u16::to_be(addr);
        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    fn bcd(&mut self, vx: usize) {
        /* FX33 - Store BCD representation of [VX] in memory locations I, I+1, and I+2.
         * most-significant digit first */
        let val = self.reg[vx];
        let hundreds = val / 100;
        let units = val % 10;
        let tens = val / 10 - hundreds * 10;

        let addr = u16::from_be(self.reg_vi) as usize;

        self.ram[addr] = hundreds;
        self.ram[addr + 1] = tens;
        self.ram[addr + 2] = units;

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(not(feature = "variant-opcodes"))]
    fn ldarray(&mut self, vx: usize) {
        /* FX55 - Store values V0 to VX inclusive in memory starting from [VI]
         * set VI to [VI] + X + 1 after the operation */
        let addr = u16::from_be(self.reg_vi) as usize;

        let mut i = 0;
        while i <= vx {
            self.ram[addr + i] = self.reg[i];
            i += 1;
        }

        let res = u16::from_be(self.reg_vi) + i as u16;
        self.reg_vi = u16::to_be(res);

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(feature = "variant-opcodes")]
    fn ldarray(&mut self, vx: usize) {
        /* FX55 - Store values V0 to VX inclusive in memory starting from [VI] */
        let addr = u16::from_be(self.reg_vi) as usize;

        let mut i = 0;
        while i <= vx {
            self.ram[addr + i] = self.reg[i];
            i += 1;
        }

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(not(feature = "variant-opcodes"))]
    fn rdarray(&mut self, vx: usize) {
        /* FX65 - Fill registers V0 to VX inclusive with values from memory starting from [VI]
         * set VI to [VI] + X + 1 after the operation */
        let addr = u16::from_be(self.reg_vi) as usize;

        let mut i = 0;
        while i <= vx {
            self.reg[i] = self.ram[addr + i];
            i += 1;
        }

        let res = u16::from_be(self.reg_vi) + i as u16;
        self.reg_vi = u16::to_be(res);

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }

    #[cfg(feature = "variant-opcodes")]
    fn rdarray(&mut self, vx: usize) {
        /* FX65 - Fill registers V0 to VX inclusive with values from memory starting from [VI] */
        let addr = u16::from_be(self.reg_vi) as usize;

        let mut i = 0;
        while i <= vx {
            self.reg[i] = self.ram[addr + i];
            i += 1;
        }

        self.program_counter += Chip8::INSTRUCTION_SIZE;
    }
}
