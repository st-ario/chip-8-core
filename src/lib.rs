const RAM_SIZE: usize = 0xFFF - 0x200; // 0x000 to 0x1FF are reserved for the interpreter
const SCREEN_WIDTH: usize = 64;
const SCREEN_HEIGHT: usize = 32;

pub type FrameBuffer = [[u8; SCREEN_WIDTH / 8]; SCREEN_HEIGHT];

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

/* stored at the beginning of the interpreter area: 0x000 to 0x1FF */
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

pub struct Chip8 {
    framebuffer: FrameBuffer, // SCREEN_WIDTH x SCREEN_HEIGHT on/off pixel
    reg: [u8; 18],            // general purpose registers
    reg_vi: u16,              // 16-bit register, used to store memory addresses
    program_counter: u16,     // pseudo-register "pc"
    stack_pointer: u8,        // pseudo-register "sp"
    call_stack: [u16; 16],    // 16 levels of nested subroutines, panic on stack overflow
    ram: [u8; RAM_SIZE],
    sound_setter: fn(u8) -> (),          // set sound timer register
    time_setter: fn(u8) -> (),           // set delay timer register
    time_getter: fn() -> u8,             // get value of delay timer register
    draw_screen: fn(&FrameBuffer) -> (), // handle draw calls
    is_pressed: fn(u8) -> bool,          // check the key corresponding to the argument
    wait_for_key: fn() -> u8, // suspend execution until a key is pressed, return key value
    rng: fn() -> u8,          // generate random number
}

impl Chip8 {
    pub fn new(
        program: &[u8],
        sound_setter: fn(u8) -> (),
        time_setter: fn(u8) -> (),
        time_getter: fn() -> u8,
        draw_screen: fn(&FrameBuffer) -> (),
        is_pressed: fn(u8) -> bool,
        wait_for_key: fn() -> u8,
        rng: fn() -> u8,
    ) -> Chip8 {
        let mut res = Chip8 {
            framebuffer: [[0; 8]; 32],
            reg: [0; 18],
            reg_vi: 0,
            call_stack: [0; 16],
            program_counter: 0,
            stack_pointer: 0,
            ram: [0; RAM_SIZE],
            sound_setter,
            time_setter,
            time_getter,
            draw_screen,
            is_pressed,
            wait_for_key,
            rng,
        };

        // system initialization
        // 0x000 to 0x1FF: reserved
        res.ram[0..80].copy_from_slice(&DIGITS_SPRITES);
        // programs start at 0x200
        res.ram[0x200..0x200 + program.len()].copy_from_slice(program);
        res.program_counter = 0x200;

        return res;
    }

    pub fn run(&mut self) -> ! {
        loop {
            let counter = self.program_counter as usize;
            let p_instr = &self.ram[counter] as *const u8 as *const u16;
            let instr;
            unsafe {
                instr = *p_instr;
            }
            self.parse_instruction(instr);
            self.program_counter += 2;
        }
    }

    fn parse_instruction(&mut self, instr: u16) {
        match (instr & 0xF000) >> 4 {
            0x0 => match instr & 0x0FF {
                0xE0 => self.cls(),
                0xEE => self.ret(),
                _ => (),
            },
            0x1 => self.jmp(instr & 0xFFF),
            0x2 => self.call(instr & 0xFFF),
            0x3 => self.se(mask_x(instr), mask_8(instr)),
            0x4 => self.sne(mask_x(instr), mask_8(instr)),
            0x5 => match instr & 0x00F {
                0 => self.sevv(mask_x(instr), mask_y(instr)),
                _ => (),
            },
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
            0x9 => match instr & 0x00F {
                0 => self.snev(mask_x(instr), mask_y(instr)),
                _ => (),
            },
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
        self.framebuffer = [[0; 8]; 32];
        (self.draw_screen)(&self.framebuffer);
    }

    fn ret(&mut self) {
        /* 0x00EE - Return from a subroutine */
        self.program_counter = self.call_stack[self.stack_pointer as usize];
        self.stack_pointer -= 1;
    }

    fn jmp(&mut self, addr: u16) {
        /* 0x1NNN - Jump to location */
        self.program_counter = addr;
    }

    fn call(&mut self, addr: u16) {
        /* 0x2NNN - Call subroutine */
        self.stack_pointer += 1;
        self.call_stack[self.stack_pointer as usize] = self.program_counter;
        self.program_counter = addr;
    }

    fn se(&mut self, vx: usize, val: u8) {
        /* 0x3XNN - Skip next instruction if [VX] equal NN */
        if self.reg[vx] == val {
            self.program_counter += 2;
        }
    }

    fn sne(&mut self, vx: usize, val: u8) {
        /* 0x4XNN - skip next instruction if [VX] not equal NN */
        if self.reg[vx] != val {
            self.program_counter += 2;
        }
    }

    fn sevv(&mut self, vx: usize, vy: usize) {
        /* 5XY0 - Skip next instruction if [VX] equal [VY] */
        if self.reg[vx] == self.reg[vy] {
            self.program_counter += 2;
        }
    }

    fn mov(&mut self, vx: usize, val: u8) {
        /* 6XNN - Store */
        self.reg[vx] = val;
    }

    fn add(&mut self, vx: usize, val: u8) {
        /* 7XNN - Add */
        self.reg[vx] += val;
    }

    fn movv(&mut self, vx: usize, vy: usize) {
        /* 8XY0 - Store [VY] in VX */
        self.reg[vx] = self.reg[vy];
    }

    fn or(&mut self, vx: usize, vy: usize) {
        /* 8XY1 - Store [VX] OR [VY] in VX */
        self.reg[vx] |= self.reg[vy];
    }

    fn and(&mut self, vx: usize, vy: usize) {
        /* 8XY2 - Store [VX] AND [VY] in VX */
        self.reg[vx] &= self.reg[vy];
    }

    fn xor(&mut self, vx: usize, vy: usize) {
        /* 8XY3 - Store [VX] XOR [VY] in VX */
        self.reg[vx] ^= self.reg[vy];
    }

    fn addv(&mut self, vx: usize, vy: usize) {
        /* 8XY4 - Store [VX] + [VY] in VX, store boolean carry in VF */
        let res = self.reg[vx].overflowing_add(self.reg[vy]);
        self.reg[vx] = res.0;
        self.reg[0xF] = res.1 as u8;
    }

    fn sub(&mut self, vx: usize, vy: usize) {
        /* 8XY5 - Store [VX] - [VY] in VX, set VF = NOT borrow as bool */
        self.reg[0xF] = (self.reg[vx] > self.reg[vy]) as u8;
        self.reg[vx] = self.reg[vx].wrapping_sub(self.reg[vy]);
    }

    #[cfg(feature = "legacy-opcodes")]
    fn shr(&mut self, vx: usize, vy: usize) {
        /* 8XY6 - Set VX as [VY] >> 1, set VF = least-significant bit before shift */
        self.reg[0xF] = self.reg[vy] & 0x1;
        self.reg[vx] = self.reg[vy] >> 1;
    }

    #[cfg(not(feature = "legacy-opcodes"))]
    fn shr(&mut self, vx: usize, _vy: usize) {
        /* 8XY6 - Set VX as [VX] >> 1, set VF = least-significant bit before shift */
        self.reg[0xF] = self.reg[vx] & 0x1;
        self.reg[vx] >>= 1;
    }

    fn subn(&mut self, vx: usize, vy: usize) {
        /* 8XY7 - Store [VY] - [VX] in VX, set VF = NOT borrow */
        self.reg[0xF] = (self.reg[vy] > self.reg[vx]) as u8;
        self.reg[vx] = self.reg[vy].wrapping_sub(self.reg[vx]);
    }

    #[cfg(feature = "legacy-opcodes")]
    fn shl(&mut self, vx: usize, vy: usize) {
        /* 8XYE - Set VX as [VY] << 1, set VF = most-significant bit before shift */
        self.reg[0xF] = self.reg[vy] & 0b1000_0000;
        self.reg[vx] = self.reg[vy] << 1;
    }

    #[cfg(not(feature = "legacy-opcodes"))]
    fn shl(&mut self, vx: usize, _vy: usize) {
        /* 8XYE - Set VX as [VX] << 1, set VF = most-significant bit before shift */
        self.reg[0xF] = self.reg[vx] & 0b1000_0000;
        self.reg[vx] <<= 1;
    }

    fn snev(&mut self, vx: usize, vy: usize) {
        /* 9XY0 - Skip next instruction if [VX] != [VY] */
        if self.reg[vx] != self.reg[vy] {
            self.program_counter += 2;
        }
    }

    fn movi(&mut self, addr: u16) {
        /* ANNN - Store NNN in VI */
        self.reg_vi = addr;
    }

    fn jmpv(&mut self, addr: u16) {
        /* BNNN - Jump to address NNN + V0, panic if out of RAM bounds */
        self.program_counter = addr + self.reg[0x0] as u16;
    }

    fn rnd(&mut self, vx: usize, val: u8) {
        /* CXNN - Set VX to random byte AND val */
        self.reg[vx] = (self.rng)() & val;
    }

    fn display(&mut self, vx: usize, vy: usize, val: u8) {
        /* DXYN - Draw a sprite at position ([VX], [VY]) with N bytes of sprite data
         * starting at [I].
         * Set VF to 1 if any pixel is switched off, otherwise set it to 0.
         * Sprites are always 8 bits of width, and up to 15 bites of height.
         * Sprites are rendered by XOR-ing with the current framebuffer data.
         * If the program tries to draw with out-of-bounds initial coordinates, the values
         * are reduced mod 32/64. If a sprite overflows the screen boundaries, it's clipped */
        let y = vy % SCREEN_HEIGHT;
        let first_influenced_byte = vx / 8;
        let first_relative_bit = vx % 8;
        let residue = 8 - first_relative_bit;

        let sum = y + val as usize;
        let last_row = if sum > SCREEN_HEIGHT {
            SCREEN_HEIGHT
        } else {
            sum
        };

        let mut current_row = y;
        let mut sprite_row = 0;

        while current_row < last_row {
            let sprite_chunk = self.ram[self.reg_vi as usize + sprite_row];
            let left_chunk = sprite_chunk >> first_relative_bit;
            let right_chunk = sprite_chunk << residue;

            // check whether something will be turned off
            // pixels will turn off if and only if they are a 1 and they will be XORed with a 1
            let flipped_left =
                self.framebuffer[first_influenced_byte][current_row] & left_chunk != 0;
            let flipped_right =
                self.framebuffer[first_influenced_byte + 1][current_row] & right_chunk != 0;

            self.reg[0xF] = (flipped_left | flipped_right) as u8;

            // update framebuffer
            self.framebuffer[first_influenced_byte][current_row] ^= left_chunk;
            self.framebuffer[first_influenced_byte + 1][current_row] ^= right_chunk;

            current_row += 1;
            sprite_row += 1;
        }

        // send draw signal
        (self.draw_screen)(&self.framebuffer);
    }

    fn skp(&mut self, vx: usize) {
        /* EX9E - Skip next instruction if the key corresponding to [VX] is pressed */
        if (self.is_pressed)(self.reg[vx]) {
            self.program_counter += 2;
        }
    }

    fn skpn(&mut self, vx: usize) {
        /* EXA1 - Skip next instruction if the key corresponding to [VX] is not pressed */
        if !(self.is_pressed)(self.reg[vx]) {
            self.program_counter += 2;
        }
    }

    fn ldt(&mut self, vx: usize) {
        /* FX07 - Store [DT] in VX */
        self.reg[vx] = (self.time_getter)();
    }

    fn wmovk(&mut self, vx: usize) {
        /* FX0A - Wait for a key press, store its value in VX */
        let key = (self.wait_for_key)();
        self.reg[vx] = key;
    }

    fn setdt(&mut self, vx: usize) {
        /* FX15 - Set DT to [VX] */
        (self.time_setter)(self.reg[vx]);
    }

    fn setst(&mut self, vx: usize) {
        /* FX18 - Set ST to [VX] */
        (self.sound_setter)(self.reg[vx]);
    }

    fn addi(&mut self, vx: usize) {
        /* FX1E - set VI to [VI] + [VX] */
        self.reg_vi += self.reg[vx] as u16;
    }

    fn ldsprite(&mut self, vx: usize) {
        /* FX29 - Set VI to the address of the sprite for digit [VX] */
        self.reg_vi = 5 * self.reg[vx] as u16;
    }

    fn bcd(&mut self, vx: usize) {
        /* FX33 - Store BCD representation of [VX] in memory locations I, I+1, and I+2.
         * most-significant digit first */
        self.ram[self.reg_vi as usize] = self.reg[vx] & 0b001;
        self.ram[self.reg_vi as usize + 1] = self.reg[vx] & 0b010;
        self.ram[self.reg_vi as usize + 2] = self.reg[vx] & 0b100;
    }

    #[cfg(feature = "legacy-opcodes")]
    fn ldarray(&mut self, vx: usize) {
        /* FX55 - Store values V0 to VX inclusive in memory starting from [VI]
         * set VI to [VI] + X + 1 after the operation */
        let mut i = 0;
        while i <= vx {
            self.ram[self.reg_vi as usize + i] = self.reg[i];
            i += 1;
        }
        self.reg_vi += i;
    }

    #[cfg(not(feature = "legacy-opcodes"))]
    fn ldarray(&mut self, vx: usize) {
        /* FX55 - Store values V0 to VX inclusive in memory starting from [VI] */
        let mut i = 0;
        while i <= vx {
            self.ram[self.reg_vi as usize + i] = self.reg[i];
            i += 1;
        }
    }

    #[cfg(feature = "legacy-opcodes")]
    fn rdarray(&mut self, vx: usize) {
        /* FX65 - Fill registers V0 to VX inclusive with values from memory starting from [VI]
         * set VI to [VI] + X + 1 after the operation */
        let mut i = 0;
        while i <= vx {
            self.reg[i] = self.ram[self.reg_vi as usize + i];
            i += 1;
        }
        self.reg_vi += i;
    }

    #[cfg(not(feature = "legacy-opcodes"))]
    fn rdarray(&mut self, vx: usize) {
        /* FX65 - Fill registers V0 to VX inclusive with values from memory starting from [VI] */
        let mut i = 0;
        while i <= vx {
            self.reg[i] = self.ram[self.reg_vi as usize + i];
            i += 1;
        }
    }
}
