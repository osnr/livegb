import 'mocha';
import { expect } from 'chai';

import { readFileSync } from 'fs';

import * as Assembler from '../../lib/assembler/index';

describe('assembler assembles', function() {
  function assemble(s: string): number[] {
    const result = Assembler.parse.parse(s);
    if (result.status) {
      return [].concat.apply([], result.value);
    } else {
      console.error(result);
      throw new Error('Failed to parse: ' + s);
    }
  }

  it('adc', function() {
    expect(assemble('adc 4')).to.eql([0xCE, 0x04]);

    const REG_C = 1;
    expect(assemble('adc c')).to.eql([0x88|REG_C]);
  });

  it('binop', function() {
    expect(assemble('ex hl, [sp]')).to.eql([0xE3]);
  });

  it('vblank test', function() {
    assemble('reti');
    assemble(`
vblank:
  push af
  push bc
  push de
  push hl

  ; Note that the DMA procedure must be initiated from High RAM. The
  ; mechanism for that is detailed alongside the definition of this
  ; initiation procedure.
  call hram_sprite_dma
  call show_window
  call scroll_bg

  ld   a,1
  ld   [VBFLAG],a

  pop  hl
  pop  de
  pop  bc
  pop  af
  reti
`);
  });

  it('avik das', function() {
    const asm = readFileSync('test/assembler/avik-das-sprite.asm', 'utf8');
    const result = Assembler.parse.parse(asm);
    console.log(result);
    expect(result.status).to.equal(true);
  });

  // Slower!
  // it('avik das linewise', function() {
  //   const asm = readFileSync('test/assembler/avik-das-sprite.asm', 'utf8');
  //   const result = Assembler.linewiseParse.parse(asm);
  //   console.log(result);
  //   expect(result.status).to.equal(true);
  // });
});
