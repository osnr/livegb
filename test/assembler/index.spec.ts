import 'mocha';
import { expect } from 'chai';

import { readFileSync } from 'fs';

import { assemble } from '../../lib/assembler/index';

describe('assembler assembles', function() {
  it('adc', function() {
    expect(assemble('adc 4')).to.eql([0xCE, 0x04]);

    const REG_C = 1;
    expect(assemble('adc c')).to.eql([0x88|REG_C]);
  });

  it('binop', function() {
    expect(assemble('ex hl, [sp]')).to.eql([0xE3]);
  });

  // Slower!
  // it('avik das linewise', function() {
  //   const asm = readFileSync('test/assembler/avik-das-sprite.asm', 'utf8');
  //   const result = Assembler.linewiseParse.parse(asm);
  //   console.log(result);
  //   expect(result.status).to.equal(true);
  // });
});
