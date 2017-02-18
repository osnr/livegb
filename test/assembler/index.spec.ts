import { parse } from '../../lib/assembler/index';
import 'mocha';
import { expect } from 'chai';

describe('assembler assembles', function() {
  it('adc', function() {
    function assemble(s: string): number[] {
      const result = parse.parse(s);
      if (result.status) {
        return [].concat.apply([], result.value);
      } else {
        throw new Error('Failed to parse: ' + s);
      }
    }
    expect(assemble('adc 4')).to.eql([0xCE, 0x04]);

    const REG_C = 1;
    expect(assemble('adc c')).to.eql([0x88|REG_C]);
  });
});
