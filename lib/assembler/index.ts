import { string, regexp, sepBy, lazy, seq, alt,
  whitespace, optWhitespace, Parser } from 'parsimmon';

function symbol(s: string) { return string(s); }
const space = whitespace;
const optSpace = optWhitespace;

const const_8bit = regexp(/[0-9]+/).map(x => parseFloat(x));
const const_3bit = regexp(/[0-9]+/).map(x => parseFloat(x));
const const_16bit = regexp(/[0-9]+/).map(x => parseFloat(x));

const reg_r = alt(
  symbol('b').result(0),
  symbol('c').result(1),
  symbol('d').result(2),
  symbol('e').result(3),
  symbol('h').result(4),
  symbol('l').result(5),
  symbol('[hl]').result(6),
  symbol('a').result(7)
);

const reg_ss = alt(
  symbol('bc').result(0),
  symbol('de').result(1),
  symbol('hl').result(2),
  symbol('sp').result(3)
);

const reg_rr = alt(
  symbol('[bc]').result(0),
  symbol('[de]').result(1),
  symbol('[hl+]').result(2),
  symbol('[hl-]').result(3)
);

const reg_tt = alt(
  symbol('bc').result(0),
  symbol('de').result(1),
  symbol('hl').result(2),
  symbol('af').result(3)
);

const ccode = alt(
  symbol('nz').result(0),
  symbol('z').result(1),
  symbol('nc').result(2),
  symbol('c').result(3)
);

function indirect<A>(p: Parser<A>): Parser<A> {
  return string('[').then(p).skip(string(']'));
}
function $ff00_plus<A>(p: Parser<A>): Parser<A> {
  return indirect(symbol('$ff00+').then(p));
}

function unaryOp<A>(name: string, argP: Parser<A>): Parser<A> {
  return symbol(name).skip(space).then(argP);
}
function binaryOp<A, B>(name: string, argP1: Parser<A>, argP2: Parser<B>): Parser<[A, B]> {
  return symbol(name).skip(space).then(seq(
    argP1
      .skip(string(','))
      .skip(optSpace),
    argP2
  ));
}

type Z80 = number;

function nullaryCpuOp(name: string, byte: Z80): Parser<Z80[]> {
  return symbol(name).result([byte]);
}
function unaryCpuOp<A>(name: string, argP: Parser<A>, map: (arg: A) => Z80[]): Parser<Z80[]> {
  return unaryOp(name, argP).map(map);
}
function binaryCpuOp<A, B>(name: string, argP1: Parser<A>, argP2: Parser<B>, map: (arg1: A, arg2: B) => Z80[]): Parser<Z80[]> {
  return binaryOp(name, argP1, argP2).map(([a, b]) => map(a, b));
}
function arithmeticCpuOp(name: string, immPrefixByte: Z80, argPrefixBits: Z80): Parser<Z80[]> {
  // immPrefixByte -- 1 byte. next byte is the immediate
  // argPrefixBits -- upper few bits. bottom bits specify which reg

  // TODO: Support "adc a, b" syntax.
  return unaryOp(name, alt(
    const_8bit.map(n => [immPrefixByte, n]), // op_a_n
    reg_r.map(r => [argPrefixBits|r]) // op_a_r
  ));
}
function rotateCpuOp(name: string, aByte: Z80, rPrefixBits: Z80) {
  return alt(
    unaryOp(name, reg_r).map(r => [0xCB, rPrefixBits|r]),
    nullaryCpuOp(name + 'a', aByte)
  );
}

function append16(arr: Z80[], nn: number): Z80[] {
  return arr.concat([(nn>>8)&0xFF, nn&0xFF]);
}

const cpuOp: Parser<Z80[]> = alt(
  // ADC n; ADC r
  arithmeticCpuOp('adc', 0xCE, 0x88),
  // ADD n; ADD r
  arithmeticCpuOp('add', 0xC6, 0x80),
  // ADD HL, ss
  binaryCpuOp('add', symbol('hl'), reg_ss,
    (_, ss) => [0x09|(ss<<4)]),
  // ADD SP, n
  binaryCpuOp('add', symbol('sp'), const_8bit,
    (_, n) => [0xE8, n]),
  // AND n; AND r
  arithmeticCpuOp('and', 0xE6, 0xA0),
  // BIT n3, r
  binaryCpuOp('bit', const_3bit, reg_r,
    (n3, r) => [0xCB, 0x40|(n3<<3)|r]),
  // CALL cc, nn
  binaryCpuOp('call', ccode, const_16bit,
    (cc, nn) => append16([0xC4|(cc<<3)], nn)),
  // CALL nn
  unaryCpuOp('call', const_16bit,
    nn => append16([0xCD], nn)),
  // CCF
  nullaryCpuOp('ccf', 0x3F),
  // CP n; CP r
  arithmeticCpuOp('cp', 0xFE, 0xB8),
  // CPL
  nullaryCpuOp('cpl', 0x2F),
  // DAA
  nullaryCpuOp('daa', 0x27),
  // DEC r
  unaryCpuOp('dec', reg_r, r => [0x05|(r<<3)]),
  // DEC ss
  unaryCpuOp('dec', reg_ss, ss => [0x0B|(ss<<4)]),
  // DI
  nullaryCpuOp('di', 0xF3),
  // EI
  nullaryCpuOp('ei', 0xFB),
  // EX HL, (SP)
  binaryCpuOp('ex', symbol('hl'), indirect(symbol('sp')), () => [0xE3]),
  // HALT
  nullaryCpuOp('halt', 0x76),
  // INC r
  unaryCpuOp('inc', reg_r, r => [0x04|(r<<3)]),
  // INC ss
  unaryCpuOp('inc', reg_ss, ss => [0x03|(ss<<4)]),
  // JP (HL)
  unaryCpuOp('jp', indirect(symbol('hl')), () => [0xE9]),
  // JP cc, nn
  binaryCpuOp('jp', ccode, const_16bit,
    (cc, nn) => append16([0xC2|(cc<<3)], nn)),
  // JP nn
  unaryCpuOp('jp', const_16bit, nn => append16([0xC3], nn)),
  // JR n
  unaryCpuOp('jr', const_8bit, n => [0x18, n]),
  // JR cc, n
  binaryCpuOp('jr', ccode, const_8bit,
    (cc, n) => [0x20|(cc<<3), n]),
  // LD (nn), SP
  binaryCpuOp('ld', indirect(const_16bit), symbol('sp'),
    (nn, _) => append16([0x08], nn)),
  // LD ($FF00+C), A
  binaryCpuOp('ld', $ff00_plus(symbol('c')), symbol('a'), () => [0xE2]),
  // LD ($FF00+n), A
  binaryCpuOp('ld', $ff00_plus(const_8bit), symbol('a'), (n, _) => [0xE0, n]),
  // LD (nn), A
  binaryCpuOp('ld', indirect(const_16bit), symbol('a'),
    (nn, _) => append16([0xEA], nn)),
  // LD (rr), A
  binaryCpuOp('ld', reg_rr, symbol('a'),
    (rr, _) => [0x02|(rr<<4)]),
  // LD A, ($FF00+C)
  binaryCpuOp('ld', symbol('a'), $ff00_plus(symbol('c')), () => [0xF2]),
  // LD A, ($FF00+n)
  binaryCpuOp('ld', symbol('a'), $ff00_plus(const_8bit), (_, n) => [0xF0, n]),
  // LD A, (nn)
  binaryCpuOp('ld', symbol('a'), indirect(const_16bit),
    (_, nn) => append16([0xFA], nn)),
  // LD A, (rr)
  binaryCpuOp('ld', symbol('a'), reg_rr, (_, rr) => [0x0A|(rr<<4)]),
  // LD HL, (SP+n)
  binaryCpuOp('ld', symbol('hl'),
    indirect(symbol('sp +').or(symbol('sp+')).then(const_8bit)),
    (_, n) => [0xF8, n]),
  // LD SP, HL
  binaryCpuOp('ld', symbol('sp'), symbol('hl'), () => [0xF9]),
  // LD r, n
  binaryCpuOp('ld', reg_r, const_8bit, (r, n) => [0x06|(r<<3), n]),
  // LD r, r'
  binaryCpuOp('ld', reg_r, reg_r, (r1, r2) => [0x40|(r1<<3)|(r2)]),
  // LD ss, nn
  binaryCpuOp('ld', reg_ss, const_16bit, (ss, nn) => append16([0x01|(ss<<4)], nn)),
  // NOP
  nullaryCpuOp('nop', 0x00),
  // OR n; OR r
  arithmeticCpuOp('or', 0xF6, 0xB0),
  // POP tt
  unaryCpuOp('pop', reg_tt, tt => [0xC1|(tt<<4)]),
  // PUSH tt
  unaryCpuOp('push', reg_tt, tt => [0xC5|(tt<<4)]),
  // RES n3, r
  binaryCpuOp('res', const_3bit, reg_r, (n3, r) => [0xCB, 0x80|(n3<<3)|r]),
  // RET
  nullaryCpuOp('ret', 0xC9),
  // RET cc
  unaryCpuOp('ret', ccode, cc => [0xC0|(cc<<3)]),
  // RETI
  nullaryCpuOp('reti', 0xD9),
  // RL r; RLA
  rotateCpuOp('rl', 0x10, 0x17),
  // RLC r; RLCA
  rotateCpuOp('rlc', 0x00, 0x07),
  // RR r; RRA
  rotateCpuOp('rr', 0x18, 0x1F),
  // RRC r; RRCA
  rotateCpuOp('rrc', 0x08, 0x0F),
  // RST n
  unaryCpuOp('rst', const_8bit, n => [0xC7|n]),
  // SBC n; SBC r
  arithmeticCpuOp('sbc', 0xDE, 0x98),
  // SCF
  nullaryCpuOp('scf', 0x37),
  // SET n3, r
  binaryCpuOp('set', const_3bit, reg_r, (n3, r) => [0xCB, 0xC0|(n3<<3)|r]),
  // SLA r
  unaryCpuOp('sla', reg_r, r => [0xCB, 0x20|r]),
  // SRA r
  unaryCpuOp('sra', reg_r, r => [0xCB, 0x28|r]),
  // SRL r
  unaryCpuOp('srl', reg_r, r => [0xCB, 0x38|r]),
  // STOP
  nullaryCpuOp('stop', 0x10),
  // SUB n; SUB r
  arithmeticCpuOp('sub', 0xD6, 0x90),
  // SWAP r
  unaryCpuOp('swap', reg_r, r => [0xCB, 0x30|r]),
  // XOR n; XOR r
  arithmeticCpuOp('xor', 0xEE, 0xA8)
);

const id = regexp(/[a-zA-z_][a-zA-Z0-9_\\@#][a-zA-Z0-9_\\@#]*/);

const label = alt(
  string(''),
  id,
  id.then(string(':')),
  id.then(string('::'))
);


const line = alt(
  label.then(cpuOp)/*,
  label.then(macro),
  label.then(simplePseudoOp),
  pseudoOp*/
);

const lines: Parser<any> = sepBy(line as any, string('\n'));

export const parse = lines;
