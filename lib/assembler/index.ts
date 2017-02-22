import { string, regexp, sepBy, lazy, seq, alt, takeWhile, succeed, seqMap,
  Parser } from 'parsimmon';
import * as Parsimmon from 'parsimmon';
import { macroPass } from './macro';
import { math } from './math';

function symbol(s: string) { return string(s); }
const space = regexp(/[ \t]+/);
const optSpace = regexp(/[ \t]*/);

const id = regexp(/[a-zA-z_\.][a-zA-Z0-9_\\@#][a-zA-Z0-9_\\@#]*/);

const stringLiteral = string('"').then(takeWhile(c => c !== '"')).skip(string('"'));
const numberLiteral = alt(
  regexp(/[0-9]+/).map(x => parseInt(x, 10)),
  string('$').then(regexp(/[0-9a-fA-F]+/)).map(x => parseInt(x, 16)),
  string('%').then(regexp(/[0-1]+/)).map(x => parseInt(x, 2))
);

const const_any: Parser<number> = numberLiteral;

function evaluate(symbolTable: {[symbol: string]: number}, expr: any[]): number {
  const reducer = ({
    'Subtract': (a, b) => a - b,
    'Add': (a, b) => a + b,
    'Divide': (a, b) => a / b
  } as {[key: string]: (a: number, b: number) => number})[expr[0]];
  if (!reducer) throw new Error('could not find: ' + expr[0]);
  expr = expr.slice(1).map(n => {
    if (typeof n === 'number') return n;
    if (!(n in symbolTable)) throw new Error('not in sym table: ' + n);
    return symbolTable[n];
  });
  return expr.slice(1).reduce(reducer, expr[0]);
}

function find(symbolTable: {[symbol: string]: number}, name: string): number {
  if (name in symbolTable) return symbolTable[name];
  throw new Error('could not find: ' + name);
}

const const_8bit: Parser<number|Deferred> = math(alt(numberLiteral, id)).map(n => {
  if (typeof n === 'number') return n;
  else if (typeof n === 'string') return { kind: 'deferred', resolve: (symbolTable: any, pos: number) => find(symbolTable, n) & 0xFF };
  else if (n.constructor === Array) {
    // Arithmetic expression.
    return { kind: 'deferred', resolve: (symbolTable: any) => evaluate(symbolTable, n) & 0xFF };
  } else {
    throw new Error('Weird 8-bit constant.')
  }
}) as any;

const const_8bit_rel: Parser<number|Deferred> = math(alt(numberLiteral, id)).map(n => {
  if (typeof n === 'number') return n;
  else if (typeof n === 'string') return { kind: 'deferred', resolve: (symbolTable: any, pos: number) => (find(symbolTable, n) - pos - 1) & 0xFF };
  else if (n.constructor === Array) {
    // Arithmetic expression.
    return { kind: 'deferred', resolve: (symbolTable: any, pos: number) => (evaluate(symbolTable, n) - pos - 1) & 0xFF };
  } else {
    throw new Error('Weird 8-bit constant.')
  }
}) as any;

const const_3bit = const_any;

const const_16bit: Parser<[number, number]|[Deferred, Deferred]> = math(alt(
  numberLiteral,
  id
)).map(nn => {
  if (typeof nn === 'number') return [nn & 0xFF, (nn >> 8) & 0xFF];
  else if (typeof nn == 'string') return [
    { kind: 'deferred', resolve: (symbolTable: any) => find(symbolTable, nn) & 0xFF },
    { kind: 'deferred', resolve: (symbolTable: any) => (find(symbolTable, nn) >> 8) & 0xFF }
  ];
  else if (nn.constructor == Array) {
    // Some arithmetic expression.
    return [
      { kind: 'deferred',
        resolve: (symbolTable: any) => evaluate(symbolTable, nn) & 0xFF },
      { kind: 'deferred',
        resolve: (symbolTable: any) => (evaluate(symbolTable, nn) >> 8) & 0xFF }
    ];
  } else {
    throw new Error('Weird 16-bit constant.');
  }
}) as any;

// function reg(regs: string[]): Parser<number> {
//   return alt.apply(alt, regs.map((name, idx) => symbol(name).result(idx)))
// }

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
  return string('[').skip(optSpace).then(p).skip(optSpace).skip(string(']'));
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
      .skip(optSpace)
      .skip(string(','))
      .skip(optSpace),
    argP2
  ));
}

type Z80 = number;

type Deferred = {
  kind: 'deferred';
  // Pos is to convert symbol to relative address for JR.
  resolve: (symbolTable: {[symbol: string]: number}, pos: number) => number;
};
type FirstPass =
  Z80
    | { kind: 'section'; name: string; sectionType: string; address?: number }
    | { kind: 'label'; name: string }
    | Deferred;

type CpuOp = { name: string, afterParser: Parser<FirstPass[]> };

const cpuOp: Parser<FirstPass[]> = (function() {
  function append16(arr: FirstPass[], nn: [Deferred,Deferred]|[number,number]): FirstPass[] {
    return arr.concat(nn);
  }

  function nullaryCpuOp(name: string, byte: Z80): CpuOp {
    return { name, afterParser: succeed([byte]) };
  }
  function unaryCpuOp<A>(name: string, argP: Parser<A>, map: (arg: A) => FirstPass[]): CpuOp {
    return { name, afterParser: space.then(argP).map(map) };
  }
  function binaryCpuOp<A, B>(name: string, argP1: Parser<A>, argP2: Parser<B>, map: (arg1: A, arg2: B) => FirstPass[]): CpuOp {
    return {
      name,
      afterParser: space.then(seq(
        argP1
          .skip(optSpace)
          .skip(string(','))
          .skip(optSpace),
        argP2
      )).map(([a, b]) => map(a, b))
    };
  }

  function arithmeticCpuOp(name: string, immPrefixByte: Z80, argPrefixBits: Z80): CpuOp {
    // immPrefixByte -- 1 byte. next byte is the immediate
    // argPrefixBits -- upper few bits. bottom bits specify which reg

    // TODO: Support "adc a, b" syntax.
    return {
      name,
      afterParser: space.then(alt(
        const_8bit.map(n => [immPrefixByte, n]), // op_a_n
        reg_r.map(r => [argPrefixBits|r]) // op_a_r
      ))
    };
  }
  function rotateCpuOp(name: string, aByte: Z80, rPrefixBits: Z80): CpuOp {
    return {
      name,
      afterParser: alt(
        space.then(reg_r).map(r => [0xCB, rPrefixBits|r]),
        string('a').result([aByte])
      )
    };
  }

  const ops: CpuOp[] = [
    // ADC n; ADC r
    arithmeticCpuOp('adc', 0xCE, 0x88),
    // ADD HL, ss
    binaryCpuOp('add', symbol('hl'), reg_ss,
      (_, ss) => [0x09|(ss<<4)]),
    // ADD n; ADD r
    arithmeticCpuOp('add', 0xC6, 0x80),
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
    // DEC ss
    unaryCpuOp('dec', reg_ss, ss => [0x0B|(ss<<4)]),
    // DEC r
    unaryCpuOp('dec', reg_r, r => [0x05|(r<<3)]),
    // DI
    nullaryCpuOp('di', 0xF3),
    // EI
    nullaryCpuOp('ei', 0xFB),
    // EX HL, (SP)
    binaryCpuOp('ex', symbol('hl'), indirect(symbol('sp')), () => [0xE3]),
    // HALT
    nullaryCpuOp('halt', 0x76),
    // INC ss
    unaryCpuOp('inc', reg_ss, ss => [0x03|(ss<<4)]),
    // INC r
    unaryCpuOp('inc', reg_r, r => [0x04|(r<<3)]),
    // JP (HL)
    unaryCpuOp('jp', indirect(symbol('hl')), () => [0xE9]),
    // JP cc, nn
    binaryCpuOp('jp', ccode, const_16bit,
      (cc, nn) => append16([0xC2|(cc<<3)], nn)),
    // JP nn
    unaryCpuOp('jp', const_16bit, nn => ([0xC3] as FirstPass[]).concat(nn)),
    // JR cc, n
    binaryCpuOp('jr', ccode, const_8bit_rel,
      (cc, n) => [0x20|(cc<<3), n]),
    // JR n
    unaryCpuOp('jr', const_8bit_rel, n => [0x18, n]),
    // LD (nn), SP
    binaryCpuOp('ld', indirect(const_16bit), symbol('sp'),
      (nn, _) => append16([0x08], nn)),
    // LD ($FF00+C), A
    binaryCpuOp('ld', $ff00_plus(symbol('c')), symbol('a'), () => [0xE2]),
    // LD ($FF00+n), A
    binaryCpuOp('ld', $ff00_plus(const_8bit), symbol('a'), (n, _) => [0xE0, n]),
    // LD (rr), A
    binaryCpuOp('ld', reg_rr, symbol('a'),
      (rr, _) => [0x02|(rr<<4)]),
    // LD (nn), A
    binaryCpuOp('ld', indirect(const_16bit), symbol('a'),
      (nn, _) => append16([0xEA], nn)),
    // LD A, ($FF00+C)
    binaryCpuOp('ld', symbol('a'), $ff00_plus(symbol('c')), () => [0xF2]),
    // LD A, ($FF00+n)
    binaryCpuOp('ld', symbol('a'), $ff00_plus(const_8bit), (_, n) => [0xF0, n]),
    // LD A, (rr)
    binaryCpuOp('ld', symbol('a'), reg_rr, (_, rr) => [0x0A|(rr<<4)]),
    // LD r, r'
    binaryCpuOp('ld', reg_r, reg_r, (r1, r2) => [0x40|(r1<<3)|(r2)]),
    // LD A, (nn)
    binaryCpuOp('ld', symbol('a'), indirect(const_16bit),
      (_, nn) => append16([0xFA], nn)),
    // LD HL, (SP+n)
    binaryCpuOp('ld', symbol('hl'),
      indirect(symbol('sp +').or(symbol('sp+')).then(const_8bit)),
      (_, n) => [0xF8, n]),
    // LD SP, HL
    binaryCpuOp('ld', symbol('sp'), symbol('hl'), () => [0xF9]),
    // LD r, n
    binaryCpuOp('ld', reg_r, const_8bit, (r, n) => [0x06|(r<<3), n]),
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
    // RETI
    nullaryCpuOp('reti', 0xD9),
    // RET cc
    unaryCpuOp('ret', ccode, cc => [0xC0|(cc<<3)]),
    // RET
    nullaryCpuOp('ret', 0xC9),
    // RL r; RLA
    rotateCpuOp('rl', 0x10, 0x17),
    // RLC r; RLCA
    rotateCpuOp('rlc', 0x00, 0x07),
    // RR r; RRA
    rotateCpuOp('rr', 0x18, 0x1F),
    // RRC r; RRCA
    rotateCpuOp('rrc', 0x08, 0x0F),
    // RST n
    unaryCpuOp('rst', const_8bit, n => [0xC7, n]), // TODO: Check this out
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
  ];
  const opTable: {[name: string]: Parser<FirstPass[]>[]} = {};
  ops.forEach(({ name, afterParser }) => {
    if (!(name in opTable)) opTable[name] = [];
    opTable[name].push(afterParser);
  });
  return Parsimmon(function(input: string, i: number) {
    for (let len = 4; len >= 1; len--) {
      const instr = input.slice(i, i + len);
      if (instr in opTable) {
        return Parsimmon.makeSuccess(i + len, opTable[instr]);
      }
    }
    return Parsimmon.makeFailure(i, 'wut');
  }).chain((afterParsers: Parser<FirstPass[]>[]) => alt.apply(alt, afterParsers) as any as Parser<FirstPass[]>);
})();

const sectionType = alt(
  symbol('WRAM0').or(symbol('BSS')),
  symbol('VRAM'),
  symbol('ROMX'),
  symbol('ROM0').or(symbol('HOME')),
  symbol('HRAM'),
  symbol('WRAMX'),
  symbol('SRAM')
);

const simplePseudoOp: Parser<FirstPass[]> = (function() {
  // FIXME: No expressions allowed to make it fast.
  const dbConst: Parser<Z80[]> = alt(
    numberLiteral.map(n => [n]), // 8BIT but no exprs pls
    stringLiteral.map(s => s.split('').map(c => c.charCodeAt(0)))
  );
  const db = symbol('DB').skip(optSpace)
    .then(sepBy(dbConst, string(',').then(optSpace)))
    // Support both DB as nullary var declaration and DB as data constants.
    .map(data => data.length > 0 ? [].concat.apply([], data) : [0]);

  const dw = symbol('DW').skip(optSpace)
    .then(sepBy(const_16bit, string(',').then(optSpace)))
    .map(data => [].concat.apply([], data));

  return alt(
    db,
    dw,
    binaryOp('SECTION', stringLiteral, seq(sectionType, indirect(numberLiteral)))
      .map(([name, [sectionType, address]]) => [{
        kind: 'section',
        name,
        sectionType,
        address
      }]),
    binaryOp('SECTION', stringLiteral, sectionType)
      .map(([name, sectionType]) => [{ kind: 'section', name, sectionType }]),
  );
})();

const label: Parser<FirstPass[]> =
  id.skip(optSpace)
    .skip(string('::').or(string(':')))
    .map((name: string) => [{ kind: 'label', name } as FirstPass]);

const comment = string(';').then(takeWhile(c => c !== '\n'));

const statement: Parser<FirstPass[]> = optSpace.then(alt(
  seqMap(
    label.skip(optSpace),
    alt(
      cpuOp,
      simplePseudoOp,
      /*macro*/
    ),
    (label, op) => label.concat(op)
  ),
  cpuOp,
  simplePseudoOp,
  label,
  optSpace.result(null)
)).skip(optSpace.then(comment.or(string(''))));

const statements: Parser<FirstPass[]> =
  sepBy(statement as any, string('\n'))
    .map(sts => [].concat.apply([], sts).filter((x: FirstPass) => x !== null));

export function pass(input: FirstPass[]): Z80[] {
  // We need an imperative walker here.
  let address = 0;
  const symbolTable: {[symbol: string]: number} = {};

  const secondPass: (Z80|Deferred)[] = [];
  for (let idx = 0; idx < input.length; idx++) {
    const item = input[idx];
    if (typeof item === 'number' || item.kind == 'deferred') {
      secondPass[address] = item;
      address++;
    } else if (item.kind === 'section') {
      address = item.address;
    } else if (item.kind === 'label') {
      symbolTable[item.name] = address;
    } else {
      throw new Error('Invalid first-pass item: ' + item);
    }
  }
  console.log(symbolTable);

  const rom: Z80[] = [];
  for (const addr in secondPass) {
    const val = secondPass[addr];
    if (typeof val === 'number') {
      rom[addr] = val;
    } else if (val == null) {
      rom[addr] = 0;
    } else if (val.kind === 'deferred') {
      rom[addr] = val.resolve(symbolTable, parseFloat(addr));
    } else {
      throw new Error('Invalid second-pass item: ' + val);
    }
  }

  return rom;
}

export function assemble(s: string) {
  const substituted = macroPass(s);
  /*console.log(substituted);*/
  const first = statements.tryParse(substituted);
  return pass(first);
}
