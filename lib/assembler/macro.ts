import { string, regexp, alt, seq, sepBy, seqMap, takeWhile, Parser } from 'parsimmon';

// First phase is a rapid scanner and macro substitution phase.
// Should take < 10 ms.

const optLineSpace = regexp(/[ \t]*/);
const lineSpace = regexp(/[ \t]+/);

const token = regexp(/[^ \t\n,;\[\]]+|\[|\]|,/);

// const macro; // TODO

const eol = seq(
  optLineSpace,
  alt(
    seq(string(';'), takeWhile(c => c !== '\n'), string('\n')),
    string('\n')
  )
);

type Macro = { kind: 'macro', name: string; body: string; };

const equ: Parser<Macro> = seqMap(
  optLineSpace.then(token).skip(lineSpace), string('EQU').skip(lineSpace),
  takeWhile(c => c !== ';' && c !== '\n'), eol,
  (name, _, body, _2) => ({ kind: 'macro', name, body } as Macro)
);

const equs: Parser<Macro> = seqMap(
  optLineSpace.then(token).skip(lineSpace), string('EQUS').skip(lineSpace),
  string('"').then(takeWhile(c => c !== '"')).skip(string('"')), eol,
  (name, _, body, _2) => ({ kind: 'macro', name, body } as Macro)
);

// This parser needs to be information-preserving.
type Statement = { kind: 'statement', tokens: string[] };

const statement: Parser<Statement> =
  optLineSpace.then(
    alt(token, lineSpace)
      .many()
      .map((tokens) => ({ kind: 'statement', tokens } as Statement))
  ).skip(eol);

const file: Parser<(Macro|Statement)[]> = alt(
  equs,
  equ,
  statement,
  eol.result(null)
).many().map(r => r.filter(x => x));

export function macroPass(s: string): string {
  const lines = file.tryParse(s + '\n');
  // Go through.
  // Build macro table and simultaneously pull in to replace.
  const macroTable: {[name: string]: string} = {};
  const newFile = [];
  for (const line of lines) {
    if (line.kind === 'macro') {
      macroTable[line.name] = line.body;
    } else if (line.kind === 'statement') {
      newFile.push(line.tokens.map(s => {
        if (s in macroTable) {
          console.log(s, line);
          return macroTable[s];
        }
        return s;
      }).join(''));
    }
  }
  return newFile.join('\n');
}
