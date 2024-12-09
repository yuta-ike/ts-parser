/** == プレイグラウンド ============================================================================= */

// README!
// - テストケース一覧は一番下にあります。
// - 負数はサポートしていません。計算中に負数が出現する場合も同様です。
// - 文字列リテラルの中を除いて、空白は許容しません。
// - 文字列リテラルの中ではクオーテーション・ダブルクオーテーションは使えません。エスケープシーケンスも未実装です。

type Result1 = Eval<"12*3+5*7+(2+45)*2-4">;
type Result2 = Eval<"cast('4'+'2','num')">;
type Result3 =
  Eval<"'hello,world!の文字数は'+cast(len('hello,world!'),'str')+'文字です'">;
type Result4 = Eval<"pi*r*r", { r: 5; pi: 3 }>;
type Result5 = Eval<"'ようこそ'+username+'さん!'", { username: "Yuta" }>;

type Result = Eval<"(数式を入力してね！)">;

/** == ここから実装 ============================================================================= */
import type { Equal, Expect } from "@type-challenges/utils";

// エラー文
type Err<
  Message extends string,
  Info1 = null,
  Info2 = null,
  Err3 = null
> = Info1 extends null
  ? [message: Message]
  : Info2 extends null
  ? [message: Message, Info1]
  : Err3 extends null
  ? [message: Message, Info1, Info2]
  : [message: Message, Info1, Info2, Err3];

/** -- 処理系 ------------------------------------- */
// シンボル定義
declare const plus: unique symbol; // nat
declare const minus: unique symbol;
declare const mul: unique symbol;
declare const parenl: unique symbol;
declare const parenr: unique symbol;
declare const eq: unique symbol; // comp
declare const neq: unique symbol;
declare const lt: unique symbol;
declare const lte: unique symbol;
declare const gt: unique symbol;
declare const gte: unique symbol;
declare const and: unique symbol; // bool
declare const or: unique symbol;
declare const comma: unique symbol;
type DigitValue = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
type Alphabet =
  | "_"
  | "a"
  | "b"
  | "c"
  | "d"
  | "e"
  | "f"
  | "g"
  | "h"
  | "i"
  | "j"
  | "k"
  | "l"
  | "m"
  | "n"
  | "o"
  | "p"
  | "q"
  | "r"
  | "s"
  | "t"
  | "u"
  | "v"
  | "w"
  | "x"
  | "y"
  | "z"
  | "A"
  | "B"
  | "C"
  | "D"
  | "E"
  | "F"
  | "G"
  | "H"
  | "I"
  | "J"
  | "K"
  | "L"
  | "M"
  | "N"
  | "O"
  | "P"
  | "Q"
  | "R"
  | "S"
  | "T"
  | "U"
  | "V"
  | "W"
  | "X"
  | "Y"
  | "Z";

// トークン定義
// 数値リテラル
type NumLiteral<Value extends string = string> = { type: "num"; value: Value };
// 文字列リテラル
type StrLiteral<Value extends string = string> = { type: "str"; value: Value };
// 変数
type VarExp<Name extends string = string> = { type: "var"; name: Name };
// 演算子
type Plus = typeof plus;
type Minus = typeof minus;
type Mul = typeof mul;
type Eq = typeof eq;
type Neq = typeof neq;
type Lt = typeof lt;
type Lte = typeof lte;
type Gt = typeof gt;
type Gte = typeof gte;
type And = typeof and;
type Or = typeof or;
type Operator<
  OpType extends
    | Plus
    | Minus
    | Mul
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    | And
    | Or = Plus | Minus | Mul | Eq | Neq | Lt | Lte | Gt | Gte | And | Or
> = { type: "op"; value: OpType };
// 記号
type ParenL = typeof parenl;
type ParenR = typeof parenr;
type Comma = typeof comma;
// ビルトイン関数
type Len = "len";
type Parse = "cast";
// トークン
type Token =
  | NumLiteral
  | StrLiteral
  | VarExp
  | Operator
  | ParenL
  | ParenR
  | Comma;

/** -- トークナイザ ------------------------------------- */
// 自然数
type TokenizerDigitRec<
  Program extends string,
  Acc extends string = ""
> = Program extends `${infer Num extends DigitValue}${infer Rest extends string}`
  ? TokenizerDigitRec<Rest, `${Acc}${Num}`>
  : [Acc, Program];
type TokenizerDigit<Program extends string> =
  Program extends `${infer Num extends DigitValue}${infer Rest extends string}`
    ? TokenizerDigitRec<Rest, Num>
    : Program;
// 変数
type TokenizerVarRec<
  Program extends string,
  Acc extends string = ""
> = Program extends `${infer Letter extends
  | Alphabet
  | DigitValue}${infer Rest extends string}`
  ? TokenizerVarRec<Rest, `${Acc}${Letter}`>
  : [Acc, Program];
type TokenizerVar<Program extends string> =
  Program extends `${infer Letter extends Alphabet}${infer Rest extends string}`
    ? TokenizerVarRec<Rest, Letter>
    : Program;

type Tokenizer<Program extends string> =
  // 終了条件
  Program extends ""
    ? []
    : Program extends `,${infer Rest extends string}`
    ? [Comma, ...Tokenizer<Rest>]
    : Program extends `+${infer Rest extends string}`
    ? [Operator<Plus>, ...Tokenizer<Rest>]
    : Program extends `-${infer Rest extends string}`
    ? [Operator<Minus>, ...Tokenizer<Rest>]
    : Program extends `*${infer Rest extends string}`
    ? [Operator<Mul>, ...Tokenizer<Rest>]
    : Program extends `(${infer Rest extends string}`
    ? [ParenL, ...Tokenizer<Rest>]
    : Program extends `)${infer Rest extends string}`
    ? [ParenR, ...Tokenizer<Rest>]
    : Program extends `<=${infer Rest extends string}`
    ? [Operator<Lte>, ...Tokenizer<Rest>]
    : Program extends `<${infer Rest extends string}`
    ? [Operator<Lt>, ...Tokenizer<Rest>]
    : Program extends `>=${infer Rest extends string}`
    ? [Operator<Gte>, ...Tokenizer<Rest>]
    : Program extends `>${infer Rest extends string}`
    ? [Operator<Gt>, ...Tokenizer<Rest>]
    : Program extends `=${infer Rest extends string}`
    ? [Operator<Eq>, ...Tokenizer<Rest>]
    : Program extends `!=${infer Rest extends string}`
    ? [Operator<Neq>, ...Tokenizer<Rest>]
    : Program extends `&&${infer Rest extends string}`
    ? [Operator<And>, ...Tokenizer<Rest>]
    : Program extends `||${infer Rest extends string}`
    ? [Operator<Or>, ...Tokenizer<Rest>]
    : Program extends `'${infer Value extends string}'${infer Rest extends string}`
    ? [StrLiteral<Value>, ...Tokenizer<Rest>]
    : Program extends `"${infer Value extends string}"${infer Rest extends string}`
    ? [StrLiteral<Value>, ...Tokenizer<Rest>]
    : TokenizerDigit<Program> extends [
        infer Num extends string,
        infer Rest extends string
      ]
    ? [NumLiteral<Num>, ...Tokenizer<Rest>]
    : TokenizerVar<Program> extends [
        infer Var extends string,
        infer Rest extends string
      ]
    ? [VarExp<Var>, ...Tokenizer<Rest>]
    : [Err<`Unknown token: ${Program}`>];

/** -- パーサ ------------------------------------- */
type AstType =
  | {
      type: "logic";
      left: AstType;
      right: AstType;
      operator: Operator<And | Or>;
    }
  | {
      type: "comp";
      left: AstType;
      right: AstType;
      operator: Operator<Eq | Neq | Lt | Lte | Gt | Gte>;
    }
  | {
      type: "expr";
      left: AstType;
      right: AstType;
      operator: Operator<Plus | Minus>;
    }
  | {
      type: "term";
      left: AstType;
      right: AstType;
      operator: Operator<Mul>;
    }
  | {
      type: "call";
      callee: AstType;
      args: AstType[];
    }
  | {
      type: "num";
      value: string;
    }
  | {
      type: "str";
      value: string;
    }
  | {
      type: "var";
      name: string;
    };

// Helper（左結合の演算子を処理する）
type FoldLeft<
  Type extends string,
  Ast extends AstType,
  Asts extends [AstType, Operator][]
> = Asts extends [
  [infer NextAst extends AstType, infer Op],
  ...infer Rest extends [AstType, Operator][]
]
  ? {
      type: Type;
      left: FoldLeft<Type, NextAst, Rest>;
      operator: Op;
      right: Ast;
    }
  : Ast;

// 基本仕様: Token[]を受け取り評価する。[評価結果のAST(Ast), 未評価のトークン列(Token[])]のタプルを返す。
// Prim
type PrimParser<Tokens extends Token[]> = Tokens extends [
  infer Token extends NumLiteral | StrLiteral,
  ...infer Rest
]
  ? [Token, Rest]
  : Tokens extends [infer Token extends VarExp, ...infer Rest]
  ? [Token, Rest]
  : Err<"Expected literal but", Tokens[0]>;

// Factor
type FactorParser<Tokens extends Token[]> = Tokens extends [
  infer _ extends ParenL,
  ...infer Rest extends Token[]
]
  ? ExprParser<Rest> extends [
      infer Ast extends AstType,
      [infer _ extends ParenR, ...infer Rest extends Token[]]
    ]
    ? [Ast, Rest]
    : ExprParser<Rest> extends [
        infer Ast extends AstType,
        [infer AnyToken, ...infer Rest]
      ]
    ? [Ast, Err<"Expected )", AnyToken, Rest>]
    : Err<"Error at FactorParser", ExprParser<Tokens>>
  : PrimParser<Tokens>;

// Comma
type CommaParser<Tokens extends Token[]> = ExprParser<Tokens> extends [
  infer Ast extends AstType,
  [Comma, ...infer Rest extends Token[]]
]
  ? CommaParser<Rest> extends [infer Asts extends AstType[], infer InnerRest]
    ? [[Ast, ...Asts], InnerRest]
    : Err<"Error at CommaParser", CommaParser<Rest>>
  : ExprParser<Tokens> extends [
      infer Ast extends AstType,
      infer Rest extends Token[]
    ]
  ? [[Ast], Rest]
  : Err<"Error at CommaParser", FactorParser<Tokens>>;

// Call
type ExtractUntilParenL<Tokens extends Token[]> = Tokens extends [
  ParenL,
  ...infer Rest extends Token[]
]
  ? [true, [], Rest]
  : Tokens extends [infer First, ...infer Rest extends Token[]]
  ? ExtractUntilParenL<Rest> extends [
      true,
      infer Extracted extends Token[],
      infer Rest extends Token[]
    ]
    ? [true, [First, ...Extracted], Rest]
    : ExtractUntilParenL<Rest> extends [false, any, infer Rest]
    ? [false, [], Rest]
    : Err<"Error at ExtractUntilParenL", ExtractUntilParenL<Rest>>
  : [false, Tokens, []];

type CallParser<Tokens extends Token[]> = ExtractUntilParenL<Tokens> extends [
  true,
  infer Extracted extends Token[],
  infer Rest extends Token[]
]
  ? FactorParser<Extracted> extends [infer Callee extends AstType, []]
    ? CommaParser<Rest> extends [
        infer Args extends AstType[],
        [ParenR, ...infer Rest extends Token[]]
      ]
      ? [
          {
            type: "call";
            callee: Callee;
            args: Args;
          },
          Rest
        ]
      : Err<"Error at CallParser", ExtractUntilParenL<Rest>>
    : FactorParser<Tokens>
  : FactorParser<Tokens>;

// Term
type TermParser<
  Tokens extends Token[],
  AstSet extends [AstType, Operator][] = []
> = CallParser<Tokens> extends [
  infer Ast extends AstType,
  [infer Op extends Operator<Mul>, ...infer Rest extends Token[]]
]
  ? TermParser<Rest, [[Ast, Op], ...AstSet]>
  : CallParser<Tokens> extends [infer Ast extends AstType, infer Rest]
  ? [FoldLeft<"term", Ast, AstSet>, Rest]
  : Err<"Error at TermParser", CallParser<Tokens>>;

// Expression
type ExprParser<
  Tokens extends Token[],
  AstSet extends [AstType, Operator][] = []
> = TermParser<Tokens> extends [
  infer Ast extends AstType,
  [infer Op extends Operator<Plus | Minus>, ...infer Rest extends Token[]]
]
  ? ExprParser<Rest, [[Ast, Op], ...AstSet]>
  : TermParser<Tokens> extends [infer Ast extends AstType, infer Rest]
  ? [FoldLeft<"expr", Ast, AstSet>, Rest]
  : Err<
      "Error at ExprParser",
      TermParser<Tokens> extends [infer Ast, any] ? Ast : false
    >;

// Comparison
type ComparisonParser<Tokens extends Token[]> = ExprParser<Tokens> extends [
  infer Ast extends AstType,
  [
    infer Op extends Operator<Eq | Neq | Lt | Lte | Gt | Gte>,
    ...infer Rest extends Token[]
  ]
]
  ? ExprParser<Rest> extends [
      infer RightAst extends AstType,
      infer RightRest extends Token[]
    ]
    ? [
        {
          type: "comp";
          operator: Op;
          left: Ast;
          right: RightAst;
        },
        RightRest
      ]
    : Err<"Error at ComparisonParser", ExprParser<Rest>>
  : ExprParser<Tokens> extends [
      infer Ast extends AstType,
      infer Rest extends Token[]
    ]
  ? [Ast, Rest]
  : Err<"Error at ComparisonParser", ExprParser<Tokens>>;

// Logic
type LogicParser<
  Tokens extends Token[],
  AstSet extends [AstType, Operator][] = []
> = ComparisonParser<Tokens> extends [
  infer Ast extends AstType,
  [infer Op extends Operator<And | Or>, ...infer Rest extends Token[]]
]
  ? LogicParser<Rest, [[Ast, Op], ...AstSet]>
  : ComparisonParser<Tokens> extends [infer Ast extends AstType, infer Rest]
  ? [FoldLeft<"logic", Ast, AstSet>, Rest]
  : Err<
      "Error at LogicParser",
      ComparisonParser<Tokens> extends [infer Ast, any] ? Ast : false
    >;

// Parser
type Parser<Tokens extends Token[]> = LogicParser<Tokens> extends [
  infer Ast extends AstType,
  []
]
  ? Ast
  : LogicParser<Tokens> extends [
      infer Ast extends AstType,
      infer Rest extends Token[]
    ]
  ? Err<"Unknown tokens", Ast, Rest>
  : Err<"Error at Parser", LogicParser<Tokens>>;

/** -- デバッグ用エリア ------------------------------------- */

type DebugProgram = "3+7*9";

type Tokenized = Tokenizer<DebugProgram>;
type Nu = PrimParser<Tokenizer<DebugProgram>>;
type F = FactorParser<Tokenizer<DebugProgram>>;
type Ca = CallParser<Tokenizer<DebugProgram>>;
type T = TermParser<Tokenizer<DebugProgram>>;
type E = ExprParser<Tokenizer<DebugProgram>>;
type C = ComparisonParser<Tokenizer<DebugProgram>>;
type L = LogicParser<Tokenizer<DebugProgram>>;

type X = Parser<Tokenizer<DebugProgram>>;
declare const x: X;

type Env = {};
type Y = Evaluator<Parser<Tokenizer<DebugProgram>>, Env>;

/** -- テストヘルパー ------------------------------------- */
type ErrLog<Ast> = Ast extends [infer Message extends string, infer NextErr]
  ? `${Message} | ${ErrLog<NextErr>}`
  : "";

type IsErr<Ast> = Ast extends [string, any]
  ? true
  : Ast extends [any, ...infer Rest]
  ? IsErr<Rest>
  : false;

/** -- テスト（パーサー） ------------------------------------- */
// テストケース
type cases = [
  // +, - 入り混じった式
  Expect<
    Equal<
      Parser<Tokenizer<"1+2*3">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: NumLiteral<"1">;
        right: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"2">;
          right: NumLiteral<"3">;
        };
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"1*2+3">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"1">;
          right: NumLiteral<"2">;
        };
        right: NumLiteral<"3">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"1+2-3">>,
      {
        type: "expr";
        operator: Operator<Minus>;
        left: {
          type: "expr";
          operator: Operator<Plus>;
          left: NumLiteral<"1">;
          right: NumLiteral<"2">;
        };
        right: NumLiteral<"3">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"1+(2-3)">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: NumLiteral<"1">;
        right: {
          type: "expr";
          operator: Operator<Minus>;
          left: NumLiteral<"2">;
          right: NumLiteral<"3">;
        };
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"1-2*3+4">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: {
          type: "expr";
          operator: Operator<Minus>;
          left: NumLiteral<"1">;
          right: {
            type: "term";
            operator: Operator<Mul>;
            left: NumLiteral<"2">;
            right: NumLiteral<"3">;
          };
        };
        right: NumLiteral<"4">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"1*2-3*4">>,
      {
        type: "expr";
        operator: Operator<Minus>;
        left: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"1">;
          right: NumLiteral<"2">;
        };
        right: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"3">;
          right: NumLiteral<"4">;
        };
      }
    >
  >,
  // カッコあり
  Expect<
    Equal<
      Parser<Tokenizer<"1*(2-3)*4">>,
      {
        type: "term";
        operator: Operator<Mul>;
        right: NumLiteral<"4">;
        left: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"1">;
          right: {
            type: "expr";
            left: NumLiteral<"2">;
            right: NumLiteral<"3">;
            operator: Operator<Minus>;
          };
        };
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"(1+2)">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: NumLiteral<"1">;
        right: NumLiteral<"2">;
      }
    >
  >,
  // 入れ子
  Expect<
    Equal<
      Parser<Tokenizer<"1+(2*(3+4))">>,
      {
        type: "expr";
        operator: Operator<Plus>;
        left: NumLiteral<"1">;
        right: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"2">;
          right: {
            type: "expr";
            left: NumLiteral<"3">;
            right: NumLiteral<"4">;
            operator: Operator<Plus>;
          };
        };
      }
    >
  >,
  // 1種類の演算子だけの式
  Expect<
    Equal<
      Parser<Tokenizer<"1-9">>,
      {
        type: "expr";
        operator: Operator<Minus>;
        left: NumLiteral<"1">;
        right: NumLiteral<"9">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"2*8">>,
      {
        type: "term";
        operator: Operator<Mul>;
        left: NumLiteral<"2">;
        right: NumLiteral<"8">;
      }
    >
  >,
  // 数値リテラルのみ
  Expect<Equal<Parser<Tokenizer<"0">>, NumLiteral<"0">>>,
  // 比較演算子
  Expect<
    Equal<
      Parser<Tokenizer<"9<12">>,
      {
        type: "comp";
        operator: Operator<Lt>;
        left: NumLiteral<"9">;
        right: NumLiteral<"12">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"9<=12">>,
      {
        type: "comp";
        operator: Operator<Lte>;
        left: NumLiteral<"9">;
        right: NumLiteral<"12">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"9>12">>,
      {
        type: "comp";
        operator: Operator<Gt>;
        left: NumLiteral<"9">;
        right: NumLiteral<"12">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"9>=12">>,
      {
        type: "comp";
        operator: Operator<Gte>;
        left: NumLiteral<"9">;
        right: NumLiteral<"12">;
      }
    >
  >,
  Expect<
    Equal<
      Parser<Tokenizer<"(1+2+3)<(3*4)">>,
      {
        type: "comp";
        operator: Operator<Lt>;
        left: {
          type: "expr";
          operator: Operator<Plus>;
          left: {
            type: "expr";
            operator: Operator<Plus>;
            left: NumLiteral<"1">;
            right: NumLiteral<"2">;
          };
          right: NumLiteral<"3">;
        };
        right: {
          type: "term";
          operator: Operator<Mul>;
          left: NumLiteral<"3">;
          right: NumLiteral<"4">;
        };
      }
    >
  >
];

// エラーケース
type errcases = [
  // 空のプログラム
  Expect<IsErr<Parser<Tokenizer<"">>>>,
  // 不正な式
  Expect<IsErr<Parser<Tokenizer<"-++-1+-+-2">>>>
];

/** -- 実行 ------------------------------------- */
// bool値の定義
type BoolType = true | false;
type BoolNot<A extends BoolType> = A extends true ? false : true;
type BoolAnd<A extends BoolType, B extends BoolType> = [A, B] extends [
  true,
  true
]
  ? true
  : false;
type BoolOr<A extends BoolType, B extends BoolType> = [A, B] extends [
  false,
  false
]
  ? false
  : true;

// 自然数の定義
declare const n: unique symbol; // 単位元
type N = typeof n;
type Base = [N, N, N, N, N, N, N, N, N, N];
type DigitToN<Digi extends string> = DigitToNRec<Digi> extends [
  infer Result,
  any
]
  ? Result
  : never;
type DigitToNRec<Digi extends string> = Digi extends ""
  ? [[], []]
  : Digi extends `${infer D extends DigitValue}${infer Rest extends string}`
  ? DigitToNRec<Rest> extends [infer Sum extends N[], infer Cnt extends N[]]
    ? D extends "0"
      ? [[...NatMul<[], NatFact<Base, Cnt>>, ...Sum], NatAdd<Cnt, [N]>]
      : D extends "1"
      ? [[...NatMul<[N], NatFact<Base, Cnt>>, ...Sum], NatAdd<Cnt, [N]>]
      : D extends "2"
      ? [[...NatMul<[N, N], NatFact<Base, Cnt>>, ...Sum], NatAdd<Cnt, [N]>]
      : D extends "3"
      ? [[...NatMul<[N, N, N], NatFact<Base, Cnt>>, ...Sum], NatAdd<Cnt, [N]>]
      : D extends "4"
      ? [
          [...NatMul<[N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : D extends "5"
      ? [
          [...NatMul<[N, N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : D extends "6"
      ? [
          [...NatMul<[N, N, N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : D extends "7"
      ? [
          [...NatMul<[N, N, N, N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : D extends "8"
      ? [
          [...NatMul<[N, N, N, N, N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : D extends "9"
      ? [
          [...NatMul<[N, N, N, N, N, N, N, N, N], NatFact<Base, Cnt>>, ...Sum],
          NatAdd<Cnt, [N]>
        ]
      : never
    : never
  : never;
type NToNum<Nat extends N[]> = Nat["length"];

// 二項演算
type NatAdd<A extends N[], B extends N[]> = [...A, ...B];
type NatSub<A extends N[], B extends N[]> = [A, B] extends [
  [N, ...infer RestA extends N[]],
  [N, ...infer RestB extends N[]]
]
  ? NatSub<RestA, RestB>
  : B extends []
  ? A
  : A extends []
  ? [] // 負数になる場合は0を返す
  : never;
type NatMul<A extends N[], B extends N[]> = B extends [
  any,
  ...infer Rest extends N[]
]
  ? [...A, ...NatMul<A, Rest>]
  : [];
type NatFact<A extends N[], B extends N[]> = NatFactRect<[N], A, B>;
type NatFactRect<Result extends N[], A extends N[], B extends N[]> = B extends [
  any,
  ...infer Rest extends N[]
]
  ? NatFactRect<NatMul<Result, A>, A, Rest>
  : Result;
type NatEqZero<A extends N[]> = A extends [any] ? false : true;
type NatEq<A extends N[], B extends N[]> = BoolAnd<
  NatEqZero<NatSub<A, B>>,
  NatEqZero<NatSub<B, A>>
>;
type NatNeq<A extends N[], B extends N[]> = BoolNot<NatEq<A, B>>;
type NatLt<A extends N[], B extends N[]> = NatEqZero<NatSub<A, B>>;
type NatLte<A extends N[], B extends N[]> = BoolOr<NatLt<A, B>, NatEq<A, B>>;
type NatGt<A extends N[], B extends N[]> = BoolNot<NatLte<A, B>>;
type NatGte<A extends N[], B extends N[]> = BoolOr<NatGt<A, B>, NatEq<A, B>>;

// 文字列
// Len
type StrLen<A extends string> = A extends `${string}${infer Rest}`
  ? NatAdd<StrLen<Rest>, [N]>
  : [];
// 二項演算
type StrAdd<A extends string, B extends string> = `${A}${B}`;
type StrEq<A extends string, B extends string> = [A, B] extends [B, A]
  ? true
  : false;
type StrNeq<A extends string, B extends string> = BoolNot<StrEq<A, B>>;

// 実行器
type Environment = Record<string, string | number | boolean>;

type EvaluatorRec<Ast extends AstType, Env extends Environment> = Ast extends {
  type: "num";
}
  ? DigitToN<Ast["value"]>
  : Ast extends { type: "str" }
  ? Ast["value"]
  : Ast extends { type: "var"; name: infer Name extends keyof Env }
  ? Env[Name] extends number
    ? DigitToN<`${Env[Name]}`>
    : Env[Name]
  : Ast extends { type: "var" }
  ? Ast // 組み込み関数などはcallで処理するのでそのまま残しておく
  : Ast extends {
      type: "call";
      callee: infer Callee extends AstType;
      args: infer Args extends AstType[];
    }
  ? // Len
    [EvaluatorRec<Callee, Env>, EvaluatorRec<Args[0], Env>] extends [
      VarExp<Len>,
      infer Value extends string
    ]
    ? StrLen<Value>
    : // Parse
    [
        EvaluatorRec<Callee, Env>,
        EvaluatorRec<Args[0], Env>,
        EvaluatorRec<Args[1], Env>
      ] extends [VarExp<Parse>, infer Value extends string, "num"]
    ? DigitToN<Value>
    : [
        EvaluatorRec<Callee, Env>,
        EvaluatorRec<Args[0], Env>,
        EvaluatorRec<Args[1], Env>
      ] extends [VarExp<Parse>, infer Value extends N[], "str"]
    ? `${NToNum<Value>}`
    : // Error handler
    [
        EvaluatorRec<Callee, Env>,
        EvaluatorRec<Args[0], Env>,
        EvaluatorRec<Args[1], Env>
      ] extends [VarExp, infer Arg1, infer Arg2]
    ? Err<"Argument type mismatched", Arg1, Arg2>
    : [EvaluatorRec<Callee, Env>, EvaluatorRec<Args[0], Env>] extends [
        infer Callee
      ]
    ? Err<"Cannot call", Callee>
    : never
  : Ast extends {
      type: "term";
      operator: Operator<Mul>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatMul<L, R>
    : never
  : Ast extends {
      type: "expr";
      operator: Operator<Plus>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatAdd<L, R>
    : [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
        infer L extends string,
        infer R extends string
      ]
    ? StrAdd<L, R>
    : never
  : Ast extends {
      type: "expr";
      operator: Operator<Minus>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatSub<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Eq>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatEq<L, R>
    : [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
        infer L extends string,
        infer R extends string
      ]
    ? StrEq<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Neq>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatNeq<L, R>
    : [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
        infer L extends string,
        infer R extends string
      ]
    ? StrNeq<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Lt>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatLt<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Lte>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatLte<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Gt>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatGt<L, R>
    : never
  : Ast extends {
      type: "comp";
      operator: Operator<Gte>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends N[],
      infer R extends N[]
    ]
    ? NatGte<L, R>
    : never
  : Ast extends {
      type: "logic";
      operator: Operator<And>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends BoolType,
      infer R extends BoolType
    ]
    ? BoolAnd<L, R>
    : [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
        infer _ extends N[],
        infer _ extends N[]
      ]
    ? Err<"Logical operations (&&) are not defined for nat.", Left, Right>
    : never
  : Ast extends {
      type: "logic";
      operator: Operator<Or>;
      left: infer Left extends AstType;
      right: infer Right extends AstType;
    }
  ? [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
      infer L extends BoolType,
      infer R extends BoolType
    ]
    ? BoolOr<L, R>
    : [EvaluatorRec<Left, Env>, EvaluatorRec<Right, Env>] extends [
        infer _ extends N[],
        infer _ extends N[]
      ]
    ? Err<"Logical operations (||) are not defined for nat.", Left, Right>
    : never
  : Err<"Unknown token", Ast>;

type Evaluator<
  Ast extends AstType,
  Env extends Environment = Record<never, never>
> = EvaluatorRec<Ast, Env> extends infer Result extends N[]
  ? NToNum<Result>
  : EvaluatorRec<Ast, Env> extends infer Result extends string
  ? Result
  : EvaluatorRec<Ast, Env> extends infer Result extends BoolType
  ? Result
  : Err<"Unknown token", EvaluatorRec<Ast, Env>>;

// トークナイザ -> パーサ → 実行
type Eval<
  Program extends string,
  Env extends Environment = Record<never, never>
> = Tokenizer<Program> extends infer Tokens extends Token[]
  ? Parser<Tokens> extends infer Ast extends AstType
    ? Evaluator<Ast, Env>
    : Parser<Tokens>
  : Tokenizer<Program>;

type cases2 = [
  // 複雑な演算
  Expect<Equal<Eval<"4*3+2">, 14>>,
  Expect<Equal<Eval<"4+3*2">, 10>>,
  Expect<Equal<Eval<"4*3-2">, 10>>,
  Expect<Equal<Eval<"6-3*2">, 0>>,
  Expect<Equal<Eval<"9*8*6">, 432>>,
  Expect<Equal<Eval<"(4+3)*(2+1)">, 21>>,
  Expect<Equal<Eval<"(4*3)-(2*1)">, 10>>,
  Expect<Equal<Eval<"(4-3)*(2-1)">, 1>>,
  Expect<Equal<Eval<"(4+3)*(2-1)">, 7>>,
  Expect<Equal<Eval<"(4-3)*(2+1)">, 3>>,
  Expect<Equal<Eval<"(6-(2+1))*((6*2)-4)+3">, 27>>,
  Expect<Equal<Eval<"1+2+3+4+5+6+7+8+9">, 45>>,
  Expect<Equal<Eval<"(1+2+3+4+5+6+7+8+9)*9">, 405>>,
  // 複数桁
  Expect<Equal<Eval<"14*3+12">, 54>>,
  Expect<Equal<Eval<"14+3*2">, 20>>,
  Expect<Equal<Eval<"14*3-2">, 40>>,
  Expect<Equal<Eval<"16-3*2">, 10>>,
  Expect<Equal<Eval<"12*8*6">, 576>>,
  Expect<Equal<Eval<"12*12">, 144>>,
  // 結合順
  Expect<Equal<Eval<"6-3+2">, 5>>,
  Expect<Equal<Eval<"6+3-2">, 7>>,
  Expect<Equal<Eval<"(6-3)+(2-1)">, 4>>,
  Expect<Equal<Eval<"6-(3+2)-1">, 0>>,
  // 比較演算
  Expect<Equal<Eval<"1=1">, true>>,
  Expect<Equal<Eval<"1=2">, false>>,
  Expect<Equal<Eval<"1!=1">, false>>,
  Expect<Equal<Eval<"1!=2">, true>>,
  Expect<Equal<Eval<"1<2">, true>>,
  Expect<Equal<Eval<"2<1">, false>>,
  Expect<Equal<Eval<"1<=2">, true>>,
  Expect<Equal<Eval<"2<=2">, true>>,
  Expect<Equal<Eval<"3<=2">, false>>,
  Expect<Equal<Eval<"2>1">, true>>,
  Expect<Equal<Eval<"1>2">, false>>,
  Expect<Equal<Eval<"3>=2">, true>>,
  Expect<Equal<Eval<"2>=2">, true>>,
  Expect<Equal<Eval<"1>=2">, false>>,
  Expect<Equal<Eval<"1<2&&3<4">, true>>,
  Expect<Equal<Eval<"1<2&&3>4">, false>>,
  Expect<Equal<Eval<"1<2||3<4">, true>>,
  Expect<Equal<Eval<"1>2||3>4">, false>>,
  // 文字列演算
  Expect<Equal<Eval<"'hello,'+'world!'">, "hello,world!">>,
  Expect<Equal<Eval<"'hello,'+'world!'='hello,world!'">, true>>,
  Expect<Equal<Eval<"'hello'='world'">, false>>,
  Expect<Equal<Eval<"'hello'!='world'">, true>>,
  // 組み込み関数呼び出し
  Expect<Equal<Eval<"len('apple')">, 5>>,
  Expect<Equal<Eval<"len('hello,'+'world!')">, 12>>,
  Expect<Equal<Eval<"len(('hello,')+('world!'))">, 12>>,
  Expect<Equal<Eval<"cast('42','num')">, 42>>,
  Expect<Equal<Eval<"cast('4'+'2','num')">, 42>>,
  Expect<
    Equal<
      Eval<"'hello,world!の文字数は'+cast(len('hello,world!'),'str')+'文字です'">,
      "hello,world!の文字数は12文字です"
    >
  >,
  // 環境
  Expect<Equal<Eval<"r*r*pi", { r: 5; pi: 3 }>, 75>>,
  Expect<
    Equal<
      Eval<"'ようこそ'+username+'さん!'", { username: "Yuta" }>,
      "ようこそYutaさん!"
    >
  >,
  Expect<
    Equal<
      Eval<
        "word+'の文字数は'+cast(len(word),'str')+'文字です'",
        { word: "Alice in Wonderland" }
      >,
      "Alice in Wonderlandの文字数は19文字です"
    >
  >,
  // エッジケース
  Expect<Equal<Eval<"0">, 0>>,
  Expect<Equal<Eval<"1">, 1>>,
  Expect<Equal<Eval<"(0)">, 0>>,
  Expect<Equal<Eval<"(1)">, 1>>,
  Expect<Equal<Eval<"((1))">, 1>>,
  Expect<Equal<Eval<"(((1+2)))">, 3>>,
  Expect<Equal<Eval<"'hello'">, "hello">>,
  Expect<Equal<Eval<"('hello')">, "hello">>,
  Expect<Equal<Eval<"(('hello'))">, "hello">>,
  Expect<Equal<Eval<"''">, "">>,
  Expect<Equal<Eval<"('')">, "">>,
  Expect<Equal<Eval<"((''))">, "">>,
  Expect<Equal<Eval<'"hello"'>, "hello">>,
  Expect<Equal<Eval<'""'>, "">>
];

type errorcases2 = [
  // 空文字
  Expect<IsErr<Eval<"">>>,
  // 不正な数式
  Expect<IsErr<Eval<"-2">>>,
  Expect<IsErr<Eval<"1++2">>>,
  Expect<IsErr<Eval<"+1+2">>>,
  Expect<IsErr<Eval<"1+2+">>>,
  // 演算子未定義
  Expect<IsErr<Eval<"1&&2">>>,
  Expect<IsErr<Eval<"'a'&&'b'">>>,
  Expect<IsErr<Eval<"'a'-'b'">>>,
  Expect<IsErr<Eval<"'a'<'b'">>>
];
