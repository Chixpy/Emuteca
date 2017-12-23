{
[Info]
This script test functions added to Pascal Script from FPC StrUtils.
[Data]
Name=Chixpy
Version=0.01
Date=20171104
[Changes]

[EndInfo]
}
program TestFPCStrUtils;
var
  aString: string;
begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This script test functions added to Pascal Script from FPC StrUtils.');
  WriteLn('');
  WriteLn('About comparing, only as a reminder:');
  WriteLn('  Many comparing functions return an integer (X):');
  WriteLn('    if X < 0, first item is lower.');
  WriteLn('    if X = 0, both items are equal.');
  WriteLn('    if X > 0, second item is lower.');
  WriteLn('  With numbers is easy, only you need to substract second item from first.');

  WriteLn('');  
  WriteLn('Case insensitive search/replace');
  WriteLn('-------------------------------');
  WriteLn('');  

  WriteLn('function AnsiResemblesText(const AText, AOther: string): Boolean;'); 
  WriteLn('    Compares 2 strings with SoundEx Compare. Don''t ask how it works.');
  WriteLn('    AnsiResemblesText(''An example'', ''ONE EXAMPLE'') -> ' +
    BoolToStr(AnsiResemblesText('An example','ONE EXAMPLE')));
  WriteLn('');
  WriteLn('function AnsiContainsText(const AText, ASubText: string): Boolean;');
  WriteLn('    Tests if a text is contained in another.');
  WriteLn('    AnsiContainsText(''An example'', ''EXA'') -> ' +
    BoolToStr(AnsiContainsText('An example', 'EXA')));
  WriteLn('');
  WriteLn('function AnsiStartsText(const ASubText, AText: string): Boolean;');
  WriteLn('    Tests if a text begins with another text.');
  WriteLn('    AnsiStartsText(''AN '', ''An example'') -> ' +
    BoolToStr(AnsiStartsText( 'AN ','An example')));
  WriteLn('');
  WriteLn('function AnsiEndsText(const ASubText, AText: string): Boolean;');
  WriteLn('    Tests if a text ends with another text.');
  WriteLn('    AnsiEndsText(''AMPLE'', ''An example'') -> ' +
    BoolToStr(AnsiEndsText('AMPLE', 'An example')));
  WriteLn('');
  WriteLn('function AnsiReplaceText(const AText, AFromText, AToText: string): string;');
  WriteLn('    Replaces a text with another.');
  WriteLn('    AnsiReplaceText(''An example'', ''EXAMPLE'', ''apple'')-> ' +
    AnsiReplaceText('An example', 'EXAMPLE', 'apple'));
  WriteLn('');
  //WriteLn('function AnsiMatchText(const AText: string; const AValues: array of string): Boolean;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  //WriteLn('function AnsiIndexText(const AText: string; const AValues: array of string): Integer;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');

  WriteLn('');  
  WriteLn('Case sensitive search/replace');
  WriteLn('-----------------------------');
  WriteLn('');  

  WriteLn('function AnsiContainsStr(const AText, ASubText: string): Boolean;');
  WriteLn('    Tests if a string is contained in another.');
  WriteLn('    AnsiContainsStr(''An example'', ''EXA'') -> ' +
    BoolToStr(AnsiContainsStr('An example', 'EXA')));
  WriteLn('');
  WriteLn('function AnsiStartsStr(const ASubText, AText: string): Boolean;');
  WriteLn('    Tests if a string begins with another string.');
  WriteLn('    AnsiStartsStr(''AN '', ''An example'') -> ' +
    BoolToStr(AnsiStartsStr( 'AN ','An example')));
  WriteLn('');
  WriteLn('function AnsiEndsStr(const ASubText, AText: string): Boolean;');
  WriteLn('    Tests if a string ends with another string.');
  WriteLn('    AnsiEndsStr(''AMPLE'', ''An example'') -> ' +
    BoolToStr(AnsiEndsStr('AMPLE', 'An example')));
  WriteLn('');
  WriteLn('function AnsiReplaceStr(const AText, AFromText, AToText: string): string;');
  WriteLn('    Replaces a string with another string.');
  WriteLn('    AnsiReplaceStr(''An example'', ''EXAMPLE'', ''apple'')-> ' +
    AnsiReplaceStr('An example', 'EXAMPLE', 'apple'));
  WriteLn('');
  //WriteLn('function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  //WriteLn('function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  //WriteLn('function MatchStr(const AText: UnicodeString; const AValues: array of UnicodeString): Boolean;');
  //WriteLn('function IndexStr(const AText: UnicodeString; const AValues: array of UnicodeString): Integer;');

  WriteLn('');  
  WriteLn('Miscellaneous');
  WriteLn('-------------');
  WriteLn('');  

  WriteLn('function DupeString(const AText: string; ACount: Integer): string;');
  WriteLn('    Repeats a string.');
  WriteLn('    DupeString(''Example'', 3) -> ' + DupeString('Example', 3));
  WriteLn('');
  WriteLn('function ReverseString(const AText: string): string;');
  WriteLn('    Reverses a string');
  WriteLn('    ReverseString(''An example'') -> ' + ReverseString('An example'));
  WriteLn('');
  //WriteLn('function AnsiReverseString(const AText: AnsiString): AnsiString;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  WriteLn('function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string;');
  WriteLn('    Inserts a string inside other');
  WriteLn('    StuffString(''An example'', 3, 5, ''other'') -> ' + 
    StuffString('An example', 3, 5, 'other'));
  WriteLn('');
  //WriteLn('function RandomFrom(const AValues: array of string): string;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  WriteLn('function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;');
  WriteLn('    Selects a string, using a boolean value.');
  WriteLn('    IfThen(True, ''Boolean is True'', ''Boolean is False'') -> ' + 
    IfThen(True, 'Boolean is True', 'Boolean is False'));
  WriteLn('    IfThen(False, ''Boolean is True'', ''Boolean is False'') -> ' + 
    IfThen(False, 'Boolean is True', 'Boolean is False'));
  WriteLn('');
  WriteLn('function NaturalCompareText (const S1, S2 : string): Integer;');
  WriteLn('    Compares two strings, keeping number order.');
  WriteLn('    NaturalCompareText (''Example 1'', ''Example 02'') -> ' +
    IntToStr(NaturalCompareText ('Example 1', 'Example 02')));
  WriteLn('    NaturalCompareText (''Example 1000'', ''Example 2'') -> ' +
    IntToStr(NaturalCompareText ('Example 1000', 'Example 2')));
  WriteLn('');
  //WriteLn('function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: Char): Integer;');

  WriteLn('');  
  WriteLn('VB emulations');
  WriteLn('-------------');
  WriteLn('');  

  WriteLn('function LeftStr(const AText: string; const ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    LeftStr(''An example'', 3) -> ' + LeftStr('An example', 3));
  WriteLn('');
  WriteLn('function RightStr(const AText: string; const ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    RightStr(''An example'', 3) -> ' + RightStr('An example', 3));
  WriteLn('');
  WriteLn('function MidStr(const AText: string; const AStart, ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    MidStr(''An example'', 4, 3) -> ' + MidStr('An example', 4, 3));
  WriteLn('');
  WriteLn('function RightBStr(const AText: string; const AByteCount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    RightBStr(''An example'', 3) -> ' + RightBStr('An example', 3));
  WriteLn('');
  WriteLn('function MidBStr(const AText: string; const AByteStart, AByteCount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    MidBStr(''An example'', 4, 3) -> ' + 
    MidBStr('An example', 4, 3));
  WriteLn('');
  WriteLn('function AnsiLeftStr(const AText: string; const ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    AnsiLeftStr(''An example'', 3) -> ' + 
    AnsiLeftStr('An example', 3));
  WriteLn('');
  WriteLn('function AnsiRightStr(const AText: string; const ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    AnsiRightStr(''An example'', 3) -> ' + 
    AnsiRightStr('An example', 3));
  WriteLn('');
  WriteLn('function AnsiMidStr(const AText: string; const AStart, ACount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    AnsiMidStr(''An example'', 4, 3) -> ' + 
    AnsiMidStr('An example', 4, 3));
  WriteLn('');
  WriteLn('function LeftBStr(const AText: string; const AByteCount: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    LeftBStr(''An example'', 3) -> ' + LeftBStr('An example', 3));
  WriteLn('');
 
  WriteLn('');  
  WriteLn('Extended search and replace');
  WriteLn('---------------------------');
  WriteLn('');  

  //WriteLn('const WordDelimiters = [#0..#255] - [''a''..''z'',''A''..''Z'',''1''..''9'',''0''];');
  WriteLn('const SErrAmountStrings = ''' + SErrAmountStrings + ''';');
  //TStringSearchOption = (soDown, soMatchCase, soWholeWord);
  //TStringSearchOptions = set of TStringSearchOption;
  //TStringSeachOption = TStringSearchOption;
  //WriteLn('function SearchBuf(Buf: PChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String; Options: TStringSearchOptions): PChar;');
  //WriteLn('function SearchBuf(Buf: PChar; BufLen: SizeInt; SelStart, SelLength: SizeInt; SearchString: String): PChar;');
  WriteLn('function PosEx(const SubStr, S: string; Offset: SizeUint): SizeInt;');
  WriteLn('    Searches a string a position');
  WriteLn('    PosEx(''e'', ''An example'', 5) -> ' + IntToStr(PosEx('e', 'An example', 5)));
  WriteLn('');
  //WriteLn('function PosEx(const SubStr, S: string): SizeInt;');
  //WriteLn('function PosEx(c:char; const S: string; Offset: SizeUint): SizeInt;');
  //WriteLn('function PosEx(const SubStr, S: UnicodeString; Offset: SizeUint): SizeInt;');
  //WriteLn('function PosEx(c: WideChar; const S: UnicodeString; Offset: SizeUint): SizeInt;');
  //WriteLn('function PosEx(const SubStr, S: UnicodeString): Sizeint;');
  //WriteLn('function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;');

  WriteLn('');  
  WriteLn('Delphi compat');
  WriteLn('-------------');
  WriteLn('');  

  WriteLn('function ReplaceStr(const AText, AFromText, AToText: string): string;');
  WriteLn('    Same as AnsiReplaceStr (Case sensitive replace). ');
  WriteLn('');
  WriteLn('function ReplaceText(const AText, AFromText, AToText: string): string;');
  WriteLn('    Same as AnsiReplaceText (Case insensitive replace).');
  WriteLn('');

  WriteLn('');  
  WriteLn('Soundex Functions');
  WriteLn('-----------------');
  WriteLn('');  

  //WriteLn('type TSoundexLength = 1..MaxInt;');

  //WriteLn('function Soundex(const AText: string; ALength: TSoundexLength): string;');
  //WriteLn('function Soundex(const AText: string): string;');

  //WriteLn('type TSoundexIntLength = 1..8');

  //WriteLn('function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;');
  WriteLn('function SoundexInt(const AText: string): Integer;');
  WriteLn('    ???');
  WriteLn('    SoundexInt(''Example'') -> ' + IntToStr(SoundexInt('Example')));
  WriteLn('');
  WriteLn('function DecodeSoundexInt(AValue: Integer): string;');
  WriteLn('    ???');
  WriteLn('    DecodeSoundexInt(36) -> ' + DecodeSoundexInt(36));
  WriteLn('');
  WriteLn('function SoundexWord(const AText: string): Word;');
  WriteLn('    ???');
  WriteLn('    SoundexWord(''Example'') -> ' + IntToStr(SoundexWord('Example')));
  WriteLn('');
  WriteLn('function DecodeSoundexWord(AValue: Word): string;');
  WriteLn('    ???');
  WriteLn('    DecodeSoundexWord(5230) -> ' + DecodeSoundexWord(5230));
  WriteLn('');
  //WriteLn('function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  WriteLn('function SoundexSimilar(const AText, AOther: string): Boolean;');
  WriteLn('    ???');
  WriteLn('    SoundexSimilar(''An example'', ''ONE EXAMPLE'') -> ' +
    BoolToStr(SoundexSimilar('An example','ONE EXAMPLE')));
  WriteLn('');
  //WriteLn('function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;');
  WriteLn('function SoundexCompare(const AText, AOther: string): Integer;');
  WriteLn('    ???');
  WriteLn('    SoundexCompare(''An example'', ''ONE EXAMPLE'') -> ' +
    IntToStr(SoundexCompare('An example','ONE EXAMPLE')));
  WriteLn('');
  WriteLn('function SoundexProc(const AText, AOther: string): Boolean;');
  WriteLn('    ???');
  WriteLn('    SoundexProc(''An example'', ''ONE EXAMPLE'') -> ' +
    BoolToStr(SoundexProc('An example','ONE EXAMPLE')));
  WriteLn('');

  //Type
  //  TCompareTextProc = Function(const AText, AOther: string): Boolean;
  //Const
  //  AnsiResemblesProc: TCompareTextProc = @SoundexProc;

  WriteLn('');  
  WriteLn('Other functions, based on RxStrUtils');
  WriteLn('------------------------------------');
  WriteLn('');  
  
  WriteLn('type TRomanConversionStrictness = (rcsStrict, rcsRelaxed, rcsDontCare)');
  WriteLn('');
  WriteLn('const SInvalidRomanNumeral = ' + SInvalidRomanNumeral);
  WriteLn('');
  //WriteLn('function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;');
  WriteLn('function DelSpace(const S: string): string;');
  WriteLn('    Removes spaces.');
  WriteLn('    DelSpace('' An      example '') -> ' + DelSpace(' An      example '));
  WriteLn('');
  //WriteLn('function DelChars(const S: string; Chr: Char): string;');
  WriteLn('function DelSpace1(const S: string): string;');
  WriteLn('    Removes multiple space with only one.');
  WriteLn('    DelSpace1('' An     example '') -> ' + DelSpace1(' An     example '));
  WriteLn('');
  WriteLn('function Tab2Space(const S: string; Numb: Byte): string;');
  WriteLn('    Converts TAB charater with N spaces');
  WriteLn('    Some example... -> ');
  WriteLn('');
  WriteLn('function NPos(const C: string; S: string; N: Integer): SizeInt;');
  WriteLn('    Returns de N iteration of S substring position.');
  WriteLn('    NPos(''e'', ''An example'', 2) -> ' + 
    IntToStr(NPos('e', 'An example', 2)));
  WriteLn('');
  WriteLn('function RPosEx(Const Substr, Source: string; Offset: SizeInt): SizeInt;');
  WriteLn('    Searchs a Substr substring from the right of Source string,');
  WriteLn('      from Offset position (counted from left).');
  WriteLn('    RPosEx(''e'', ''An example'', 2) -> ' + 
    IntToStr(RPosEx('e', 'An example', 2)));
  WriteLn('    RPosEx(''e'', ''An example'', 5) -> ' +
    IntToStr(RPosEx('e', 'An example', 5)));
  WriteLn('');
  WriteLn('function RPos(Const Substr, Source: string) : SizeInt;');
  WriteLn('    Searchs a substring from the right.');
  WriteLn('    RPos(''e'', ''An example'') -> ' + 
    IntToStr(RPos('e', 'An example')));
  WriteLn('');
  //WriteLn('function AddChar(C: Char; const S: string; N: Integer): string;');
  //WriteLn('function AddCharR(C: Char; const S: string; N: Integer): string;');
  WriteLn('function PadLeft(const S: string; N: Integer): string;');
  WriteLn('    Some explanation...');
  WriteLn('    PadLeft(''      An example'', 3) -> ' + PadLeft('An example', 3));
  WriteLn('');
  WriteLn('function PadRight(const S: string; N: Integer): string;');
  WriteLn('    Some explanation...');
  WriteLn('    PadRight(''An example'', 15) -> ' + PadRight('An example', 15));
  WriteLn('');
  WriteLn('function PadCenter(const S: string; Len: SizeInt): string;');
  WriteLn('    Some explanation...');
  WriteLn('    PadCenter(''An example'', 15) -> ' + PadCenter('An example', 15));
  WriteLn('');
  //WriteLn('function Copy2Symb(const S: string; Symb: Char): string;');
  //WriteLn('function Copy2SymbDel(var S: string; Symb: Char): string;');
  WriteLn('function Copy2Space(const S: string): string;');
  WriteLn('    Copies substring before an space.');
  WriteLn('    Copy2Space(''An example'') -> ' + Copy2Space('An example'));
  WriteLn('');
  WriteLn('function Copy2SpaceDel(var S: string): string;');
  WriteLn('    Splits S string variable, returning the substring before a space,');
  WriteLn('      and removing it from S string variable.');
  aString := 'An Example';
  WriteLn('    aString := ''An Example''; Copy2SpaceDel(aString) -> ' + Copy2SpaceDel(aString));
  WriteLn('    aString -> ' + aString);
  WriteLn('');
  //WriteLn('function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;');
  //WriteLn('function WordCount(const S: string; const WordDelims: TSysCharSet): SizeInt;');
  //WriteLn('function WordPosition(const N: Integer; const S: string; const WordDelims: TSysCharSet): SizeInt;');
  //WriteLn('function ExtractWord(N: Integer; const S: string;  const WordDelims: TSysCharSet): string;');
  //WriteLn('function ExtractWordPos(N: Integer; const S: string; const WordDelims: TSysCharSet; out Pos: Integer): string;');
  //WriteLn('function ExtractDelimited(N: Integer; const S: string;  const Delims: TSysCharSet): string;');
  //WriteLn('function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: TSysCharSet): string;');
  //WriteLn('function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;');
  WriteLn('function FindPart(const HelpWilds, InputStr: string): SizeInt;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  WriteLn('function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  //WriteLn('function XorString(const Key, Src: ShortString): ShortString;');
  WriteLn('function XorEncode(const Key, Source: string): string;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  WriteLn('function XorDecode(const Key, Source: string): string;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  //WriteLn('function GetCmdLineArg(const Switch: string; SwitchChars: TSysCharSet): string;');
  WriteLn('function Numb2USA(const S: string): string;');
  WriteLn('    Some explanation...');
  WriteLn('    Numb2USA(''1323250.45'') -> ' + Numb2USA('1323250.45'));
  WriteLn('    Decimals wrong....');
  WriteLn('');
  WriteLn('function Hex2Dec(const S: string): Longint;');
  WriteLn('    Some explanation...');
  WriteLn('    Hex2Dec(''AF2'') -> ' + IntToStr(Hex2Dec('AF2')));
  WriteLn('');
  WriteLn('function Dec2Numb(N: Longint; Len, Base: Byte): string;');
  WriteLn('    Some explanation...');
  WriteLn('    Dec2Numb(1000, 10, 8) -> ' + Dec2Numb(1000, 10, 8));
  WriteLn('');
  WriteLn('function Numb2Dec(S: string; Base: Byte): Longint;');
  WriteLn('    Some explanation...');
  WriteLn('    Numb2Dec(''0000001750'', 8) -> ' + IntToStr(Numb2Dec('0000001750', 8)));
  WriteLn('');
  WriteLn('function IntToBin(Value: Longint; Digits, Spaces: Integer): string;');
  WriteLn('    Some explanation...');
  WriteLn('    IntToBin(125, 8, 4) -> ' + IntToBin(125, 8, 4));
  WriteLn('');
  WriteLn('function IntToRoman(Value: Longint): string;');
  WriteLn('    Some explanation...');
  WriteLn('    IntToRoman(125) -> ' + IntToRoman(125));
  WriteLn('');
  WriteLn('function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness): Boolean;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  WriteLn('function RomanToInt(const S: string; Strictness: TRomanConversionStrictness): Longint;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  WriteLn('function RomanToIntDef(Const S : String; const ADefault: Longint; Strictness: TRomanConversionStrictness): Longint;');
  WriteLn('    Some explanation...');
  WriteLn('    Some example... -> ');
  WriteLn('');
  //WriteLn('procedure BinToHex(BinValue, HexValue: PChar; BinBufSize: Integer);');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');
  //WriteLn('function HexToBin(HexValue, BinValue: PChar; BinBufSize: Integer): Integer;');
  //WriteLn('    Some explanation...');
  //WriteLn('    Some example... -> ');
  //WriteLn('');

  //const
  //  DigitChars = ['0'..'9'];
  //  Brackets = ['(',')','[',']','{','}'];
  //  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;
  //  StdSwitchChars = ['-','/'];

  //WriteLn('function PosSet (const c:TSysCharSet;const s : ansistring ):SizeInt;');
  //WriteLn('function PosSetEx (const c:TSysCharSet;const s : ansistring;count:Integer ):SizeInt;');

  //WriteLn('procedure Removeleadingchars(VAR S : AnsiString; Const CSet:TSysCharset);');
  //WriteLn('procedure RemoveTrailingChars(VAR S : AnsiString;Const CSet:TSysCharset);');
  //WriteLn('procedure RemovePadChars(VAR S : AnsiString;Const CSet:TSysCharset);');

  //WriteLn('function TrimLeftSet(const S: String;const CSet:TSysCharSet): String;');
  //WriteLn('function TrimRightSet(const S: String;const CSet:TSysCharSet): String;');
  //WriteLn('function TrimSet(const S: String;const CSet:TSysCharSet): String;');

  //type
  //  SizeIntArray = array of SizeInt;

  //WriteLn('procedure FindMatchesBoyerMooreCaseSensitive(const S,OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean);');

  //WriteLn('procedure FindMatchesBoyerMooreCaseInSensitive(const S, OldPattern: String; out aMatches: SizeIntArray; const aMatchAll: Boolean);');

  WriteLn('type TStringReplaceAlgorithm = (sraDefault, sraManySmall, sraBoyerMoore);');

  //WriteLn('function StringReplace(const S, OldPattern, NewPattern: string; Flags: TReplaceFlags; Algorithm : TStringReplaceAlgorithm): string;');

  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
