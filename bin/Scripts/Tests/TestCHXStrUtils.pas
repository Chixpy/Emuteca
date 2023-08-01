{
[Info]
This script tests some basic functions added to Pascal Script from uCHXStrUtils.
[Data]
Name=Chixpy
Version=1.0
Date=20230801
[Changes]
v1.0 - 20230801
  * Updating to current uCHXStrUtils.
[EndInfo]
}
program TestCHXStrUtils;
var
  Str1, Str2: string;
begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test added functions to Pascal Script from');
  WriteLn('  uCHXStrUtils.');
  WriteLn('');  

  WriteLn('String utils');    
  WriteLn('------------');    
  WriteLn('function UTF8TextReplace(const S, OldPattern, NewPattern: string; ALanguage: string): string');
  WriteLn('    Replaces a text with another inside a string, case insesitive.');  
  WriteLn('    You can set ALanguage for special collations (''es''), but use ''''.');  
  WriteLn('    UTF8TextReplace(''Example'', ''ex'', ''An ex'', '''') -> ' +
    UTF8TextReplace('Example', 'ex', 'An ex', ''));
  WriteLn('');
  WriteLn('function SimpleStringSplit(const aString, aDelimiter: string; var aStr1, aStr2: string): integer');
  WriteLn('    Splits aString in two strings aStr1 (left string) and aStr2 (right string).');
  SimpleStringSplit('1stStr @ 2ndStr', ' @ ', Str1, Str2);
  WriteLn('    SimpleStringSplit(''1stStr @ 2ndStr'', '' @ '', Str1, Str2) -> ');
  WriteLn('      Str1 = ' + Str1);
  WriteLn('      Str2 = ' + Str2);
  WriteLn('');
  WriteLn('function RemoveFromBrackets(const aString: string): string');
  WriteLn('    Removes from fist "(" or "[" found.');
  WriteLn('    RemoveFromBrackets(''Example (This will be deleted'') -> ' +
    RemoveFromBrackets('Example (This will be delete)'));
  WriteLn('    RemoveFromBrackets(''Example [This will be deleted'') -> ' +
    RemoveFromBrackets('Example [This will be deleted'));
  WriteLn('');  
  WriteLn('function CopyFromBrackets(const aString: string): string');
  WriteLn('    Oposite of previous one, a.k.a. RemoveBeforeBrakets');
  WriteLn('    CopyFromBrackets(''Delete this (Example'') -> ' +
    CopyFromBrackets('Delete this (Example'));
  WriteLn('    CopyFromBrackets(''Delete this [Example'') -> ' +
    CopyFromBrackets('Delete this [Example'));
  WriteLn('');  
  WriteLn('function TextSimilarity(const aString1, aString2: string): byte');
  WriteLn('    Gives the similarity between two strings, between 1-100')
  WriteLn('    TextSimilarity(''An Example'',''For example'') -> ' +
    IntToStr(TextSimilarity('An Example','For example')));
  
  WriteLn('');  
  WriteLn('');  
  WriteLn('Directory utils');    
  WriteLn('---------------');    
  WriteLn('function SetAsFolder(const aValue: string): string');
  WriteLn('    Add trailing path delimiter if needed and not empty.');
  WriteLn('    SetAsFolder(''a\folder'') -> ' + SetAsFolder('a\folder'));
  WriteLn('');  
  WriteLn('function SysPath(const aPath: string): string');
  WriteLn('    Changes directory separators to system ones. Windows recognises');
  WriteLn('      "/" as folder separator, but some FPC functions don''t work well');
  WriteLn('      with Unix style.');
  WriteLn('    SysPath(''a/folder\example/'') -> ' +
    SysPath('a/folder\example/'));
  WriteLn('');  
  WriteLn('function WinPath(const aPath: string): string');
  WriteLn('    Changes directory separators to Windows style.');
  WriteLn('    WinPath(''a/folder\example/'') -> ' +
    WinPath('a/folder\example/'));
  WriteLn('');  
  WriteLn('function UnixPath(const aPath: string): string');
  WriteLn('    Changes directory separators to Unix style.');
  WriteLn('    UnixPath(''a/folder\example/'') -> ' +
    UnixPath('a/folder\example/'));

  WriteLn('');  
  WriteLn('');  
  WriteLn('Filename utils');    
  WriteLn('---------------');    
  WriteLn('function CleanFileName(const AFileName: string; const DoTrim: boolean = True; const PathAware: boolean = False): string');
  WriteLn('    Cleans a filename string, changing forbiden chars.');
  WriteLn('    DoTrim: Trims spaces in leading and trailing spaces in filenames');
  WriteLn('      and directories.');
  WriteLn('    PathAware: Do not change "/" and "\"');
  WriteLn('    CleanFileName(''An:invalid?/ filename?.txt'', true, true) -> ' +
    CleanFileName('An:invalid?/ filename?.txt', true, true));
  WriteLn('    CleanFileName(''An:invalid?/ filename?.txt'', true, false) -> ' +
    CleanFileName('An:invalid?/ filename?.txt', true, false));
  WriteLn('');
  WriteLn('function SetAsRelativeFile(const aFileName: string; BaseDir: string): string');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function SetAsAbsoluteFile(const aFileName: string; BaseDir: string): string');
  WriteLn('    Creates an absolute path from a relative one and a base dir.');
  WriteLn('    SetAsAbsoluteFile(''..\sub/folder/..\example.file'', ''base/folder'') -> ' +
    SetAsAbsoluteFile('..\sub/folder/..\example.file', 'base/folder'));
  WriteLn('');  
  WriteLn('function SetAsFile(const aFileName: string): string');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function SupportedExtCT(aFilename: string; aExtCT: string): boolean');
  WriteLn('function SupportedExtSL(aFilename: string; aExt: TStrings): boolean');
  WriteLn('    Checks if a file has a supported extension.');
  WriteLn('    SupportedExtCT(''TextFile.txt'',''txt,bat'') -> ' +
    BoolToStr(SupportedExtCT('TextFile.txt', 'txt,bat')));

  WriteLn('');  
  WriteLn('');  
  WriteLn('StringList utils');    
  WriteLn('----------------');    
  WriteLn('procedure CleanStringList(aStringList: TStrings; CommentChar: string)');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function AddToStringList(aList: TStrings; aString: string): integer');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function FileMaskFromStringList(aList: TStrings): string');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function FileMaskFromCommaText(aText: string): string');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');

  WriteLn('');  
  WriteLn('');  
  WriteLn('Misc');    
  WriteLn('----');    
  WriteLn('procedure StandardFormatSettings');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function StrCount(aString, ToSearch: string; CaseSensitve: boolean = False): cardinal');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function StrToCardinal(const aString: string): cardinal');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function StrToCardinalDef(const aString: string; const Default: cardinal): cardinal');
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');
  WriteLn('');  
  WriteLn('function SecondsToFmtStr(aValue: int64): string'); 
  WriteLn('    Some info...');
  WriteLn('    An example... -> ');

  WriteLn('');  
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
