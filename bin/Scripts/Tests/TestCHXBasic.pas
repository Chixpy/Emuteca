{
[Info]
This script test some basic functions added to Pascal Script from uPSI_CHXBasic.
[Data]
Name=Chixpy
Version=0.03
Date=20230116
[Changes]
0.03 - 20230116
  + LineEnding
0.02 - 20221021
  + AskOption example
[EndInfo]
}
program TestCHXBasic;

var
  i: integer;
  aStr, aFolder, aFile: string;
  aFileList: TStringList;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test some basic functions added to Pascal Script.');
  WriteLn('');

  WriteLn('Some basic types');
  WriteLn('----------------');
  WriteLn('');
  WriteLn('type SizeInt = Int64');
  WriteLn('type SizeUInt = Int64 (must be QWord...)');

  WriteLn('');
  WriteLn('');
  
  WriteLn('Some basic constants');
  WriteLn('--------------------');
  WriteLn('');
  WriteLn('const LineEnding = #13#10 (Win)');

  WriteLn('');
  WriteLn('');

  WriteLn('Some basic functions');
  WriteLn('--------------------');

  // General strings
  WriteLn('procedure WriteLn(aStr: string);');
  WriteLn('  is already working well. :-D');
  WriteLn('');
  WriteLn('function ReadLn(const aQuestion, DefAnswer: String): String;');
  aStr := ReadLn('Please write something', 'Your name for example');
  WriteLn('    Did you write your name? ' + aStr);
  WriteLn('');
  WriteLn('function CompareText(const S1, S2: String): Integer;');
  WriteLn('    This is case insensitive comparison:');
  WriteLn('    CompareText(''EMUTECA'', ''emuteca'') -> ' +
    IntToStr(CompareText('EMUTECA', 'emuteca')));
  WriteLn('');
  WriteLn('function CompareStr(const S1, S2: String): Integer;');
  WriteLn('    This is case sensitive comparison:');
  WriteLn('    CompareStr(''emuteca'', ''Emuteca'') -> ' +
    IntToStr(CompareStr('emuteca', 'Emuteca')));
  WriteLn('    CompareStr(''Emuteca'', ''emuteca'') -> ' +
    IntToStr(CompareStr('Emuteca', 'emuteca')));
  WriteLn('  (Remember "E" < "e")');
  WriteLn('');
  WriteLn('function IntToStr(aInt: integer): string;');
  WriteLn('    This function was used in the previous test to display the');
  WriteLn('      result with WriteLn. But it''s a Pascal Script core function.');
  WriteLn('    In fact, all default PS core plugins are imported, but...');
  WriteLn('');
  WriteLn('function RPos(const Substr: String; const Source: String) : Integer;');
  WriteLn('    ... I had to import a function to search a subtring from right.');
  WriteLn('    RPos(''e'',''emuteca'') -> ' + IntToStr(RPos('e', 'emuteca')));
  WriteLn('function PosEx(const SubStr, Source: string; Offset: integer): integer;');
  WriteLn('    ... And another to search a especific position (Offset).');
  WriteLn('    PosEx(''e'',''emuteca'', 3) -> ' + 
    IntToStr(PosEx('e', 'emuteca', 3)));
  WriteLn('    At least until StrUtils is imported.')
  WriteLn('');
  WriteLn('function BoolToStr(const aBool: Boolean): String;');
  WriteLn('    Another common function that I had imported.');
  WriteLn('    BoolToStr(True) -> ' + BoolToStr(True));
  WriteLn('');
  WriteLn('function UTF8ToSys(const S: String): String; and');
  WriteLn('function SysToUTF8(const S: String): String;');
  WriteLn('    When opening files with TStringList or fix a string an encode');
  WriteLn('      problem, may be you want to use these functions.');
  WriteLn('');
  WriteLn('function LowerCase(const AInStr: String): String; and');
  WriteLn('function UpperCase(const AInStr: String): String;');
  WriteLn('    Not much to explain...');
  WriteLn('    LowerCase(''LOWERCASE: ÁÉÍÓÚÜÑÇ'') -> ' +
    LowerCase('LOWERCASE: ÁÉÍÓÚÜÑÇ'));
  WriteLn('    UpperCase(''uppercase: áéíóúüñç'') -> ' +
    UpperCase('uppercase: áéíóúüñç'));

  // Dialogs
  WriteLn('');
  WriteLn('function AskFolder(const aTitle, DefFolder: String): String;');
  aFolder := AskFolder('Select a random folder', '');
  WriteLn('    You can ask for a folder.');
  WriteLn('    aFolder = ' + aFolder);
  WriteLn('');

  WriteLn('function AskFile(const aTitle, aExt, DefFile: String): String;');
  aFile := AskFile('Select random file', '', aFolder);
  WriteLn('    Asking for a file.');
  WriteLn('    aFile   = ' + aFile);
  WriteLn('');

  WriteLn('function AskMultiFile(aFileList: TStrings; const aTitle: string;');
  WriteLn('  const aExtFilter: string; const DefFolder: string): boolean');
  WriteLn('    Asking for multiple files.');
  aFileList := CreateStringList;
  AskMultiFile(aFileList, 'Select many files', '', aFolder) // Result: Boolean
  for i := 0 to aFileList.Count - 1 do
    WriteLn('    aFileList[' + IntToStr(i) + '] = ' + aFileList[i]);
  WriteLn('');

  WriteLn('function AskOption(const aCaption, aQuestion: string');
  WriteLn('  aOptionList: TStrings): integer');
  i := AskOption('Select random option', 'One of selected files', aFileList);
  if i = -1 then
    WriteLn('    You canceled the option select or there was no options.')
  else
    WriteLn('    You selected: ' + IntToStr(i) + ' = ' + aFileList[i]);
  WriteLn('');
  aFileList.Free;

  // Filename strings
  WriteLn('');
  WriteLn('There are some functions to simplify this kind of strings');
  WriteLn('  many of them tested in TestCHXStrUtils. But here are some');
  WriteLn('  basic added.');
  WriteLn('function ExcludeTrailingPathDelimiter(const aString: String): String;');
  WriteLn('    Removes folder ending ("/" or "\") in a folder name');
  WriteLn('    ExcludeTrailingPathDelimiter(''' + aFile + ''') -> ' +
    ExcludeTrailingPathDelimiter(aFile));
  WriteLn('    ExcludeTrailingPathDelimiter(''' + aFolder +
    ''') -> ' + ExcludeTrailingPathDelimiter(aFolder));
  WriteLn('');
  WriteLn('function ExtractFilePath(const aFileName: String): String;');
  WriteLn('    Extracts a folder path.');
  WriteLn('    ExtractFilePath(''' + aFile + ''') -> ' +
    ExtractFilePath(aFile));
  WriteLn('    ExtractFilePath(''' + aFolder + ''') -> ' +
    ExtractFilePath(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileName(const aFileName: String): String;');
  WriteLn('    Extracts file name.');
  WriteLn('    ExtractFileName(''' + aFile + ''') -> ' +
    ExtractFileName(aFile));
  WriteLn('    ExtractFileName(''' + aFolder + ''') -> ' +
    ExtractFileName(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileNameOnly(const AFilename: String): String;');
  WriteLn('    Extracts filename without extension.');
  WriteLn('    ExtractFileNameOnly(''' + aFile + ''') -> ' +
    ExtractFileNameOnly(aFile));
  WriteLn('    ExtractFileNameOnly(''' + aFolder + ''') -> ' +
    ExtractFileNameOnly(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileExt(const aFilename: String): String;');
  WriteLn('  Extracts file extension .');
  WriteLn('    ExtractFileExt(''' + aFile + ''') -> ' + ExtractFileExt(aFile));
  WriteLn('    ExtractFileExt(''' + aFolder + ''') -> ' +
    ExtractFileExt(aFolder));
  WriteLn('');
  WriteLn('function ChangeFileExt(const aFileName, aExtension: String): String;');
  WriteLn('    Changes file extension.');
  WriteLn('    ChangeFileExt(''' + aFile + ''',''.emu'') -> ' +
    ChangeFileExt(aFile,'.emu'));
  WriteLn('    ChangeFileExt(''' + aFolder + ''',''.emu'') -> ' +
    ChangeFileExt(aFolder,'.emu'));
  WriteLn('    CAUTION: You must use ExcludeTrailingPathDelimiter, if you');
  WriteLn('      want to change the extension of a folder.');

  // HACK: We can't create Stringlist!!!
  WriteLn('');
  WriteLn('function CreateStringList: TStringList;');
  WriteLn('    HACK: In current Pascal Script version can''t create TStringList.');
  WriteLn('    I don''t know if can create other objects now...');

  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
