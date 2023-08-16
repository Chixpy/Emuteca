{
[Info]
This script test some basic functions added to Pascal Script engine
  internally (FrontEnd iteration) and from uPSI_CHXBasic (SysUtils and
  System FPC units).
[Data]
Name=Chixpy
Version=0.04
Date=20230812
[Changes]
0.04 - 20230812
  + AskYesNoCancel example
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

  WriteLn('=======================================');
  WriteLn('Added internally for frontend iteration');
  WriteLn('=======================================');
  WriteLn('');
  WriteLn('procedure WriteLn(aStr: string);');
  WriteLn('  It is already working well if you read this. :-P');
  WriteLn('');
  WriteLn('function ReadLn(const aQuestion, DefAnswer: string): string');
  aStr := ReadLn('Please write something', 'Your name for example');
  WriteLn('    Did you write your name? ' + aStr);

  WriteLn('');
  WriteLn('');
  WriteLn('More usefull prompts / dialogs');
  WriteLn('------------------------------');
  WriteLn('');
  WriteLn('function AskFolder(const aTitle, DefFolder: string): string;');
  aFolder := AskFolder('Select a random folder', '');
  WriteLn('    You can ask for a folder.');
  WriteLn('    aFolder = ' + aFolder);
  WriteLn('');
  WriteLn('function AskFile(const aTitle, aExt, DefFile: string): string;');
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
  WriteLn('    Asking for an option from a list.');
  i := AskOption('Select random option', 'One of selected files', aFileList);
  if i = -1 then
    WriteLn('    You canceled the option select or there was no options.')
  else
    WriteLn('    You selected: ' + IntToStr(i) + ' = ' + aFileList[i]);
  WriteLn('');
  aFileList.Free;
  WriteLn('');
  WriteLn('function AskYesNoCancel(const aCaption, aQuestion: string): integer');
  WriteLn('    A simple Yes/No/Cancel. Returning mrYes/mrNo/mrCancel');
  i := AskYesNoCancel('An example of YNC', 'Pick a random button.')
  case i of
    mrYes: WriteLn('    You are positive.');
    mrNo: WriteLn('    You are negative.');
    mrCancel: WriteLn('    You are canceled.');
    else
      WriteLn('    You are impossible.');
  end;

  WriteLn('');
  WriteLn('');
  WriteLn('Important Fix/Hack');
  WriteLn('------------------');
  WriteLn('');
  // HACK: We can't create Stringlist!!!
  WriteLn('');
  WriteLn('function CreateStringList: TStringList;');
  WriteLn('    In current Pascal Script version can''t create TStringList.');
  WriteLn('    I don''t know if can create other objects now...');

  WriteLn('');
  WriteLn('');
  WriteLn('');
  WriteLn('======');
  WriteLn('System');
  WriteLn('======');
  WriteLn('');
  WriteLn('Some types and constants');
  WriteLn('------------------------');
  WriteLn('');
  WriteLn('type SizeInt = Int64 (if CPU64)');
  WriteLn('type SizeUInt = Int64 (must be QWord...)');
  WriteLn('');
  WriteLn('const LineEnding = #13#10 (Win)');

  WriteLn('');
  WriteLn('');
  WriteLn('Random functions');
  WriteLn('----------------');
  WriteLn('');
  WriteLn('procedure Randomize;');
  WriteLn('    Inits the random seed.');
  Randomize;
  WriteLn('');
  WriteLn('function Random: Extended;');
  WriteLn('    Random decimal number between in range [0,1). Never 1.');
  WriteLn('    Random -> ' + FloatToStr(Random));
  WriteLn('');
  WriteLn('function RandomInt(l, h: Int64): Int64;');
  WriteLn('    Random integer number in range [l,h]. h and l are included.');
  WriteLn('    RandomInt(5,500) -> ' + IntToStr(RandomInt(5,500)));

  WriteLn('');
  WriteLn('');
  WriteLn('');
  WriteLn('=======');
  WriteLn('SySUtils');
  WriteLn('=======');
  WriteLn('');
  WriteLn('function ChangeFileExt(const aFileName, aExtension: string): string;');
  WriteLn('    Changes file extension.');
  WriteLn('    ChangeFileExt(''' + aFile + ''',''.emu'') -> ' +
    ChangeFileExt(aFile,'.emu'));
  WriteLn('    ChangeFileExt(''' + aFolder + ''',''.emu'') -> ' +
    ChangeFileExt(aFolder,'.emu'));
  WriteLn('    CAUTION: You must use ExcludeTrailingPathDelimiter, if you');
  WriteLn('      want to change the extension of a folder.');
  WriteLn('');
  WriteLn('');
  WriteLn('function ExtractFilePath(const aFileName: String): String;');
  WriteLn('    Extracts a folder path.');
  WriteLn('    ExtractFilePath(''' + aFile + ''') -> ' +
    ExtractFilePath(aFile));
  WriteLn('    ExtractFilePath(''' + aFolder + ''') -> ' +
    ExtractFilePath(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileDrive(const aFilename: String): String;');
  WriteLn('  Extracts file extension .');
  WriteLn('    ExtractFileDrive(''' + aFile + ''') -> ' +
    ExtractFileDrive(aFile));
  WriteLn('    ExtractFileDrive(''' + aFolder + ''') -> ' +
    ExtractFileDrive(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileName(const aFileName: String): String;');
  WriteLn('    Extracts file name.');
  WriteLn('    ExtractFileName(''' + aFile + ''') -> ' +
    ExtractFileName(aFile));
  WriteLn('    ExtractFileName(''' + aFolder + ''') -> ' +
    ExtractFileName(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileExt(const aFilename: String): String;');
  WriteLn('  Extracts file extension .');
  WriteLn('    ExtractFileExt(''' + aFile + ''') -> ' +
    ExtractFileExt(aFile));
  WriteLn('    ExtractFileExt(''' + aFolder + ''') -> ' +
    ExtractFileExt(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileDir(const aFilename: String): String;');
  WriteLn('  Extracts file extension .');
  WriteLn('    ExtractFileDir(''' + aFile + ''') -> ' +
    ExtractFileDir(aFile));
  WriteLn('    ExtractFileDir(''' + aFolder + ''') -> ' +
    ExtractFileDir(aFolder));
  WriteLn('');
  WriteLn('function ExtractShortPathName(const FileName : string) : string;')
  WriteLn('    ExtractShortPathName(''' + aFile + ''') -> ' +
    ExtractFilePath(aFile));
  WriteLn('    ExtractShortPathName(''' + aFolder + ''') -> ' +
    ExtractFilePath(aFolder));
  WriteLn('');
  WriteLn('function ExpandFileName(const FileName : string): string;');
  WriteLn('    ExpandFileName(''' + aFile + ''') -> ' +
    ExpandFileName(aFile));
  WriteLn('    ExpandFileName(''' + aFolder + ''') -> ' +
    ExpandFileName(aFolder));
  // WriteLn('');
  // WriteLn('function ExpandFileNameCase (const FileName: string; out MatchFound: TFilenameCaseMatch): string;
  WriteLn('');
  WriteLn('function ExpandUNCFileName(const FileName : string): string;');
  WriteLn('    ExpandUNCFileName(''' + aFile + ''') -> ' +
    ExpandUNCFileName(aFile));
  WriteLn('    ExpandUNCFileName(''' + aFolder + ''') -> ' +
    ExpandUNCFileName(aFolder));
  WriteLn('');
  WriteLn('function ExtractRelativepath (const BaseName,DestNAme : string): string;');
  WriteLn('    ExtractRelativepath(''' + aFile + ''', ''' + aFolder + ''') -> ' +
    ExtractRelativepath(aFile, aFolder));
  WriteLn('    ExtractRelativepath(''' + aFolder + ''', ''' + aFile + ''') -> ' +
    ExtractRelativepath(aFolder, aFile));
  WriteLn('');
  WriteLn('function IncludeTrailingPathDelimiter(const aString: String): String;');
  WriteLn('    Includes folder ending ("/" or "\") in a folder name.');
  WriteLn('    IncludeTrailingPathDelimiter(''' + aFile + ''') -> ' +
    IncludeTrailingPathDelimiter(aFile));
  WriteLn('    IncludeTrailingPathDelimiter(''' + aFolder +
    ''') -> ' + IncludeTrailingPathDelimiter(aFolder));
  WriteLn('');
  WriteLn('function IncludeTrailingBackslash(const aString: String): String;');
  WriteLn('    Includes folder ending ("/" or "\") in a folder name (again).');
  WriteLn('    IncludeTrailingBackslash(''' + aFile + ''') -> ' +
    IncludeTrailingBackslash(aFile));
  WriteLn('    IncludeTrailingBackslash(''' + aFolder +
    ''') -> ' + IncludeTrailingBackslash(aFolder));
  WriteLn('');
  WriteLn('function ExcludeTrailingBackslash(const aString: String): String;');
  WriteLn('    Removes folder ending ("/" or "\") in a folder name.');
  WriteLn('    ExcludeTrailingBackslash(''' + aFile + ''') -> ' +
    ExcludeTrailingBackslash(aFile));
  WriteLn('    ExcludeTrailingBackslash(''' + aFolder +
    ''') -> ' + ExcludeTrailingBackslash(aFolder));
  WriteLn('');
  WriteLn('function ExcludeTrailingPathDelimiter(const aString: String): String;');
  WriteLn('    Removes folder ending ("/" or "\") in a folder name (again).');
  WriteLn('    ExcludeTrailingPathDelimiter(''' + aFile + ''') -> ' +
    ExcludeTrailingPathDelimiter(aFile));
  WriteLn('    ExcludeTrailingPathDelimiter(''' + aFolder +
    ''') -> ' + ExcludeTrailingPathDelimiter(aFolder));
  WriteLn('');
  WriteLn('function IncludeLeadingPathDelimiter(const Path: string): string;');
  WriteLn('    IncludeLeadingPathDelimiter(''' + aFile + ''') -> ' +
    IncludeLeadingPathDelimiter(aFile));
  WriteLn('    IncludeLeadingPathDelimiter(''' + aFolder + ''') -> ' +
    IncludeLeadingPathDelimiter(aFolder));
  WriteLn('');
  WriteLn('function ExcludeLeadingPathDelimiter(const Path: string): string;');
  WriteLn('    ExcludeLeadingPathDelimiter(''' + aFile + ''') -> ' +
    ExcludeLeadingPathDelimiter(aFile));
  WriteLn('    ExcludeLeadingPathDelimiter(''' + aFolder + ''') -> ' +
    ExcludeLeadingPathDelimiter(aFolder));
  WriteLn('    ExcludeLeadingPathDelimiter(''\actual test\'') -> ' +
    ExcludeLeadingPathDelimiter('\actual test\'));
  WriteLn('');
  WriteLn('function IsPathDelimiter(const Path: string; Index: Integer): Boolean;');
  WriteLn('');
  WriteLn('procedure DoDirSeparators (var FileName : string);');
  WriteLn('');
  WriteLn('function SetDirSeparators (const FileName : string) : string;');
  WriteLn('    SetDirSeparators(''' + aFile + ''') -> ' +
    SetDirSeparators(aFile));
  WriteLn('    SetDirSeparators(''' + aFolder + ''') -> ' +
    SetDirSeparators(aFolder));
  // WriteLn('');
  // WriteLn('function GetDirs (Var DirName : PathStr; Var Dirs : Array of PathPChar) : Longint;'); // {$ifdef FPC_HAS_CPSTRING}rtlproc;{$endif}
  WriteLn('');
  WriteLn('function ConcatPaths(const Paths: array of string): string;');
  WriteLn('     ConcatPaths([''a'',''b'',''..'',''c'']) -> ' +
    ConcatPaths(['a','b','..','c']));









  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
