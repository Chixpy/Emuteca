{
[Info]
This script test some basic functions added to Pascal Script.
[Author]
Name=Chixpy
Date=20170821
[Script]
}
program TestBasic;

var
  i: integer;
  aStr, aFolder, aFile: string;
  aBool: boolean;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test Pascal Script and Emuteca data.');
  WriteLn('');
  WriteLn('Some Basic functions');
  WriteLn('--------------------');

  // General strings
  WriteLn('function WriteLn(aStr: string);');
  WriteLn('  is already working well. :-D');
  WriteLn('');
  WriteLn('function ReadLn(const aQuestion, DefAnswer: String): String;');
  aStr := ReadLn('Please write something', 'Your name for example');
  WriteLn('  Did you write your name? ' + aStr);
  WriteLn('');
  WriteLn('function CompareText(const S1, S2: String): Integer;');
  WriteLn('  This is case insensitive comparison:');
  WriteLn('    CompareText(''emuteca'', ''emuteca'') -> ' +
    IntToStr(CompareText('emuteca', 'emuteca')));
  WriteLn('');
  WriteLn('function CompareStr(const S1, S2: String): Integer;');
  WriteLn('  This is case sensitive comparison:');
  WriteLn('    CompareStr(''emuteca'', ''Emuteca'') -> ' +
    IntToStr(CompareStr('emuteca', 'Emuteca')));
  WriteLn('    CompareStr(''Emuteca'', ''emuteca'') -> ' +
    IntToStr(CompareStr('Emuteca', 'emuteca')));
  WriteLn('  (Remember "E" < "e")');
  WriteLn('');
  WriteLn('function IntToStr(aInt: integer): string;');
  WriteLn('  This function was used in the previous test to display the');
  WriteLn('    result with WriteLn. But it''s a Pascal Script core function.');
  WriteLn('  In fact, all default PS core plugins are imported, but...');
  WriteLn('');
  WriteLn('function RPos(const Substr: String; const Source: String) : Integer;');
  WriteLn('  ... I had to import a function to search a subtring from right.');
  WriteLn('    RPos(''e'',''Emuteca'') -> ' +
    IntToStr(RPos('e', 'emuteca')));
  WriteLn('');
  WriteLn('function BoolToStr(const aBool: Boolean): String;');
  WriteLn('  Another common function that I had imported.');
  WriteLn('');
  WriteLn('function UTF8ToSys(const S: String): String; and');
  WriteLn('function SysToUTF8(const S: String): String;');
  WriteLn('  When opening files or fix a string encode problem, may be');
  WriteLn('    you want to use these functions.');
  WriteLn('');
  WriteLn('function LowerCase(const AInStr: String): String; and');
  WriteLn('function UpperCase(const AInStr: String): String;');
  WriteLn('  Not much to explain...');
  WriteLn('    LowerCase(''LOWERCASE: ÁÉÍÓÚÜÑÇ'') -> ' +
    LowerCase('LOWERCASE: ÁÉÍÓÚÜÑÇ'));
  WriteLn('    UpperCase(''uppercase: áéíóúüñç'') -> ' +
    UpperCase('uppercase: áéíóúüñç'));

  // Dialogs
  WriteLn('');
  WriteLn('function AskFolder(const aTitle, DefFolder: String): String; and');
  WriteLn('function AskFile(const aTitle, aExt, DefFile: String): String;');
  aFolder := AskFolder('Select a random folder', '');
  aFile := AskFile('Select random file', '', aFolder);
  WriteLn('  You can ask for files or folders!!!');
  WriteLn('    aFolder = ' + aFolder);
  WriteLn('    aFile   = ' + aFile);
  WriteLn('  And there are some functions to simplify this kind of strings.');

  // Filename strings
  WriteLn('');
  WriteLn('function CleanFileName(const aFileName: String): String;');
  WriteLn('  Remove or change some invalid characters in Filenames.');
  WriteLn('  It''s NOT folder aware.');
  WriteLn('    CleanFileName(''Inv: char?.txt'') -> ' +
    CleanFileName('Inv: char?.txt'));
  WriteLn('    CleanFileName(''A/folder/with/file.txt'') -> ' +
    CleanFileName('A/folder/with/file.txt'));
  WriteLn('');
  WriteLn('function ExcludeTrailingPathDelimiter(const aString: String): String;');
  WriteLn('  Removes folder ending ("/" or "\") in a folder name');
  WriteLn('    ExcludeTrailingPathDelimiter(''A\folder\'') -> ' +
    ExcludeTrailingPathDelimiter('A\folder\'));
  WriteLn('    ExcludeTrailingPathDelimiter(''' + aFolder +
    ''') -> ' + ExcludeTrailingPathDelimiter(aFolder));
  WriteLn('');
  WriteLn('function ExtractFilePath(const aFileName: String): String;');
  WriteLn('  Extracts a folder path.');
  WriteLn('    ExtractFilePath(''' + aFile + ''') -> ' +
    ExtractFilePath(aFile));
  WriteLn('    ExtractFilePath(''' + aFolder + ''') -> ' +
    ExtractFilePath(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileName(const aFileName: String): String;');
  WriteLn('  Extaract file name.');
  WriteLn('    ExtractFileName(''' + aFile + ''') -> ' +
    ExtractFileName(aFile));
  WriteLn('    ExtractFileName(''' + aFolder + ''') -> ' +
    ExtractFileName(aFolder));
  WriteLn('');
  WriteLn('function ExtractFileNameOnly(const AFilename: String): String;');
  WriteLn('  Extracts filename without extension.');
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
  WriteLn('  Changes file extension.');
  WriteLn('    ChangeFileExt(''' + aFile + ''',''.emu'') -> ' +
    ChangeFileExt(aFile,'.emu'));
  WriteLn('    ChangeFileExt(''' + aFolder + ''',''.emu'') -> ' +
    ChangeFileExt(aFolder,'.emu'));

  // Files and Folders UTF8
  WriteLn('');
  WriteLn('function FileExistsUTF8(const aFileName: String): Boolean;');
  WriteLn('  Search if a file exists, UTF8 string aware.');
  WriteLn('    FileExistsUTF8(''' + aFile + ''') -> ' +
    BoolToStr(FileExistsUTF8(aFile)));
  WriteLn('    FileExistsUTF8(''' + aFolder + ''') -> ' +
    BoolToStr(FileExistsUTF8(aFolder)));
  WriteLn('');
  WriteLn('function DirectoryExistsUTF8(const aFileName: String): Boolean;');
  WriteLn('  Search if a folder exists, UTF8 string aware.');
  WriteLn('    DirectoryExistsUTF8(''' + aFile + ''') -> ' +
    BoolToStr(DirectoryExistsUTF8(aFile)));
  WriteLn('    DirectoryExistsUTF8(''' + aFolder + ''') -> ' +
    BoolToStr(DirectoryExistsUTF8(aFolder)));

  // HACK: We can't create Stringlist!!!
  WriteLn('');
  WriteLn('function CreateStringList: TStringList;');
  WriteLn('  HACK: Pascal Script can''t create TStringList.');
  WriteLn('  I don''t know if can create other object now...');

  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
