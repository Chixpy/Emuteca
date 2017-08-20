program Test;

var
  i: Integer;
  aStr: string;
  aBool: boolean;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test Pascal Script and Emuteca data.');
  WriteLn('');
  WriteLn('Some Basic functions')
  WriteLn('--------------------')
  WriteLn('function WriteLn(aStr: string);');
  WriteLn('  is already working well. :-D');
  WriteLn('');
  WriteLn ('function ReadLn(const aQuestion, DefAnswer: String): String;');
  aStr := ReadLn('Please write something', 'Your name for example');
  WriteLn('  Did you write your name? ' + aStr);
  WriteLn('');
  WriteLn('function UTF8CompareText(const S1, S2: String): Integer;');
  WriteLn('  This is case insensitive comparison:');
  WriteLn('    UTF8CompareText(''emuteca'', ''Emuteca'') -> ' +
    IntToStr(UTF8CompareText('emuteca', 'Emuteca')));
  WriteLn('');
  WriteLn('function UTF8CompareStr(const S1, S2: String): Integer;');
  WriteLn('  This is case sensitive comparison:');
  WriteLn('    UTF8CompareStr(''emuteca'', ''Emuteca'') -> ' +
    IntToStr(UTF8CompareStr('emuteca', 'Emuteca')));
     WriteLn('    UTF8CompareStr(''Emuteca'', ''emuteca'') -> ' +
    IntToStr(UTF8CompareStr('Emuteca', 'emuteca')));
  WriteLn('  (Remember "E" < "e")');
  WriteLn('');
  WriteLn('function IntToStr(aInt: integer): string;');
  WriteLn('  This function was used in the previous test to display the');
  WriteLn('    result with WriteLn. But it''s a Pascal Script core function.');
  WriteLn('  In fact, all default PS core plugins are imported.');
  WriteLn('');
  WriteLn('function UTF8ToSys(const S: String): String; and');
  WriteLn('function SysToUTF8(const S: String): String;');
  WriteLn('  While opening files or fix a string encode problem, may be');
  WriteLn('    you want to use these functions.');
  WriteLn('');
  WriteLn('function UTF8LowerCase(const AInStr: String): String; and');
  WriteLn('function UTF8UpperCase(const AInStr: String): String;');
  WriteLn('  Not much to explain...');
  WriteLn('    LowerCase(''LOWERCASE: ÁÉÍÓÚÜÑÇ'') -> ' +
    UTF8LowerCase('LOWERCASE: ÁÉÍÓÚÜÑÇ'));
  WriteLn('    UpperCase(''uppercase: áéíóúüñç'') -> ' +
    UTF8UpperCase('uppercase: áéíóúüñç'));


  {
    // Misc string functions
  Sender.AddMethod(Self, @cCHXScriptEngine.RPos,
    'function RPos(const Substr: String; const Source: String) : Integer;');

  // Path and filename strings
  Sender.AddFunction(@CleanFileName,
    'function CleanFileName(const AFileName: String): String;');
  Sender.AddFunction(@ExcludeTrailingPathDelimiter,
    'function ExcludeTrailingPathDelimiter(const aString: String): String;');
  Sender.AddFunction(@ExtractFilePath,
    'function ExtractFilePath(const aFileName: String): String;');
  Sender.AddFunction(@ExtractFileName,
    'function ExtractFileName(const aFileName: String): String;');
  Sender.AddFunction(@ExtractFileNameOnly,
    'function ExtractFileNameOnly(const AFilename: String): String;');
  Sender.AddFunction(@ExtractFileExt,
    'function ExtractFileExt(const AFilename: String): String;');
  Sender.AddFunction(@ChangeFileExt,
    'function ChangeFileExt(const aFileName, aExtension: String): String;');

  // Files and Folders UTF8
  Sender.AddFunction(@FileExistsUTF8,
    'function FileExistsUTF8(const aFileName: String): Boolean;');
  Sender.AddFunction(@DirectoryExistsUTF8,
    'function DirectoryExistsUTF8(const aFileName: String): Boolean;');

  // Dialogs
  Sender.AddMethod(Self, @cCHXScriptEngine.AskFile,
    'function AskFile(const aTitle, aExt, DefFile: String): String;');
  Sender.AddMethod(Self, @cCHXScriptEngine.AskFolder,
    'function AskFolder(const aTitle, DefFolder: String): String;');





  WriteLn('');
  WriteLn('');
  WriteLn('');
  WriteLn('uPSI_uEmutecaCommon.pas');
  WriteLn('-----------------------');
  WriteLn('Some common constants and functions.');
  WriteLn('');

  WriteLn('');
  WriteLn('uPSI_uEmutecaCommon');
  WriteLn('-------------------');
  WriteLn('krsEmuteca: ' +  krsEmuteca);
  WriteLn('krsEmutecaGameSubFolder: ' +  krsEmutecaGameSubFolder);
  WriteLn('kGroupSectionKey: ' +  kGroupSectionKey);
  WriteLn('kEmutecaVirtualFolderExt: ' +  kEmutecaVirtualFolderExt);
  WriteLn('kEmutecaVirtualGroupExt: ' +  kEmutecaVirtualGroupExt);
  WriteLn('kEmutecaVirtualGameExt: ' +  kEmutecaVirtualGameExt);
  WriteLn('kEmutecaExecErrorNoGame: ' +  IntToStr(kEmutecaExecErrorNoGame));
  WriteLn('kEmutecaDecompressError: ' +  IntToStr(kEmutecaDecompressError));
  WriteLn('krsCRC32: ' +  krsCRC32);
  WriteLn('krsSHA1: ' +  krsSHA1);
  WriteLn('krsFileName: ' +  krsFileName);
  WriteLn('krsCustom: ' +  krsCustom);
  WriteLn('krsGameKey: ' +  krsGameKey);
  WriteLn('krsZones: ' +  krsZones);
  WriteLn('krsDeveloper: ' +  krsDeveloper);
  WriteLn('krsPublisher: ' +  krsPublisher);
  WriteLn('krsVersion: ' +  krsVersion);
  WriteLn('krsFilename: ' +  krsFilename);
  WriteLn('krsFmtNItems: ' +  krsFmtNItems);
  WriteLn('krsFmtNTimes: ' +  krsFmtNTimes);
  WriteLn('krsNever: ' +  krsNever);
  WriteLn('krsUnknown: ' +  krsUnknown);
  WriteLn('krsFmtApplicationTitle: ' +  krsFmtApplicationTitle);
  WriteLn('krsFmtWindowCaption: ' +  krsFmtWindowCaption);

  WriteLn('');
  WriteLn('uPSI_ucEmuteca');
  WriteLn('--------------');
  WriteLn('Emuteca.TempFolder: ' + Emuteca.TempFolder);
  }
end.
