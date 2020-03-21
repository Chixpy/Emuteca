{
[Info]
This script test some basic functions added to Pascal Script from uPSI_FPCFileUtil.
[Data]
Name=Chixpy
Version=0.01
Date=20200126
[Changes]

[EndInfo]
}
program TestFPCFileUtil;

var
  i: integer;
  aStr, aFolder, aFile: string;
  aFileList: TStringList;

begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This script test functions added to Pascal Script from FPC FileUtil');
  WriteLn('and LazFileUtils');
  WriteLn('');

  aFolder := AskFolder('Select a random folder', '');
  aFile := AskFile('Select random file', '', aFolder);

  // Methods
  // -------

  WriteLn('');
  WriteLn('File operations');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('function FileExists (const aFileName: String): Boolean;');
  WriteLn('  Search if a file exists, UTF8 string aware.');
  WriteLn('    FileExists(''' + aFile + ''') -> ' +
    BoolToStr(FileExists(aFile)));
  WriteLn('    FileExists(''' + aFolder + ''') -> ' +
    BoolToStr(FileExists(aFolder)));
  WriteLn('');
  WriteLn('function DirectoryExists(const aFileName: String): Boolean;');
  WriteLn('  Search if a folder exists, UTF8 string aware.');
  WriteLn('    DirectoryExists(''' + aFile + ''') -> ' +
    BoolToStr(DirectoryExists(aFile)));
  WriteLn('    DirectoryExists(''' + aFolder + ''') -> ' +
    BoolToStr(DirectoryExists(aFolder)));

  WriteLn('');
  WriteLn('File search');
  WriteLn('-----------');
  WriteLn('');

  WriteLn('procedure FindAllFiles(AList: TStrings; const SearchPath: string; SearchMask: string; SearchSubDirs: Boolean);');
  WriteLn('  Search all files in a folder.');
  WriteLn('  * AList: StringList where all filenames will be stored.');
  WriteLn('  * SearchPath: Folder to search files.');
  WriteLn('  * SearchMask: Filemask of searching. Empty = all files.');
  WriteLn('  * SearchSubDirs: Search files in subfolders too.');
  WriteLn('    FindAllFiles(''AList, ''' + aFolder + ''', '''', False) -> ');
  aFileList := CreateStringList;
  FindAllFiles(aFileList, aFolder, '', False);
  for i := 0 to aFileList.Count - 1 do
    WriteLn('    aFileList[' + IntToStr(i) + '] = ' + aFileList[i]);
  aFileList.Free;
  WriteLn('');


  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
