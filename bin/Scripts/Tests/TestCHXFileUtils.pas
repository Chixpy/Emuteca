{
[Info]
This script test some basic functions added to Pascal Script from uCHXFileUtils.
[Data]
Name=Chixpy
Version=0.01
Date=20171014
[Changes]

[EndInfo]
}
program TestCHXFileUtils;
var
  aFolder, aFile: string;
begin
  WriteLn('Hello!');
  WriteLn('');
  WriteLn('This Script will test added functions to Pascal Script from');
  WriteLn('  uCHXFileUtils.');
  WriteLn('');
  
  aFolder := AskFolder('Select a random folder', '');
  aFile := AskFile('Select random file', '', aFolder);

  WriteLn('function CRC32FileInt(const aFileName: string): cardinal');
  WriteLn('function CRC32FileStr(const aFileName: string): string');
  WriteLn('    Gets CRC32 checksum of a file.');
  WriteLn('    CRC32FileStr('''+ aFile + ''') -> ' +
    CRC32FileStr(aFile));
  WriteLn('');

  WriteLn('function SHA1FileStr(const aFileName: string): string');
  WriteLn('    Gets SHA1 checksum of a file.');
  WriteLn('    SHA1FileStr('''+ aFile + ''') -> ' +
    SHA1FileStr(aFile));
  WriteLn('');

  WriteLn('function FilesInFolder(aFolder, aFileMask: string): integer'); 
  WriteLn('    Returns the number of files in a folder.')
  WriteLn('    FilesInFolder(''' + aFolder + ''','''') -> ' +
    IntToStr(FilesInFolder(aFolder,'')));
    
  WriteLn('');
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
