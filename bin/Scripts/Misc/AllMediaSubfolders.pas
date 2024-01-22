{ Emuteca Script
[Info]
This script moves ALL files to subfolders with its name, but removing
  parentesis in folder's name.
  
Useful for media files of "Ads", "Reviews", "Maps" and "Other" folders,
  as the info in parentesis can be useful and interesting.
  
Don't confuse with RemoveParentesis.pas wich can be useful for other
  media folders.

For example: GameName (Magazine XX - Section YY).ext
 * It will renamed to: GameName/GameName (Magazine XX - Section YY).ext

[Data]
Name=Chixpy
Version=1.02
Date=20240119

[Changes]
* 1.02 - 20240119
  * Showing in console script name, folder and the filemask used.
* 1.01 - 20230108
  f Folders problems with folders ending with a dot '.'
* 1.00 - 20200909
  * Initial version.

[EndInfo]
}
program AllMediaSubfolders;

//uses uETKFileUtils;
{$I '../Units/uETKFileUtils.pas'}

var
  aFolder, aFile, aExt, outFile: string;
  AFileList: TStringList;
  i, j: integer;  
  
begin
  WriteLn('AllMediaSubfolders.pas');
  WriteLn('----------------------');
  WriteLn('');

  aFolder := AskFolder('Select folder', '');
  if aFolder = '' then
  begin
    WriteLn('CANCELLED');
    WriteLn('---------');
    WriteLn('');
    Exit;
  end;

  WriteLn('Moving files from: ' + aFolder);

  aFile := ReadLn('Write FileMask (Empty = All files)', '');

  WriteLn('FileMask: ' + aFile);
  WriteLn('');

  // TODO: Preguntar si se esta seguro realizar la operación

  AFileList := CreateStringList;
  FindAllFiles(AFileList, aFolder, aFile, False);

  i := 0;
  while i < AFileList.Count do
  begin
    aFolder := ExtractFilePath(AFileList[i]);
    aFile := RemoveFromBrackets(ExtractFilenameOnly(AFileList[i]));
    // Windows BUG FIX: Folder names ended in a dot '.' cause some problems
    //   creating and deleting them. Emuteca actually change them in SortName
    //   automatically.
    if aFile[Length(aFile)] = '.' then
      aFile[Length(aFile)] := '_';
    aExt := ExtractFileExt(AFileList[i]);

    // WriteLn(aFolder);
    // WriteLn(aFile);
    // WriteLn(aExt);

    // Subdir no exists
    if not DirectoryExists(aFolder + aFile) then
      ForceDirectories(aFolder + aFile);
      
    outFile := ETKCheckRenameFile(SetAsFolder(aFolder + aFile) + ExtractFilename(AFileList[i]));
       
    WriteLn(AFileList[i] + ' -> ' + outFile);
    RenameFile(AFileList[i], outFile);

    Inc(i);
  end;

  AFileList.Free;  

  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');
  WriteLn('');

end.
