{ Emuteca Script
[Info]
This script moves ALL files to subfolders with its name, but
  removing parentesis in folder's name.
  
Useful for media files of "Ads", "Reviews" and "Other" folders, as the info in parentesis
  can be interesting. 
  
Don't confuse with RemoveParentesis.pas wich can be useful for other media folders.

For example: GameName (Magazine XX - Section YY).ext
 * It will renamed to: GameName/GameName (Magazine XX - Section YY).ext

[Data]
Name=Chixpy
Version=1.00
Date=20200909

[Changes]
* 1.00 - 20200909
  * Initial version

[EndInfo]
}
program RemoveParentesis;

//uses uETKFileUtils;
{$I '../Units/uETKFileUtils.pas'}

var
  aFolder, aFile, aExt, outFile: string;
  AFileList: TStringList;
  i, j: integer;  
  
begin
  aFolder := AskFolder('Select folder', '');
  if aFolder = '' then
  begin
    WriteLn('');
    WriteLn('');
    WriteLn('CANCELLED');
    WriteLn('---------');
    Exit;
  end;

  aFile := ReadLn('Write FileMask (Empty = All files)', '');

  // TODO: Preguntar si se esta seguro realizar la operación

  AFileList := CreateStringList;
  FindAllFiles(AFileList, aFolder, aFile, False);

  i := 0;
  while i < AFileList.Count do
  begin
    aFolder := ExtractFilePath(AFileList[i]);
    aFile := RemoveFromBrackets(ExtractFilenameOnly(AFileList[i]));
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
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
