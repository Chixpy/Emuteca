{
[Info]
This script removes parentesis from filenames in a folder.

Useful for media files named with NoIntro, GoodXXX, TOSEC scheme.

For "Ads", "Maps", "Reviews" and "Other" folders, it's better use "AllMediaSubfolders.pas", as the info in parentesis can be interesting.

For example: GameName (USA).ext
 * If the subfolder GameName exists, it will be moved to GameName\GameName (USA).ext
 * If GameName.ext exists, they both will be moved to GameName\[Filename.ext]
 * If not GameName.ext, it will be renamed to it.

[Data]
Name=Chixpy
Version=1.03
Date=20240119

[Changes]
* 1.03 - 20240119
  f Showing in console script name, foled and filemask
* 1.02 - 20230108
  f Folders problems with folders ending with a dot '.'
* 1.01 - 20200201
  + Cancel, if ask folder is cancelled
* 1.00 - 20200126
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
  WriteLn('RemoveParentesis.pas');
  WriteLn('--------------------');
  WriteLn('');

  aFolder := AskFolder('Select folder', '');
  if aFolder = '' then
  begin
    WriteLn('CANCELLED');
    WriteLn('---------');
    Exit;
  end;

  WriteLn('Moving files from: ' + aFolder);
  WriteLn('');

  aFile := ReadLn('Write FileMask (Empty = All files)', '');

  // TODO: Preguntar si se esta seguro realizar la operación

  AFileList := CreateStringList;
  FindAllFiles(AFileList, aFolder, aFile, False);

  i := 0;
  while i < AFileList.Count do
  begin
   aFolder := ExtractFilePath(AFileList[i]);
   aFile := RemoveFromBrackets(ExtractFilenameOnly(AFileList[i]));
   // Windows bug fix: Folder names ended in a dot '.' cause some problems
   //   creating and deleting them. Emuteca actually change them in SortName
   //   automatically.
   if aFile[Length(aFile)] = '.' then
     aFile[Length(aFile)] := '_';
   aExt := ExtractFileExt(AFileList[i]);

   // WriteLn(aFolder);
   // WriteLn(aFile);
   // WriteLn(aExt);

   if (aFile <> '') and FileExists(AFileList[i]) then
   begin
     // Subdir exists
     if DirectoryExists(aFolder + aFile) then
     begin
       outFile := ETKCheckRenameFile(SetAsFolder(aFolder + aFile) + ExtractFilename(AFileList[i]));
       
       WriteLn(AFileList[i] + ' -> ' + outFile);
       RenameFile(AFileList[i], outFile);
     end
     else if CompareFileNames(SetAsFile(AFileList[i]), SetAsFile(aFolder + aFile + aExt)) <> 0 then
     begin
       outFile := aFolder + aFile + aExt;
       if FileExists(outFile) then
       begin
         ForceDirectories(aFolder + aFile);
         outFile := ETKCheckRenameFile(SetAsFolder(aFolder + aFile) + aFile + aExt);         
         WriteLn(aFolder + aFile + aExt + ' -> ' + outFile);
         RenameFile(aFolder + aFile + aExt, outFile);
         
         outFile := ETKCheckRenameFile(SetAsFolder(aFolder + aFile) + ExtractFilename(AFileList[i]));
         WriteLn(AFileList[i] + ' -> ' + outFile);         
         RenameFile(AFileList[i], outFile);
       end
       else
       begin
         WriteLn(AFileList[i] + ' -> ' + outFile);
         RenameFile(AFileList[i], outFile);
       end;
     end;
   end;

   Inc(i);
  end;

  AFileList.Free;
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');
  WriteLn('');

end.
