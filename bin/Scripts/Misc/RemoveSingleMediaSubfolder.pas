{
[Info]
This script removes media subfolder with single file, and moves the file to parent
folder changing its filename with folder name. Removes emty folders too;

For example:

If "BaseFolder\GameName\" has only "RandonName.ext"
1. "BaseFolder\GameName\RandomName.ext" will be moved to "BaseFolder\GameName.ext"
2. And folder "BaseFolder\GameName\" will be removed.

If "BaseFolder\GameName.ext" already exists, reverse operation is performed;
"BaseFolder\GameName.ext" will be moved to "BaseFolder\GameName\"
(without overwriting if already exists).

[Data]
Name=Chixpy
Version=1.00
Date=2020XXXX
[Changes]
* 1.00
  * Initial version

[EndInfo]
}
program RemoveSingleMediaSubfolder;

//uses uETKFileUtils;
{$I '../Units/uETKFileUtils.pas'}

var
  aFolder, aExt, outFile: string;
  aFolderList, aFileList: TStringList;
  i, nFiles: integer;
  
begin
  aFolder := AskFolder('Select base folder', '');
  if aFolder = '' then
  begin
    WriteLn('');
    WriteLn('');
    WriteLn('CANCELLED');
    WriteLn('---------');
    Exit;
  end;

  aFolderList := CreateStringList;
  aFileList := CreateStringList;  
  FindAllDirectories(AFolderList, aFolder, False);

  // TODO: Preguntar si se esta seguro realizar la operación
  
  i := 0;
  while i < AFolderList.Count do
  begin
    nFiles := FilesInFolder(AFolderList[i], '');
    if nFiles = 1 then
    begin
      aFileList.Clear;
      FindAllFiles(aFileList, AFolderList[i], '', False);

      // We are sure that there is 1 file, so its in aFileList[0];
      aExt := ExtractFileExt(AFileList[0]);

      outFile := SetAsFolder(aFolder) +  ExtractFilename(AFolderList[i]) + aExt;

      if FileExists(outFile) then
      begin
        // File already exists, so we move it to the folder
        RenameFile(outFile, ETKCheckRenameFile(SetAsFolder(AFolderList[i]) + ExtractFilename(outFile)));
        WriteLn(outFile + ' -> ' + ETKCheckRenameFile(SetAsFolder(AFolderList[i]) + ExtractFilename(outFile)));
      end
      else
      begin
        // Moving to base folder and removing the initial subfolder
        RenameFile(AFileList[0], outFile);
        RemoveDir(AFolderList[i]);
        WriteLn(AFileList[0] + ' -> ' + outFile);
        WriteLn(AFolderList[i] + ' deleted.');
      end;
    end
    else if nFiles = 0 then  // Subfolder is empty, removing it
    begin
        RemoveDir(AFolderList[i]);
        WriteLn(AFolderList[i] + ' deleted.');
    end;

  
    Inc(i);
  end;

  aFileList.Free;  
  aFolderList.Free;  
  WriteLn('');
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');

end.
