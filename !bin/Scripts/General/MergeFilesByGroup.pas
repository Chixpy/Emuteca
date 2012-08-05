{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20111203
Description =
~begin~
  Merges all games in the same group in the same 7z archive.
  
  NOTE 1: All files must be decompressed before this operation.

  NOTE 2: Only files recognised by Emuteca will be compressed. So, multifile
    games will not be merge correctly (only main files will be compressed).

  NOTE 3: You must delete source files, change with new compressed archives,
    and update the game list.
~end~
Changes =
~begin~
  0.1 - 20111203:
    + Initial version
~end~
[END]
}
program MergeFilesByGroup;
var
  i, j, k: Integer;
  OutputFolder: String;
  OutputFile: String;
  CurrGroup: cGameGroup;
  CurrGame: cGame;
  ExitCode: Integer;
  FileList: TStringList;
begin
  OutputFolder := AskFolder('Select folder for compressed archives:','');
  if (OutputFolder = '') or (not DirectoryExistsUTF8(OutputFolder)) then
    Exit;

  WriteLn('Checking if all files are decompressed.')
  j := 0; // Counting compressed files...
  i := 0;
  while i < GameManager.GameCount do
  begin
    CurrGame := GameManager.GameAtPos(j);
    if FileExistsUTF8(ExcludeTrailingPathDelimiter(CurrGame.Folder)) then
    begin
      WriteLn(CurrGame.Folder + 'archive has ' + CurrGame.FileName);
      Inc(j);
    end;
    Inc(i);
  end;

  if j <> 0 then
  begin
    WriteLn(IntToStr(j) + ' files are in compressed archives.');
    WriteLn('Stopping.');
    Exit;
  end;
  WriteLn('OK.');

  FileList := TStringList.Create;
  try    
    i := 0;
    while i < GameManager.GroupCount do
    begin
      CurrGroup := GameManager.GroupAtPos(i);
      OutputFile := ChangeFileExt(CurrGroup.MediaFileName, '.7z');
      WriteLn(OutputFolder + OutputFile);

      if not FileExistsUTF8(OutputFolder + OutputFile) then
      begin
        FileList.Clear;
        j := 0
        while j < GameManager.GameCount do
        begin
          if UTF8LowerCase(GameManager.GameAtPos(j).GameGroup) = CurrGroup.Key then
          begin
            FileList.Add(GameManager.GameAtPos(j).Folder +
              GameManager.GameAtPos(j).FileName)
          end;      
          Inc(j);
        end;
        FileList.SaveToFile(GameManager.TempFolder + GameManager.TempFile);
        FileList.Clear;
        FileList.Add('@' + GameManager.TempFolder + GameManager.TempFile);

        ExitCode := Compress7zFile(OutputFolder + OutputFile, FileList, True, '');
        if ExitCode <> 0 then
        begin
          WriteLn('ERROR: 7z.exe exits with ' + IntToStr(ExitCode) + ' exitcode.');
          FileList.LoadFromFile(GameManager.TempFolder + GameManager.TempFile);
          for j := 0 to FileList.Count - 1 do
            WriteLn('--> ' + FileList[j]);
        end;
      end
      else
      begin
        WriteLn('ERROR: ' + OutputFolder + OutputFile + ' already exists.');
      end;
      Inc(i);
    end;
  finally
    // TODO 1: MEMORY LEAK!!!
    FileList.Free;
  end;
end.
