{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20110121
Description =
~begin~
  TODO 1: Freeing a TStringList make memory leak!
  Tries to merge roms as GoodMerge but with
~end~
Changes =
~begin~
  0.1 - 20111127:
    + Initial version
~end~
}
program TOSECFilenames;
var
  aGame: cGame;
  aSearch, aGroupName: String;
  aMergeFile: TStringList;
  i, j: integer;
begin
  aSearch := AskFile('TOSEC Merge file', 'TOSECMerge dats|*.tmd', '');
  if not FileExistsUTF8(aSearch) then
  begin
    WriteLn(aSearch + ' don''t exists.');
    Exit;
  end;

  aMergeFile := TStringList.Create;
  aMergeFile.LoadFromFile(aSearch);
  try
    WriteLn('Cleaning merge file.');

    // BOM of UTF8: #$ef#$bb#$bf
    if aMergeFile.Count > 0 then
      aMergeFile[0] := AnsiReplaceText(aMergeFile[0], #$ef#$bb#$bf, '');

    i := aMergeFile.Count - 1;
    while i >= 0 do
    begin
      j := Pos('#', aMergeFile[i]);
      if j <> 0 then
        aMergeFile[i] := Copy(aMergeFile[i], 1, j - 1);

      aMergeFile[i] := Trim(aMergeFile[i]);

      if Trim(aMergeFile[i]) = '' then
        aMergeFile.Delete(i)
      else
      begin
        if Pos('==>', aMergeFile[i]) = 0 then
        begin
          WriteLn('Wrong rule: "' + aMergeFile[i] + '"');
          aMergeFile.Delete(i);
        end;
      end;
      Dec(i);
    end;

    WriteLn('Merging groups:');
    i := aMergeFile.Count - 1;
    while i >= 0  do
    begin
      j := Pos('==>', aMergeFile[i]);

      aSearch := Copy(aMergeFile[i], 1, j - 1);
      aGroupName := Copy(aMergeFile[i], j + 3, 512);

      j := Pos('*', aSearch);
      if (j = 1) then
      begin // Search in the middle of the name
        aSearch := Copy(aSearch, 2, 512);

        j := GameManager.GameCount - 1;
        while j >= 0 do
        begin
          aGame := GameManager.GameAtPos(j);
          if Pos(aSearch,
            ExtractFileNameOnly(aGame.Filename)) <> 0 then
          begin
            aGame.GameGroup := aGroupName;
            WriteLn(aGroupName + ' <- ' + aGame.FileName);
          end;
          Dec(j);
        end;
      end
      else
      begin // The game begins with...
        j := GameManager.GameCount - 1;
        while j >= 0 do
        begin
          aGame := GameManager.GameAtPos(j);
          if UTF8CompareText(aSearch, Copy(ExtractFileNameOnly(
            aGame.Filename), 1, Length(aSearch))) = 0 then
          begin
            aGame.GameGroup := aGroupName;
            WriteLn(aGroupName + ' <- ' + aGame.FileName);
          end;
          Dec(j);
        end;
      end;
    Dec(i);
  end;
  finally
    // TODO 1: ¿¿¡¡Memory LEAK !!??
    aMergeFile.Free;
  end;

  GameManager.UpdateGroupList;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
