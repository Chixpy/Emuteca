{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20110121
Description =
~begin~
  Extract game info from history.dat (from MAME or www.quickplayfrontend.com)
~end~
Changes =
~begin~
  0.1 - 20111206:
    f Fixed error with '"'.
  0.1 - 20111206:
    + Initial version.
~end~
[END]
}
program SplitHistoryDat;

var
  FileName: String;
  OutputFolder: String;
  DatFile, CurrFileText: TStringList;
  CurrFile, CurrLine: String;
  i: Integer;
begin
  FileName := AskFile('Select QuickPlay ini',
    'QuickPlay ini (*.ini)|*.ini', '');
  if (FileName = '') or (not FileExistsUTF8(FileName)) then
    Exit;
    
  OutputFolder := AskFolder('Select output folder', '');
  if (OutputFolder = '') or (not DirectoryExistsUTF8(OutputFolder)) then
    Exit;
    
  DatFile := TStringList.Create;
  CurrFileText := TStringList.Create;
  try
    DatFile.LoadFromFile(FileName);
    CurrFile := '';
    CurrFileText.Clear;
    for i := 0 to DatFile.Count - 1 do
    begin
      CurrLine := DatFile[i];
      
      if Pos('[', CurrLine) = 1 then
      begin
        if (CurrFile <> '') and (CurrFileText.Count <> 0) then
        begin
          CurrFile := CleanFileName(CurrFile);
          WriteLn('Saving: ' +CurrFile);
          CurrFile := AnsiReplaceText(CurrFile, '/', ' - ');
          CurrFileText.SaveToFile(UTF8ToSys(OutputFolder + CurrFile + '.txt'));
        end;
        CurrFileText.Clear;
        CurrFile := Copy(CurrLine, 2, Pos(']', CurrLine) - 2);
      end
      else if Pos('Description=', CurrLine) = 1 then
      begin
        CurrLine := Copy(CurrLine, 13, 4194303);
        CurrLine := AnsiReplaceText(CurrLine, '"', '""');
        CurrLine := AnsiReplaceText(CurrLine, '<br>', '","');
        CurrLine := AnsiReplaceText(CurrLine, '<br />', '","');
        CurrLine := AnsiReplaceText(CurrLine, '<br/>', '","');
        CurrFileText.CommaText := '"' + CurrLine + '"';
      end;
    end;
  finally
    DatFile.Free;
  end;
  
end.
