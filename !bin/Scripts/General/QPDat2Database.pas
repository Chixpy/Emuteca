{ [SCRIPTDATA]
Author = Chixpy
Version =
Date =
Description =
~begin~
  Creates a Emuteca's .edb from Quickplay's .dat file.
~end~
Changes =
~begin~
  0.1 - 20101109:
    + Initial version
~end~
[END]
}
program GroupByFolder;
const
  Debug = False;

var
  aGame: cGame;
  aGroup: cGameGroup;
  DatFileName, DBFileName, aString: String;
  DatFile, CurrLine, DBFile: TStringList;
  i: integer;

begin
  DatFileName := AskFile('Select a QuickPlay .dat file',
    'QuickPlay .dat file (*.dat)|*.dat', '');
  if not FileExistsUTF8(DatFileName) then Exit;
  WriteLn('Input: ' + DatFileName);

  DBFileName := AskFile('Select a Emuteca .edb file',
    'Emuteca .edb file (*.edb)|*.edb', ExtractFilePath(DatFileName) + 'QuickPlay.edb');
  if DBFileName = '' then Exit;
  WriteLn('Output: ' + DBFileName);

  DatFile := TStringList.Create;
  CurrLine := TStringList.Create;
  DBFile := TStringList.Create;
  try
    DatFile.LoadFromFile(DatFileName);

    if (DatFile.Count > 0) and
      (DatFile[0] = 'ROM DataFile Version : 1.1') then
    begin
      for i := 1 to DatFile.Count - 1 do
      begin
        CurrLine.Clear;
        // Separated with ANSI ¬ = Chr($00AC)
        CurrLine.CommaText := '"' + AnsiReplaceText(AnsiReplaceText(
          DatFile[i], '"', '""'), Chr($00AC), '","') + '"';

        { Slow way... using cGame and cGameGroup Export...

        aGame := cGame.Create('', CurrLine[0] + '.xyz', CurrLine[1]);
        aGame.GameGroup := CurrLine[2];
        aGame.Publisher := CurrLine[6];
        aGame.Year := CurrLine[7];

        aString := Trim(CurrLine[8]);
        if aString <> '' then
          aGame.Tags.Add('Genre/' + aString);

        aString := Trim(CurrLine[10])
        if aString <> '' then
          aGame.Languages.Add(aString);

        aString := Trim(CurrLine[17]);
        if aString <> '' then
          aGame.Tags.Add('Players/' + aString);

        aGame.ExportData(DBFileName, true);
        aGame.Free

        // Si CurrLine[2] = '' entonces es un grupo (y hay que guardarlo)
        if CurrLine[2] = '' then
        begin
          aGroup := cGameGroup.Create(CurrLine[0]);
          aGroup.Key := CurrLine[1];
          aGroup.Developer := CurrLine[6];
          aGroup.Year := CurrLine[7];
          aGroup.ExportData(DBFileName, True);
          aGroup.Free
        end;
        }

        // Faster way but...
        DBFile.Add('[' + CurrLine[1] + ']');
        DBFile.Add('Name=' + CurrLine[0]);
        DBFile.Add('SortName=' + CurrLine[0]);
        if CurrLine[2] <> '' then
          DBFile.Add('GameGroup=' + CurrLine[2])
        else
          DBFile.Add('GameGroup=' + CurrLine[1]);
        DBFile.Add('Publisher=' + CurrLine[6]);
        DBFile.Add('Year=' + CurrLine[7]);
        DBFile.Add('Languages=' + CurrLine[10]);

        if CurrLine[2] = '' then
        begin
          DBFile.Add('');
          DBFile.Add('[Group: ' + UTF8LowerCase(CurrLine[1]) + ']');
          DBFile.Add('Name=' + CurrLine[0]);
          DBFile.Add('SortName=' + CurrLine[0]);
          DBFile.Add('Developer=' + CurrLine[6]);
          DBFile.Add('Year=' + CurrLine[7]);
          if Trim(CurrLine[8]) <> '' then
            aString := '"Genre/' + Trim(CurrLine[8]) + '"';
          if Trim(CurrLine[17]) <> '' then
          begin
            if aString <> '' then
              aString := aString + ',';
            aString := aString + '"Players/' + Trim(CurrLine[17]) + '"';
          end;
          if aString <> '' then
            DBFile.Add('Tags=' + aString);
          DBFile.Add('MediaFileName=' + CurrLine[1] + '.(group)');
        end;
        DBFile.Add('');

        if i mod 100 = 0 then
          WriteLn(IntToStr(i) + ' games of ' + IntToStr(DatFile.Count))
      end;
    end
    else
      WriteLn(DatFileName + 'seems not to be a Quickplay .dat file v1.1');
  finally
    DBFile.SaveToFile(UTF8ToSys(DBFileName));
    DBFile.Free;
    CurrLine.Free;
    DatFile.Free;
  end;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
