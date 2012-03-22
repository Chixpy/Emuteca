{ [SCRIPTDATA]
Author = Chixpy
Version = 0.2
Date = 20110420
Description =
~begin~
  Creates a file with MAME data, which it can be imported in Emuteca.

  1. mame -listxml > afile.txt

~end~
Changes =
~begin~
  0.1 - 20110420:
    + Initial version
~end~
}
program GroupByFolder;
var
  aPos: Integer;
  aString: String;
  Game: cGame;
  Group: cGameGroup;
  aFileName: String;
  aFile: TStringList;
  i: Integer;
begin
  aFileName := AskFile('File with clones data (mame -listclones > clones.txt)',
    'All files (*.*)|*.*', '');

  if not FileExistsUTF8(aFilename) then
  begin
    WriteLn('The file "' + aFilename + '" not found.');
    Exit;
  end;
  WriteLn('Reading: ' + aFileName);
  WriteLn('It can be long...');

  aFile := TStringList.Create;
  try
    aFile.LoadFromFile(aFileName);
    i := 0;
    while i < aFile.GameCount do
    begin
      aPos := Pos('<game name="', aFile[i]);
      if aPos <> 0 then
      begin
        aString := aFile[i];
        aPos := Pos('"', aString);
        aString := Copy(aString, aPos + 1, 512)

        aPos := Pos('"', aString);
        Game := cGame.Create('', '', Copy(aString, 1, aPos));
        aString := copy(aString, aPos + 1, 512)


        continuar....

      end;

      Inc(i);
    end;
  finally
    // TODO 1: ¿¡Memory leak!?
    aFile.Free
  end;
end.
