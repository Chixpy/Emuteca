{ [SCRIPTDATA]
Author = Chixpy
Version = 0.2
Date = 20110401
Description =
~begin~
  Changes the game group based in the folder (or zip) where the file is in.
  ~end~
Changes =
~begin~
  0.2 - 20110401
    m More verbose but show only changed
  0.1 - 20110102:
    + Initial version
~end~
[END]
}
program GroupByFolder;

var
  aGame: cGame;
  aGroup: cGameGroup;
  i: Integer;
  aGroupName: String;
  Compressed: Boolean;
begin
  WriteLn('Changing game groups:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    aGame := GameManager.GameAtPos(i);

    aGroupName := ExcludeTrailingPathDelimiter(aGame.Folder);
    Compressed := FileExistsUTF8(aGroupName);

    aGroupName := ExtractFileName(aGroupName);

    if Compressed then
    begin // begin..end needed
      if aGame.GameGroup <> ChangeFileExt(aGroupName, '') then
      begin
        aGame.GameGroup := ChangeFileExt(aGroupName, '');
      end;
    end
    else
      if aGame.GameGroup <> aGroupName then
      begin
        aGame.GameGroup := aGroupName;
      end;

    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;

  GameManager.UpdateGroupList;
end.

