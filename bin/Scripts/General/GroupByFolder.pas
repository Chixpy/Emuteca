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
  Game: cGame;
  Group: cGameGroup;
  i: Integer;
  GroupName: String;
  Compressed: Boolean;
begin
  WriteLn('Changing game groups:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i);

    GroupName := ExcludeTrailingPathDelimiter(Game.Folder);
    Compressed := FileExistsUTF8(GroupName);

    GroupName := ExtractFileName(GroupName);

    if Compressed then
    begin // begin..end needed
      if Game.GameGroup <> ChangeFileExt(GroupName, '') then
      begin
        Game.GameGroup := ChangeFileExt(GroupName, '');
      end;
    end
    else
      if Game.GameGroup <> GroupName then
      begin
        Game.GameGroup := GroupName;
      end;

    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;

  GameManager.UpdateGroupList;
end.

