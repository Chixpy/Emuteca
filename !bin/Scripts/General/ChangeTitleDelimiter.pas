{
[SCRIPTDATA]
Author = Chixpy
Version = 0.1
Date = 20101119
Description =
~begin~
  Changes the game group based in the folder (or zip) where the file is in.
~end~
Changes =
~begin~
  0.2 - 20111116
    m Adapting to changes in cGame and cGameGroup
  0.1 - 20101109:
    + Initial version
~end~
[END]
}
program ChangeTitleDelimiter;
function ChangeDelimiter(aTitle: String): String;
begin
  Result := AnsiReplaceText(aTitle, ' - ', ': ');
  //WriteLn(aTitle + ' --> ' + Result);
end;

var
  Game: cGame;
  Group: cGameGroup;
  i: integer;
begin
  WriteLn('Changing title delimiter of games:');
  i := 0;
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i);
    Game.Name := ChangeDelimiter(Game.Name);
    Game.SortKey := ChangeDelimiter(Game.SortKey);
    Game.GameGroup := ChangeDelimiter(Game.GameGroup);
    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;

  WriteLn('');
  WriteLn('Changing title delimiter of groups:');
  i := 0;
  while i < GameManager.GroupCount do
  begin
    Group := GameManager.GroupAtPos(i);
    Group.Name := ChangeDelimiter(Group.Name);
    Group.Key := ChangeDelimiter(Group.Key);
    Group.SortKey := ChangeDelimiter(Group.SortKey);
    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' groups parsed.');
  end;

  WriteLn('');
  WriteLn('---------------');
  WriteLn('');
  WriteLn('Done.');
end.
