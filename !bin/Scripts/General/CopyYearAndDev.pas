{ [SCRIPTDATA]
Author = Chixpy
Version = 1
Date = 20110121
Description =
~begin~
  Extract group dev and year from older game release.
~end~
Changes=
~begin~
~end~
}

program CopyYearAndDev;
var
  i: integer;
  Game: cGame;
  Group: cGameGroup;
begin
  while i < GameManager.GameCount do
  begin
    Game := GameManager.GameAtPos(i)
    Group := GameManager.Group(Game.GameGroup);

    if Group.Developer = '' then
      Group.Developer := Game.Publisher;

    if Group.Year = '' then
    begin
      Group.Year := Game.Year;
    end
    else
    begin
      if (Game.Year <> '') and (Group.Year >= Game.Year) then
      begin
        Group.Year := Game.Year;
        if Game.Publisher <> '' then
          Group.Developer := Game.Publisher;
      end;
    end;


    Inc(i);
    if i mod 250 = 0 then
      WriteLn(IntToStr(i) + ' games parsed.');
  end;
end.
