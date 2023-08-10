{
[Info]
Script fix SortTitles:
  * Search groups and Software with articles (The, A, El, etc.) and fix them.
[Data]
Name=Chixpy
Version=0.01
Date=20230810
[Changes]
0.01 - 20230810
  + Initial version
[EndInfo]
}
program FixSortTitles;

//uses uETKStrUtils;
{$I '../Units/uETKStrUtils.pas'}

var
  aSystem: cEmutecaSystem;
  aGroupList: cEmutecaGroupList;
  aGroup: cEmutecaGroup;
  aSoftList: cEmutecaSoftList;
  aSoft: cEmutecaSoftware;
  aID, aTitle, aSortTitle: string;
  i, j: integer;

  DEBUG: Boolean;
  CHANGEGRPID: Boolean;
  KEEPSORTTITLE: Boolean;
begin
  i := AskYesNoCancel('Preview Changes', 'Do you want to only preview changes?');
  if i = mrCancel then
  begin
    WriteLn('¡Canceled!');
    Exit;
  end;
  DEBUG := TRUE;

  i := AskYesNoCancel('Group Ids', 'Change group IDs too?');
  if i = mrCancel then
  begin
    WriteLn('¡Canceled!');
    Exit;
  end;
  CHANGEGRPID := i = mrYes;

  i := AskYesNoCancel('SortTitles', 'Overwrite already changed SortTitles?');
  if i = mrCancel then
  begin
    WriteLn('¡Canceled!');
    Exit;
  end;
  KEEPSORTTITLE := i = mrNo;

  aSystem := ETKAskSystem(Emuteca.SystemManager.FullList);
   
  if not assigned(aSystem) then
  begin
    WriteLn('No system was selected.');
    Exit;
  end
  else
  begin
    WriteLn(aSystem.Title);
    WriteLn('');
  end;

  // Loading data of the system if it isn't loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);
   
  aGroupList := aSystem.GroupManager.FullList;

  WriteLn('Changing Group Titles:');
  WriteLn('');

  i := 0;
  while i < aGroupList.Count do
  begin
    aGroup := aGroupList[i];

    aTitle := aGroup.Title;
    aSortTitle := '';

    ETKFixSortTitle(aTitle, aSortTitle);

    // If we want to change Group.ID...
    aID := aGroup.ID;
    IF CHANGEGRPID THEN
      ETKFixGrpID(aTitle, aID);

    if (aSortTitle <> '') then
    begin
      IF CHANGEGRPID THEN
      BEGIN
        if aGroup.ID <> aID then
        begin
          WriteLn('   ID: ' + aGroup.ID + ' -> '  + aID);
          IF NOT DEBUG THEN
            aGroup.ID := aID;
        end;
      END;

      if aGroup.Title <> aTitle then
      begin
        WriteLn('TITLE: ' + aGroup.Title + ' -> ' + aTitle);
        IF NOT DEBUG THEN
          aGroup.Title := aTitle;
      end;

      IF (NOT KEEPSORTTITLE) OR (aGroup.GetActualSortTitle = '') THEN
      BEGIN
        if aGroup.SortTitle <> aSortTitle then
        begin
          WriteLn(' SORT: ' + aGroup.SortTitle + ' -> ' + aSortTitle);
          IF NOT DEBUG THEN
            aGroup.SortTitle := aSortTitle;
        end;
      END;

    end;
    
    Inc(i);
  end;

  WriteLn('');
  WriteLn('');
  WriteLn('Changing Software Titles:');
  WriteLn('');

  aSoftList := aSystem.SoftManager.FullList;

  j := 0;
  while j < aSoftList.Count do
  begin
    aSoft := aSoftList[j];

    aTitle := aSoft.GetActualTitle;
    aSortTitle := '';

    if aTitle <> '' then // Only search non inherited titles
    begin

      ETKFixSortTitle(aTitle, aSortTitle);

      if (aSortTitle <> '') then
      begin
        if aSoft.Title <> aTitle then
        begin
          WriteLn('TITLE: ' + aSoft.Title + ' -> ' + aTitle)
          IF NOT DEBUG THEN
            aSoft.Title := aTitle;
        end;

        IF (NOT KEEPSORTTITLE) OR (aSoft.GetActualSortTitle = '') THEN
        BEGIN
          if aSoft.SortTitle <> aSortTitle then
          begin
            WriteLn(' SORT: ' + aSoft.SortTitle + ' -> ' + aSortTitle)
            IF NOT DEBUG THEN
              aSoft.SortTitle := aSortTitle;
          end;
        END;
      end;
    end;

    Inc(j);
  end;
  
  WriteLn('');
  WriteLn('Finished!');
end.
