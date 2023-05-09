{
[Info]
This script searches for groups with same Media Filenames and versions with 
  same filename that groups (except the one wich owned it). 
[Data]
Name=Chixpy
Version=1.2
Date=20230509
[Changes]
1.2 - 20230501
  + A little rework.
  + Systems group list will be cleaned after on finish.
1.1 - 20230501
  + Added comparison between soft filenames.
0.01 - 20230415
  + Initial version
[EndInfo]
}
program CheckMediaFileNames;

var
  aSystem: cEmutecaSystem;
  aGroupList: cEmutecaGroupList;
  aSoftList: cEmutecaSoftList;
  aGroup1, aGroup2: cEmutecaGroup;
  aSoft1, aSoft2: cEmutecaSoftware;
  i, j: integer; 
  Abort: boolean;  
  aFile: string;
  aSL: TStringList;
  
procedure DoExit(Aborted: boolean);
begin
  if assigned(aSL) then
    aSL.Free;

  if assigned(aSystem) then
  begin
    aSystem.CacheGroups;
    aSystem.CleanGroupList;
  end;
    
  WriteLn('');
  if Aborted then
    WriteLn('Aborted!')
  else
    WriteLn('Finished!');
end;

begin
  aSystem := ETKAskSystem(Emuteca.SystemManager.FullList);
   
  if not assigned(aSystem) then
  begin
    WriteLn('No system was selected.');
    Exit;
  end;
  
  WriteLn(aSystem.Title);

  // Loading data of the system if it isn't loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);
   
  aGroupList := aSystem.GroupManager.FullList;
  aSoftList := aSystem.SoftManager.FullList;
   
  aSL := CreateStringList;
  aSL.Duplicates := dupAccept;
  aSL.CaseSensitive := False;
  
  WriteLn('');
  WriteLn('Sorting group filenames.');
  i := 0;
  while i < aGroupList.Count do
  begin
    aSL.AddObject(LowerCase(aGroupList[i].MediaFileName), aGroupList[i]);
    Inc(i);
  end;
  
  aSL.Sort;
  
  Abort := False;   
  WriteLn('Comparíng filenames of groups.');
  WriteLn('There are ' + IntToStr(aGroupList.Count) + ' groups.'); 
  i := 0;
  while (i + 1 < aSL.Count) and (not Abort) do
  begin    
    if aSL[i] = aSL[i + 1] then
    begin
      aGroup1 := cEmutecaGroup(aSL.Objects[i]);
      aGroup2 := cEmutecaGroup(aSL.Objects[i + 1]);

      WriteLn('');
      WriteLn(aSL[i] + ' <- ' + aGroup1.Title + ' | ' + aGroup2.Title);
        
      Abort := mrCancel = ETKCompareSG(aGroup1, aGroup2);
    end;    
  
    Inc(i);
  end;   
  
  if Abort then  
  begin
    DoExit(Abort);
    exit;
  end;
  
  aSL.Clear;
  aSL.Sorted := False;
  WriteLn('');
  WriteLn('Updating group filenames.');
  i := 0;
  while i < aGroupList.Count do
  begin
    aSL.AddObject(LowerCase(aGroupList[i].MediaFileName), aGroupList[i]);
    Inc(i);
  end;  
  aSL.Sorted := True;

  WriteLn('');
  WriteLn('Comparíng filenames between versions and groups.');
  WriteLn('There are ' + IntToStr(aSoftList.Count) + ' versions and ' + 
    IntToStr(aGroupList.Count) + ' groups.');
  WriteLn('Please wait.');

  i := 0;
  while (i < aSoftList.Count) and (not Abort) do
  begin
    aSoft2 := aSoftList[i];
    
    // Only compare soft that don't match it group's filename
    if not aSoft2.MatchGroupFile then
    begin
      aGroup2 := cEmutecaGroup(aSoft2.CachedGroup);
      aFile := LowerCase(aSoft2.MediaFileName);      
      
      j := aSL.IndexOf(aFile);
      if (j <> -1) then
      begin
          aGroup1 := cEmutecaGroup(aSL.Objects[j])
      
          WriteLn('');
          WriteLn(aFile + ' <- ' + aGroup1.Title +
            ' | ' + aSoft2.Title + ' {' + aGroup2.Title + '}');

          Abort := mrCancel = ETKCompareSG(aGroup1, aSoft2);          
          
          // Updating Group list filenames
          aSL.Sorted := False;
          aSL[j] := LowerCase(aGroup1.MediaFileName);
          
          j := aSL.IndexOfObject(aGroup2);
          if (j <> -1) then
            aSL[j] := LowerCase(aGroup2.MediaFileName);
  
          aSL.Sorted := True;
      end;
    end;

    Inc(i);
  end;
  
  if Abort then  
  begin
    DoExit(Abort);
    exit;
  end;
  
  aSL.Clear;
  aSL.Sorted := False;
  WriteLn('');
  WriteLn('Updating soft filenames');
  i := 0;
  while i < aSoftList.Count do
  begin
    aSL.AddObject(LowerCase(aSoftList[i].MediaFileName), aSoftList[i]);
    Inc(i);
  end;  
  aSL.Sorted := True; 
 
  WriteLn('Comparíng filenames of soft in different group.');
  WriteLn('There are ' + IntToStr(aSoftList.Count) + ' groups.'); 
  i := 0;
  while (i + 1 < aSL.Count) and (not Abort) do
  begin
      
    // Compare soft in different group
    if (aSL[i] = aSL[i + 1]) then
    begin    
      aSoft1 := cEmutecaSoftware(aSL.Objects[i]);
      aSoft2 := cEmutecaSoftware(aSL.Objects[i + 1]);
    
      if (aSoft1.CachedGroup <> aSoft2.CachedGroup) then
      begin
        WriteLn('');
        WriteLn(aSL[i] + ' <- ' + aSoft1.Title + 
          ' {' + aSoft1.CachedGroup.Title + '}' +        
          ' | ' +
          aSoft2.Title + 
          ' {' + aSoft2.CachedGroup.Title + '}');
        
        Abort := mrCancel = ETKCompareSG(aSoft1, aSoft2);
      end;
    end;    
  
    Inc(i);
  end;    
  
  DoExit(Abort);

end.
