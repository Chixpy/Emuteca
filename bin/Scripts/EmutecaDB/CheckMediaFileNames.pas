{
[Info]
This script searches for groups with same Media Filenames and versions with 
  same filename that groups (except the one wich owned it). 
[Data]
Name=Chixpy
Version=1.2
Date=20230501
[Changes]
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
  i, j: integer; 
  Abort: boolean;  
  aFile: string;
  aSL: TStringList;
begin
  aSystem := ETKAskSystem(Emuteca.SystemManager.FullList);
   
  if not assigned(aSystem) then
  begin
    WriteLn('No system was selected.');
  end
  else
  begin
    WriteLn(aSystem.Title);
  end;

  // Loading data of the system if it isn't loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);
   
  aGroupList := aSystem.GroupManager.FullList;
  aSoftList := aSystem.SoftManager.FullList;
   
  aSL := CreateStringList;
  aSL.Duplicates := dupAccept;
  aSL.CaseSensitive := False;
  
  WriteLn('');
  WriteLn('Sorting group filenames');
  i := 0;
  while i < aGroupList.Count do
  begin
    aSL.AddObject(LowerCase(aGroupList[i].MediaFileName), aGroupList[i]);
    Inc(i);
  end;
  
  aSL.Sort;
  
  Abort := False;   
  WriteLn('Comparíng filenames of groups');
  WriteLn('There are ' + IntToStr(aGroupList.Count) + ' groups.'); 
  i := 0;
  while (i + 1 < aSL.Count) and (not Abort) do
  begin
    if aSL[i] = aSL[i + 1] then
    begin
        WriteLn('');
        WriteLn(aSL[i] + ' <- ' + cEmutecaGroup(aSL.Objects[i]).Title + ' | ' +
          cEmutecaGroup(aSL.Objects[i + 1]).Title);
        WriteLn('');
        
        Abort := mrCancel = ETKCompareSG(cEmutecaGroup(aSL.Objects[i]),
          cEmutecaGroup(aSL.Objects[i + 1]));
    end;    
  
    Inc(i);
  end;   
  
  aSL.Clear;
  aSL.Sorted := False;
  WriteLn('');
  WriteLn('Updating group filenames');
  i := 0;
  while i < aGroupList.Count do
  begin
    aSL.AddObject(LowerCase(aGroupList[i].MediaFileName), aGroupList[i]);
    Inc(i);
  end;  
  aSL.Sorted := True;

  WriteLn('');
  if Abort then  
  begin
    aSL.Free;
    WriteLn('Aborted!');
    exit;
  end;

  WriteLn('Comparíng filenames between versions and groups.');
  WriteLn('There are ' + IntToStr(aSoftList.Count) + ' versions and ' + 
    IntToStr(aGroupList.Count) + ' groups.');
  WriteLn('Please wait.');

  i := 0;
  while (i < aSoftList.Count) and (not Abort) do
  begin
    // Only compare soft that don't match it group's filename
    if not aSoftList[i].MatchGroupFile then
    begin
      aFile := LowerCase(aSoftList[i].MediaFileName);      
      
      j := aSL.IndexOf(aFile);
      if (j <> -1) then
      begin
          WriteLn('');
          WriteLn(aFile + ' <- ' + cEmutecaGroup(aSL.Objects[j]).Title +
            ' | ' + aSoftList[i].Title + ' {' +
            aSoftList[i].CachedGroup.Title + '}');
          WriteLn('');

          Abort := mrCancel = ETKCompareSG(cEmutecaGroup(aSL.Objects[j]), 
            aSoftList[i]);

          // Updating Group list
          aSL.Sorted := False;
          aSL[j] := LowerCase(cEmutecaGroup(aSL.Objects[j]).MediaFileName);
          aSL.Sorted := True;
      end;
    end;

    Inc(i);
  end;
  
  if Abort then  
  begin
    aSL.Free;
    WriteLn('Aborted!');
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
    if (aSL[i] = aSL[i + 1]) and 
      (cEmutecaSoftware(aSL.Objects[i]).CachedGroup <> cEmutecaSoftware(aSL.Objects[i + 1]).CachedGroup)
      then
    begin    
        WriteLn('');
        WriteLn(aSL[i] + ' <- ' + cEmutecaSoftware(aSL.Objects[i]).Title + 
          ' {' + cEmutecaSoftware(aSL.Objects[i]).CachedGroup.Title + '}' +        
          + ' | ' +
          cEmutecaSoftware(aSL.Objects[i + 1]).Title + 
          ' {' + cEmutecaSoftware(aSL.Objects[i + 1]).CachedGroup.Title + '}');
        WriteLn('');
        
        Abort := mrCancel = ETKCompareSG(cEmutecaSoftware(aSL.Objects[i]),
          cEmutecaSoftware(aSL.Objects[i + 1]));
    end;    
  
    Inc(i);
  end;    
  
  aSL.Free; 

  if Abort then
    WriteLn('Aborted!')
  else
    WriteLn('Finished!');
end.
