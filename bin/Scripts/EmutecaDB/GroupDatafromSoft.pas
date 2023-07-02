{
[Info]
If a group has the developer or date empty, this script will copy it from
  its children software. Developer will be copied only if all software
  have the same value, and date will be copied the lowest one.  
[Data]
Name=Chixpy
Version=0.01
Date=20230420
[Changes]
0.01 - 20230420
  + Initial version
[EndInfo]
}
program CheckMediaFileNames;

var
  aSystem: cEmutecaSystem;
  aGroupList: cEmutecaGroupList;
  aGroup: cEmutecaGroup;
  aSoftList: cEmutecaSoftList;
  aSoft: cEmutecaSoftware;
  i, j: integer;  
  aDev, aDate: string;
begin
  aSystem := ETKAskSystem(Emuteca.SystemManager.FullList);
   
  if not assigned(aSystem) then
  begin
    WriteLn('No system was selected.');
    Exit;
  end
  else
  begin
    WriteLn(aSystem.Title);
  end;

  // Loading data of the system if it isn't loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);
   
  aGroupList := aSystem.GroupManager.FullList;
  
  i := 0;
  while i < aGroupList.Count do
  begin
    aGroup := aGroupList[i];
    aSoftList := aGroup.SoftList;
    
    if (aGroup.Developer = '') or (aGroup.Date = '') then
    begin
      aDev := '';
      aDate := '';
    
      j := 0;
      while j < aSoftList.Count do
      begin
        aSoft := aSoftList[j];

        // Trying to extract developer from publisher
        if aDev = '' then 
          aDev := aSoft.Publisher
        else
          // If there are different publishers, don't change it
          if aDev <> aSoft.Publisher then
            aDev := '-';
      
        // Date
        if aDate = '' then 
          aDate := aSoft.Date
        else
        begin
          if aSoft.Date <> '' then
          begin
            if aDate > aSoft.Date then // '1999/02/03' > '1999' ...
              aDate := aSoft.Date;
          end;
        end;
            
        Inc(j);
      end; 

      if (aGroup.Developer = '') and (aDev <> '-') then 
        aGroup.Developer := aDev;      
      if (aGroup.Date = '') and (aDate <> '') then
        aGroup.Date := aDate; 
    end;    
    
    Inc(i);
  end;
  
  WriteLn('Finished!');    
end.
